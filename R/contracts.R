# The dictionary is deliberately explicit: inference is profiling, not approval.
dq_dictionary <- function(fields) {
  .dq_df(fields)
  .dq_cols(fields, c("name", "type", "required"))
  if (!nrow(fields) || !is.character(fields$name) || anyNA(fields$name) ||
      any(!nzchar(fields$name)) || anyDuplicated(fields$name)) .dq_abort("Dictionary names must be unique and populated.")
  if (!is.character(fields$type) || anyNA(fields$type) || any(!fields$type %in% .dq_types)) .dq_abort("Dictionary contains unsupported types.")
  if (!is.logical(fields$required) || anyNA(fields$required)) .dq_abort("required must contain TRUE or FALSE.")
  defaults <- list(description = "", unit = "", owner = "unassigned", source = "project dictionary",
                   min = NA_real_, max = NA_real_, pattern = NA_character_, max_missing = NA_real_)
  for (nm in names(defaults)) if (!nm %in% names(fields)) fields[[nm]] <- rep(defaults[[nm]], nrow(fields))
  for (nm in c("description", "unit", "owner", "source"))
    if (!is.character(fields[[nm]]) || anyNA(fields[[nm]])) .dq_abort(paste(nm, "must be non-missing text."))
  if (any(!nzchar(fields$owner)) || any(!nzchar(fields$source))) .dq_abort("Dictionary owner and source must be populated.")
  if (!is.character(fields$pattern)) .dq_abort("Dictionary pattern must be character, with NA for unspecified patterns.")
  for (nm in c("min", "max", "max_missing")) if (!is.numeric(fields[[nm]])) .dq_abort(paste(nm, "must be numeric."))
  for (i in seq_len(nrow(fields))) {
    if (!is.na(fields$min[i]) && !is.na(fields$max[i]) && fields$min[i] > fields$max[i]) .dq_abort("Dictionary min exceeds max.")
    if (!is.na(fields$max_missing[i]) && (fields$max_missing[i] < 0 || fields$max_missing[i] > 1)) .dq_abort("max_missing must be between 0 and 1.")
    if (fields$required[i] && !is.na(fields$max_missing[i]) && fields$max_missing[i] != 0)
      .dq_abort("Required fields must have a zero missingness tolerance.")
    if ((!is.na(fields$min[i]) || !is.na(fields$max[i])) && !fields$type[i] %in% c("numeric", "integer", "double"))
      .dq_abort("Dictionary numeric bounds may only constrain numeric fields; use date_window for dates.")
  }
  for (nm in c("allowed", "sentinels")) {
    if (!nm %in% names(fields)) fields[[nm]] <- rep(list(NULL), nrow(fields))
    if (!is.list(fields[[nm]])) .dq_abort(paste(nm, "must be a list-column of explicit values."))
    for (z in fields[[nm]]) if (!is.null(z) && (!is.atomic(z) || anyNA(z))) .dq_abort(paste("Invalid", nm, "values."))
  }
  structure(fields, class = c("dq_dictionary", "data.frame"))
}

dq_contract <- function(dataset, version, dictionary, key, rules = list(),
                        grain, owner, purpose, allow_extra = FALSE, min_rows = 1L,
                        reference_versions = list()) {
  for (nm in c("dataset", "version", "grain", "owner", "purpose")) .dq_chr1(get(nm), nm)
  .dq_flag(allow_extra, "allow_extra"); .dq_num1(min_rows, "min_rows", 0)
  if (min_rows != floor(min_rows)) .dq_abort("min_rows must be an integer.")
  if (!inherits(dictionary, "dq_dictionary")) dictionary <- dq_dictionary(dictionary)
  if (!is.character(key) || !length(key) || anyNA(key) || anyDuplicated(key) || !all(key %in% dictionary$name))
    .dq_abort("key must name dictionary fields defining the declared grain.")
  if (!all(dictionary$required[match(key, dictionary$name)])) .dq_abort("Every key field must be required in the dictionary.")
  if (length(rules) && !inherits(rules, "dq_rules")) rules <- dq_rules(rules)
  if (!is.list(reference_versions) || (length(reference_versions) && (is.null(names(reference_versions)) || anyDuplicated(names(reference_versions)))))
    .dq_abort("reference_versions must be a named list.")
  for (z in reference_versions) .dq_chr1(z, "reference version")
  structure(list(dataset = dataset, version = version, dictionary = dictionary, key = key,
    rules = rules, grain = grain, owner = owner, purpose = purpose, allow_extra = allow_extra,
    min_rows = as.integer(min_rows), reference_versions = reference_versions), class = "dq_contract")
}

.dq_compile_contract <- function(contract) {
  dd <- contract$dictionary
  expected <- dd$name; allow_extra <- contract$allow_extra
  rules <- list(
    dq_rule("schema.columns", function(data, context) all(expected %in% names(data)) &&
      (allow_extra || all(names(data) %in% expected)), dimension = "structure", severity = "critical",
      action = "block", scope = "dataset", source = "data dictionary"),
    dq_spec("structure.row_count", "row_count", params = list(min = contract$min_rows),
      scope = "dataset", dimension = "coverage", severity = "critical", action = "block"),
    dq_spec("identity.complete", "not_missing", contract$key, dimension = "completeness", severity = "critical"),
    dq_spec("identity.unique", "unique", contract$key, dimension = "uniqueness", severity = "critical")
  )
  for (i in seq_len(nrow(dd))) {
    nm <- dd$name[i]; type <- dd$type[i]
    # local() creates an independent closure for every column.
    rules[[length(rules) + 1L]] <- local({
      field <- nm; expected_type <- type
      dq_rule(paste0("dictionary.", field, ".type"), function(data, context) .dq_type(data[[field]], expected_type),
        columns = field, dimension = "validity", action = "block", scope = "dataset", source = "data dictionary")
    })
    miss <- if (dd$required[i]) 0 else dd$max_missing[i]
    if (!is.na(miss)) rules[[length(rules) + 1L]] <- dq_spec(paste0("dictionary.", nm, ".missing"),
      "not_missing", nm, dimension = "completeness", max_fail_rate = miss,
      action = if (dd$required[i]) "quarantine" else "review")
    # Optional absence does not create a validity violation; missingness is separate.
    args <- list(columns = nm, na_policy = "exclude", allow_empty = !dd$required[i], source = dd$source[i])
    if (!is.na(dd$min[i]) || !is.na(dd$max[i])) {
      p <- list(); if (!is.na(dd$min[i])) p$min <- dd$min[i]; if (!is.na(dd$max[i])) p$max <- dd$max[i]
      rules[[length(rules) + 1L]] <- do.call(dq_spec, c(list(id = paste0("dictionary.", nm, ".range"), type = "range", params = p), args))
    }
    if (length(dd$allowed[[i]])) rules[[length(rules) + 1L]] <- do.call(dq_spec,
      c(list(id = paste0("dictionary.", nm, ".allowed"), type = "allowed", params = list(values = dd$allowed[[i]])), args))
    if (length(dd$sentinels[[i]])) rules[[length(rules) + 1L]] <- do.call(dq_spec,
      c(list(id = paste0("dictionary.", nm, ".sentinels"), type = "no_sentinel", params = list(values = dd$sentinels[[i]])), args))
    if (!is.na(dd$pattern[i]) && nzchar(dd$pattern[i])) rules[[length(rules) + 1L]] <- do.call(dq_spec,
      c(list(id = paste0("dictionary.", nm, ".pattern"), type = "regex", params = list(pattern = dd$pattern[i])), args))
  }
  dq_rules(c(rules, unclass(contract$rules)))
}

# JSON and YAML readers accept a finite, declarative schema; neither executes code.
dq_read_contract <- function(path) {
  .dq_chr1(path, "path")
  if (!file.exists(path) || dir.exists(path) || file.info(path)$size > 1024^2)
    .dq_abort("Contract must be an available regular file no larger than 1 MiB.")
  ext <- tolower(tools::file_ext(path))
  x <- if (ext %in% c("yaml", "yml")) {
    .dq_need("yaml"); yaml::read_yaml(path, eval.expr = FALSE)
  } else if (ext == "json") jsonlite::fromJSON(path, simplifyVector = FALSE) else
    .dq_abort("Contract files must be JSON or YAML.")
  permitted <- c("dataset", "version", "grain", "owner", "purpose", "key", "allow_extra", "min_rows", "fields", "rules", "reference_versions")
  if (!is.list(x) || any(!names(x) %in% permitted)) .dq_abort("Unknown contract fields.")
  required <- c("dataset", "version", "grain", "owner", "purpose", "key", "fields")
  if (length(setdiff(required, names(x)))) .dq_abort("Contract metadata is incomplete.")
  if (!is.list(x$fields) || !length(x$fields)) .dq_abort("fields must be a non-empty array.")
  allowed_fields <- c("name", "type", "required", "description", "unit", "owner", "source", "min", "max", "pattern", "max_missing", "allowed", "sentinels")
  for (f in x$fields) if (!is.list(f) || any(!names(f) %in% allowed_fields) || !all(c("name", "type", "required") %in% names(f)))
    .dq_abort("Dictionary field specification is invalid.")
  chr <- function(nm, default = "") vapply(x$fields, function(f) f[[nm]] %||% default, character(1))
  num <- function(nm) vapply(x$fields, function(f) {
    z <- f[[nm]] %||% NA_real_
    if (!is.numeric(z) || length(z) != 1L) .dq_abort("Numeric contract parameters must be numeric scalars, not strings.")
    as.numeric(z)
  }, numeric(1))
  dd <- data.frame(name = chr("name"), type = chr("type"),
    required = vapply(x$fields, `[[`, logical(1), "required"), description = chr("description"),
    unit = chr("unit"), owner = chr("owner", "unassigned"), source = chr("source", "project dictionary"),
    min = num("min"), max = num("max"), max_missing = num("max_missing"), pattern = chr("pattern", NA_character_),
    stringsAsFactors = FALSE)
  dd$allowed <- lapply(x$fields, function(f) unlist(f$allowed, use.names = FALSE))
  dd$sentinels <- lapply(x$fields, function(f) unlist(f$sentinels, use.names = FALSE))
  rule_fields <- c("id", "type", "columns", "params", "dimension", "severity", "action", "scope", "max_fail_rate", "max_fail_count", "na_policy", "owner", "rationale", "version", "source", "allow_empty")
  rules <- lapply(x$rules %||% list(), function(r) {
    if (!is.list(r) || any(!names(r) %in% rule_fields)) .dq_abort("Unknown rule fields or executable rule in contract.")
    r$columns <- unlist(r$columns %||% character(), use.names = FALSE)
    r$params <- lapply(r$params %||% list(), function(p) if (is.list(p)) unlist(p, use.names = FALSE) else p)
    if (is.null(r$scope) && r$type %in% c("row_count", "freshness")) r$scope <- "dataset"
    do.call(dq_spec, r)
  })
  dq_contract(x$dataset, x$version, dd, unlist(x$key, use.names = FALSE), rules,
    grain = x$grain, owner = x$owner, purpose = x$purpose,
    allow_extra = x$allow_extra %||% FALSE, min_rows = x$min_rows %||% 1L,
    reference_versions = x$reference_versions %||% list())
}

dq_write_contract <- function(contract, path, overwrite = FALSE) {
  if (!inherits(contract, "dq_contract")) .dq_abort("Expected a dq_contract.")
  if (any(vapply(contract$rules, function(r) is.null(r$specification) || !is.null(r$when), logical(1))))
    .dq_abort("Custom functions cannot be serialized as declarative contracts; retain them in version-controlled R.")
  dd <- contract$dictionary
  fields <- lapply(seq_len(nrow(dd)), function(i) {
    f <- lapply(dd, function(x) if (is.list(x)) x[[i]] else x[i])
    f <- f[!vapply(f, function(z) is.null(z) || !length(z) || (length(z) == 1L && is.atomic(z) && is.na(z)), logical(1))]
    for (nm in intersect(c("allowed", "sentinels"), names(f))) f[[nm]] <- as.list(f[[nm]])
    f
  })
  rules <- lapply(contract$rules, function(r) {
    out <- r[c("id", "columns", "dimension", "severity", "action", "scope", "max_fail_rate", "na_policy", "owner", "rationale", "version", "source", "allow_empty")]
    if (is.finite(r$max_fail_count)) out$max_fail_count <- r$max_fail_count
    out$columns <- as.list(r$columns); out$type <- r$specification$type; out$params <- r$specification$params
    for (nm in intersect(c("values", "exclusive", "reference_columns", "group"), names(out$params)))
      out$params[[nm]] <- as.list(out$params[[nm]])
    out
  })
  out <- contract[c("dataset", "version", "grain", "owner", "purpose", "allow_extra", "min_rows", "reference_versions")]
  out$key <- as.list(contract$key); out$fields <- fields; out$rules <- rules
  .dq_atomic_write(path, function(p) jsonlite::write_json(out, p, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), overwrite)
}
