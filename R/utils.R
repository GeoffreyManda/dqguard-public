# Internal utilities. Inputs and raw data are never modified in place.
`%||%` <- function(x, y) if (is.null(x)) y else x

.dq_abort <- function(message, class = "dq_error") {
  stop(structure(list(message = message, call = NULL),
                 class = c(class, "error", "condition")))
}
.dq_need <- function(package) {
  if (!requireNamespace(package, quietly = TRUE))
    .dq_abort(paste0("Install optional package '", package, "' for this operation."),
              "dq_dependency_error")
}
.dq_chr1 <- function(x, name, empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || (!empty && !nzchar(x)))
    .dq_abort(paste(name, "must be one non-missing character value."))
  invisible(x)
}
.dq_num1 <- function(x, name, min = -Inf, max = Inf, finite = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      (finite && !is.finite(x)) || x < min || x > max)
    .dq_abort(paste(name, "is outside its permitted numeric range."))
  invisible(x)
}
.dq_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    .dq_abort(paste(name, "must be TRUE or FALSE."))
  invisible(x)
}
.dq_cols <- function(data, columns) {
  if (!is.character(columns) || anyNA(columns) || any(!nzchar(columns)) ||
      anyDuplicated(columns)) .dq_abort("columns must contain unique, non-empty names.")
  absent <- setdiff(columns, names(data))
  if (length(absent)) .dq_abort(paste("Missing columns:", paste(absent, collapse = ", ")),
                               "dq_missing_columns")
  invisible(TRUE)
}
.dq_df <- function(x) {
  if (!is.data.frame(x)) .dq_abort("data must be a local data.frame or tibble.")
  if (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x)))
    .dq_abort("Data columns must have unique, non-empty names; inspect the raw file first.")
  invisible(TRUE)
}
.dq_empty_findings <- function() data.frame(
  run_id = character(), dataset = character(), rule_id = character(),
  rule_version = character(), dimension = character(), severity = character(),
  row = integer(), record_id = character(), finding_id = character(),
  fields = character(), code = character(), message = character(),
  action = character(), stringsAsFactors = FALSE)
.dq_bind <- function(x, empty = data.frame()) {
  x <- Filter(function(y) is.data.frame(y) && nrow(y) > 0L, x)
  if (!length(x)) return(empty)
  out <- do.call(rbind, x); rownames(out) <- NULL; out
}
.dq_time <- function(x = Sys.time()) {
  if (!inherits(x, "POSIXt") || length(x) != 1L || is.na(x))
    .dq_abort("A single POSIXct/POSIXlt time is required.")
  format(as.POSIXct(x), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}
.dq_parse_time <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
.dq_hash <- function(x) digest::digest(x, algo = "sha256", serialize = TRUE,
                                     serializeVersion = 2)
.dq_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE,
                       dataframe = "rows", null = "null", na = "null",
                       digits = 16, POSIXt = "ISO8601", Date = "ISO8601"))
.dq_missing <- function(x) {
  if (is.list(x) && !inherits(x, "POSIXlt"))
    return(vapply(x, function(z) length(z) == 0L ||
                    (length(z) == 1L && is.atomic(z) && is.na(z)), logical(1)))
  is.na(x) | if (is.character(x) || is.factor(x))
    trimws(as.character(x)) == "" else FALSE
}
.dq_token <- function(x) {
  if (inherits(x, "POSIXt")) x <- paste0("UTC:", sprintf("%.17g", as.numeric(x)))
  if (is.factor(x)) x <- as.character(x)
  if (is.list(x)) .dq_abort("List columns cannot be used in keys.")
  z <- if (is.numeric(x) && !inherits(x, c("Date", "difftime", "integer64"))) {
    v <- sprintf("%.17g", x); v[!is.na(x) & x == 0] <- "0"; v
  } else as.character(x)
  z <- enc2utf8(z); z[is.na(x)] <- ""
  ifelse(is.na(x), "N;", paste0(nchar(z, type = "bytes"), ":", z, ";"))
}
# Length-prefixing avoids composite key delimiter collisions.
.dq_key <- function(data, columns) {
  .dq_cols(data, columns)
  if (!length(columns)) return(rep("", nrow(data)))
  do.call(paste0, lapply(data[columns], .dq_token))
}
.dq_type <- function(x, type) switch(type,
  character = is.character(x), numeric = is.numeric(x) && !inherits(x, c("Date", "POSIXt", "difftime", "integer64")),
  double = is.double(x) && !inherits(x, c("Date", "POSIXt", "difftime", "integer64")),
  integer = is.integer(x) && !is.factor(x), logical = is.logical(x),
  date = inherits(x, "Date"), datetime = inherits(x, "POSIXct"),
  factor = is.factor(x), list = is.list(x) && !inherits(x, "POSIXt"), FALSE)
.dq_types <- c("character", "numeric", "double", "integer", "logical", "date", "datetime", "factor", "list")
.dq_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
.dq_atomic_write <- function(path, writer, overwrite = FALSE) {
  .dq_chr1(path, "path"); .dq_flag(overwrite, "overwrite")
  dir <- dirname(path)
  if (!dir.exists(dir)) .dq_abort("Output parent directory does not exist.")
  if (file.exists(path) && !overwrite) .dq_abort("Refusing to overwrite an existing file.")
  tmp <- tempfile(".dqguard-", tmpdir = dir)
  on.exit(unlink(tmp), add = TRUE)
  writer(tmp)
  if (!overwrite) {
    # A no-replace hard link prevents a writer racing the initial existence check.
    # Fail closed on filesystems without hard-link support; do not fall back to rename.
    committed <- suppressWarnings(file.link(tmp, path))
    if (!isTRUE(committed)) .dq_abort("Could not commit without replacement: destination exists or filesystem lacks hard-link support.")
  } else {
    if (dir.exists(path)) .dq_abort("Output path is a directory.")
    # Explicit replacement is not a crash-atomic or concurrent-writer guarantee.
    if (!file.copy(tmp, path, overwrite = TRUE)) .dq_abort("Could not replace output.")
  }
  invisible(path)
}
.dq_csv_safe <- function(x) {
  # Spreadsheet formula-injection protection applies to exported strings only.
  for (nm in names(x)) if (is.character(x[[nm]])) {
    bad <- !is.na(x[[nm]]) & grepl("^[[:space:]]*[=+@-]", x[[nm]])
    x[[nm]][bad] <- paste0("'", x[[nm]][bad])
  }
  x
}

.dq_key_compatible <- function(a, ac, b, bc) {
  .dq_cols(a, ac); .dq_cols(b, bc)
  if (length(ac) != length(bc)) .dq_abort("Key arities differ.")
  for (i in seq_along(ac)) {
    x <- a[[ac[i]]]; y <- b[[bc[i]]]
    if (!(identical(class(x), class(y)) || (.dq_type(x, "numeric") && .dq_type(y, "numeric"))))
      .dq_abort("Key storage types differ; normalize explicitly before comparison.")
  }
  invisible(TRUE)
}
.dq_boundary <- function(x) {
  .dq_chr1(x, "time boundary")
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) {
    z <- as.Date(x, format = "%Y-%m-%d")
    if (is.na(z) || format(z, "%Y-%m-%d") != x) .dq_abort("Invalid calendar date.")
    return(as.POSIXct(z, tz = "UTC"))
  }
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$", x))
    .dq_abort("Use YYYY-MM-DD or an explicit UTC ISO8601 timestamp ending Z.")
  z <- .dq_parse_time(x)
  if (is.na(z) || format(z, "%Y-%m-%dT%H:%M:%S", tz = "UTC") != substr(x, 1, 19))
    .dq_abort("Invalid UTC timestamp.")
  z
}
