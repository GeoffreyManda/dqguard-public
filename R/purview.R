# Rule engine following the Microsoft Purview Unified Catalog data quality model (Microsoft Learn,
# "Create data quality rules", "Data quality scores", "Data quality thresholds", "Data quality actions",
# "Data quality alerts", "Data quality error records"; pages updated July 2026). Rule types, null handling,
# score arithmetic, threshold bands, action severities and alert conditions follow those pages.
# Rule scores are passed / (passed + failed + miscast + empty) * 100; ignored rows are excluded.

.dq_pv_types <- c("freshness", "unique", "format", "type_match", "duplicate_rows", "empty", "lookup", "custom")
.dq_pv_dims <- c("accuracy", "completeness", "conformity", "consistency", "timeliness", "uniqueness")
.dq_pv_default_dim <- c(freshness = "timeliness", unique = "uniqueness", format = "conformity", type_match = "conformity",
                        duplicate_rows = "uniqueness", empty = "completeness", lookup = "consistency", custom = "accuracy")

.dq_rx_escape <- function(ch) gsub("([][{}()+*^$|.?\\\\])", "\\\\\\1", ch)

# Purview "like" patterns: _ is any one character, % is zero or more, \ escapes the next character.
.dq_like <- function(x, pattern) {
  ch <- strsplit(pattern, "")[[1]]; out <- character(); i <- 1L
  while (i <= length(ch)) {
    if (ch[i] == "\\" && i < length(ch)) { out <- c(out, .dq_rx_escape(ch[i + 1L])); i <- i + 2L; next }
    out <- c(out, if (ch[i] == "_") "." else if (ch[i] == "%") ".*" else .dq_rx_escape(ch[i])); i <- i + 1L
  }
  !is.na(x) & grepl(paste0("^", paste(out, collapse = ""), "$"), x, perl = TRUE)
}

# Enumerations are comma-separated; a backslash escapes a comma or a backslash.
.dq_enum <- function(values) {
  if (length(values) > 1L) return(values)
  ch <- strsplit(values, "")[[1]]; out <- character(); cur <- ""; i <- 1L
  while (i <= length(ch)) {
    if (ch[i] == "\\" && i < length(ch)) { cur <- paste0(cur, ch[i + 1L]); i <- i + 2L; next }
    if (ch[i] == ",") { out <- c(out, trimws(cur)); cur <- "" } else cur <- paste0(cur, ch[i])
    i <- i + 1L
  }
  c(out, trimws(cur))
}

.dq_castable <- function(x, type) {
  x <- trimws(as.character(x))
  one <- function(f) vapply(x, function(v) !is.na(tryCatch(f(v), error = function(e) NA)), TRUE, USE.NAMES = FALSE)
  switch(type,
    integer = grepl("^[+-]?[0-9]+$", x),
    double = !is.na(suppressWarnings(as.numeric(x))),
    boolean = tolower(x) %in% c("true", "false", "yes", "no", "1", "0"),
    date = one(function(v) as.Date(v, tryFormats = c("%Y-%m-%d", "%d/%m/%Y"))),
    timestamp = one(function(v) as.POSIXct(v, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M"))),
    string = rep(TRUE, length(x)),
    .dq_abort(paste0("Unknown data type '", type, "'; use integer, double, boolean, date, timestamp or string.")))
}

# One rule. `columns` names the column(s); `params` holds the type's settings:
#   freshness: max_age_hours, with context$last_modified (and optionally context$as_of) supplied at scan time;
#   format: exactly one of values (enumeration), like or regex; type_match: type; lookup: reference (vector);
#   custom: row, and optionally filter and null, each a function(data) returning a logical vector.
dq_pv_rule <- function(id, type, columns = character(), params = list(), name = id, dimension = NULL, asset = NULL) {
  .dq_chr1(id, "id"); type <- match.arg(type, .dq_pv_types)
  if (is.null(dimension)) dimension <- unname(.dq_pv_default_dim[type])
  dimension <- match.arg(dimension, .dq_pv_dims)
  if (type %in% c("unique", "format", "type_match", "empty", "lookup") && length(columns) != 1L)
    .dq_abort(paste0("Rule '", id, "': a ", type, " rule applies to exactly one column."))
  if (type == "duplicate_rows" && length(columns) < 2L) .dq_abort(paste0("Rule '", id, "': duplicate rows needs two or more columns."))
  if (type == "format" && sum(c("values", "like", "regex") %in% names(params)) != 1L)
    .dq_abort(paste0("Rule '", id, "': a format rule takes exactly one of values, like or regex."))
  if (type == "custom" && !is.function(params$row)) .dq_abort(paste0("Rule '", id, "': a custom rule needs a row function."))
  if (type == "freshness" && is.null(params$max_age_hours)) .dq_abort(paste0("Rule '", id, "': freshness needs max_age_hours."))
  structure(list(id = id, name = name, type = type, columns = columns, params = params, dimension = dimension, asset = asset), class = "dq_pv_rule")
}

# Evaluate rules on one asset. Returns rule scores and the failing rows (error records, at most
# `max_error_records` per rule; Purview keeps 100,000).
dq_pv_scan <- function(data, rules, asset = "asset", run_id = format(Sys.time(), "%Y%m%d%H%M%S"),
                       context = list(), max_error_records = 100000L) {
  .dq_df(data); if (inherits(rules, "dq_pv_rule")) rules <- list(rules)
  ids <- vapply(rules, `[[`, "", "id"); if (anyDuplicated(ids)) .dq_abort("Rule IDs must be unique within a scan.")
  if (sum(vapply(rules, function(r) r$type == "duplicate_rows", TRUE)) > 1L) .dq_abort("An asset takes at most one duplicate rows rule.")
  # Unique, format, type and lookup rules count empty values only on columns that also carry an empty/blank rule.
  empty_cols <- unlist(lapply(rules, function(r) if (r$type == "empty") r$columns))
  n <- nrow(data); scores <- list(); errors <- list()
  for (r in rules) {
    .dq_cols(data, r$columns); outcome <- rep("passed", n)
    x <- if (length(r$columns) == 1L) data[[r$columns]] else NULL
    miss <- if (!is.null(x)) .dq_missing(x) | (is.character(x) & !is.na(x) & !nzchar(trimws(x))) else rep(FALSE, n)
    if (r$type == "empty") outcome[miss] <- "empty"
    if (r$type %in% c("unique", "format", "type_match", "lookup")) {
      v <- as.character(x)
      ok <- switch(r$type,
        unique = !(duplicated(v) | duplicated(v, fromLast = TRUE)) | miss,
        format = if (!is.null(r$params$values)) v %in% .dq_enum(r$params$values) else if (!is.null(r$params$like)) .dq_like(v, r$params$like) else !is.na(v) & grepl(r$params$regex, v, perl = TRUE),
        type_match = .dq_castable(x, r$params$type),
        lookup = v %in% as.character(r$params$reference))
      outcome[!ok] <- if (r$type == "type_match") "miscast" else "failed"
      outcome[miss] <- if (r$columns %in% empty_cols) "empty" else "ignored"
    }
    if (r$type == "duplicate_rows") { k <- .dq_key(data, r$columns); outcome[duplicated(k) | duplicated(k, fromLast = TRUE)] <- "failed" }
    if (r$type == "custom") {
      flt <- if (is.function(r$params$filter)) r$params$filter(data) %in% TRUE else rep(TRUE, n)
      nul <- if (is.function(r$params$null)) r$params$null(data) %in% TRUE else rep(FALSE, n)
      row <- r$params$row(data); if (length(row) == 1L) row <- rep(row, n)
      outcome[!(row %in% TRUE)] <- "failed"; outcome[nul | !flt] <- "ignored"
    }
    if (r$type == "freshness") {
      lm <- context$last_modified; if (is.null(lm)) .dq_abort(paste0("Rule '", r$id, "': freshness needs context$last_modified."))
      as_of <- if (is.null(context$as_of)) Sys.time() else context$as_of
      outcome <- if (isTRUE(as.numeric(difftime(as_of, lm, units = "hours")) <= r$params$max_age_hours)) "passed" else "failed"
    }
    tab <- table(factor(outcome, c("passed", "failed", "miscast", "empty", "ignored")))
    den <- sum(tab[c("passed", "failed", "miscast", "empty")])
    a <- if (is.null(r$asset)) asset else r$asset
    scores[[length(scores) + 1L]] <- data.frame(run_id = run_id, asset = a, rule_id = r$id, rule_name = r$name, rule_type = r$type,
      dimension = r$dimension, column = paste(r$columns, collapse = ", "), passed = tab[["passed"]], failed = tab[["failed"]],
      miscast = tab[["miscast"]], empty = tab[["empty"]], ignored = tab[["ignored"]],
      score = if (den > 0) round(100 * tab[["passed"]] / den, 2) else NA_real_, stringsAsFactors = FALSE)
    bad <- which(outcome %in% c("failed", "miscast", "empty"))
    if (r$type != "freshness" && length(bad)) {
      bad <- head(bad, max_error_records)
      errors[[length(errors) + 1L]] <- data.frame(run_id = run_id, asset = a, rule_id = r$id, rule_name = r$name,
        rule_type = r$type, row = bad, outcome = outcome[bad],
        value = if (!is.null(x)) as.character(x[bad]) else .dq_key(data[bad, , drop = FALSE], if (length(r$columns)) r$columns else names(data)),
        stringsAsFactors = FALSE)
    }
  }
  list(scores = .dq_bind(scores), errors = .dq_bind(errors, data.frame(run_id = character(), asset = character(), rule_id = character(),
    rule_name = character(), rule_type = character(), row = integer(), outcome = character(), value = character())))
}

# Purview roll-up: a column scores the mean of its rules, an asset the mean of its columns (a rule on
# several columns or none is its own unit), and each higher level the mean of the level below.
# `levels` maps each asset to its parents in order, for example data.frame(asset, product, domain).
dq_pv_rollup <- function(scores, levels = NULL, by_dimension = FALSE) {
  s <- scores[!is.na(scores$score), , drop = FALSE]
  s$unit <- ifelse(nzchar(s$column) & !grepl(",", s$column), s$column, s$rule_id)
  dim_g <- if (by_dimension) "dimension" else character()
  col <- stats::aggregate(s["score"], s[c("asset", dim_g, "unit")], mean)
  out <- list(asset = stats::aggregate(col["score"], col[c("asset", dim_g)], mean))
  if (!is.null(levels)) {
    lv_names <- setdiff(names(levels), "asset"); prev <- "asset"; cur <- out$asset
    for (lv in lv_names) {
      map <- unique(levels[c(prev, lv)])
      cur <- merge(cur, map, by = prev)
      cur <- stats::aggregate(cur["score"], cur[c(lv, dim_g)], mean)
      out[[lv]] <- cur; prev <- lv
    }
  }
  lapply(out, function(d) { d$score <- round(d$score, 2); d })
}

# Threshold band of a score: the Purview defaults are low below 40, medium from 40 to below 80, high from 80.
dq_pv_band <- function(score, low = 40, high = 80) {
  as.character(cut(score, c(-Inf, low, high, Inf), right = FALSE, labels = c("low", "medium", "high")))
}

# Actions for units scoring below their threshold. Severity follows Purview: high below 25, medium below 50,
# low otherwise. `owner` and `recommendation` are values or functions of the action rows.
dq_pv_actions <- function(scores, threshold = 80, unit = "rule_id", owner = NULL, recommendation = NULL) {
  d <- scores[!is.na(scores$score) & scores$score < threshold, , drop = FALSE]
  if (!nrow(d)) return(data.frame(unit = character(), score = numeric(), severity = character(), status = character()))
  d$unit <- d[[unit]]
  d$severity <- ifelse(d$score < 25, "high", ifelse(d$score < 50, "medium", "low"))
  d$status <- "Active"
  if (!is.null(owner)) d$assigned_to <- if (is.function(owner)) owner(d) else owner
  if (!is.null(recommendation)) d$recommendation <- if (is.function(recommendation)) recommendation(d) else recommendation
  d[order(d$score), , drop = FALSE]
}

# Alerts from score history (columns unit, run, score; run values sort in time order): the latest score below
# `below`, or a fall of more than `drop` points from the previous run.
dq_pv_alerts <- function(history, below = 40, drop = 10) {
  empty <- data.frame(unit = character(), run = character(), score = numeric(), previous = numeric(), condition = character())
  if (!nrow(history)) return(empty)
  h <- history[order(history$unit, history$run), , drop = FALSE]
  out <- lapply(split(h, h$unit), function(d) {
    last <- d[nrow(d), ]; prev <- if (nrow(d) > 1) d$score[nrow(d) - 1] else NA_real_; a <- list()
    if (!is.na(last$score) && last$score < below)
      a[[1]] <- data.frame(unit = last$unit, run = as.character(last$run), score = last$score, previous = prev, condition = "score below threshold")
    if (!is.na(prev) && !is.na(last$score) && prev - last$score > drop)
      a[[length(a) + 1]] <- data.frame(unit = last$unit, run = as.character(last$run), score = last$score, previous = prev, condition = "score decreased")
    if (length(a)) do.call(rbind, a)
  })
  .dq_bind(out, empty)
}
