.dq_param_names <- function() list(
  not_missing = character(), type = "type", range = c("min", "max"),
  allowed = "values", regex = "pattern", unique = character(),
  no_whitespace = character(), no_sentinel = "values", finite = character(),
  integerish = "tolerance", no_blank_rows = character(), no_duplicate_rows = character(),
  row_count = c("min", "max"), foreign_key = c("reference", "reference_columns"),
  compare = c("other", "operator", "tolerance"),
  required_if = c("field", "values"), missing_if = c("field", "values"),
  mutually_exclusive = c("require_one"), multiselect = c("values", "separator", "exclusive"),
  sum_matches = c("total", "tolerance"), ratio = c("min", "max"),
  date_window = c("start", "end"), latency = c("min_seconds", "max_seconds"),
  future = "tolerance_seconds", freshness = "max_seconds",
  geo_bounds = c("lat_min", "lat_max", "lon_min", "lon_max", "exclude_zero"),
  distance = "max_metres", nondecreasing = c("group", "order_by"),
  no_summary_rows = "values")
.dq_check_types <- function() names(.dq_param_names())

.dq_validate_params <- function(type, columns, p) {
  if (length(p) && (is.null(names(p)) || any(!nzchar(names(p))) || anyNA(names(p)) || anyDuplicated(names(p))))
    .dq_abort("Every parameter must have a unique name.")
  extra <- setdiff(names(p), .dq_param_names()[[type]])
  if (length(extra)) .dq_abort(paste("Unknown parameters:", paste(extra, collapse = ", ")))
  required <- switch(type, type = "type", allowed = "values", regex = "pattern",
    no_sentinel = "values", foreign_key = c("reference", "reference_columns"),
    compare = c("other", "operator"), required_if = c("field", "values"),
    missing_if = c("field", "values"), multiselect = "values", sum_matches = "total",
    date_window = c("start", "end"), latency = "max_seconds", freshness = "max_seconds",
    distance = "max_metres", nondecreasing = "order_by", no_summary_rows = "values", character())
  if (length(setdiff(required, names(p))))
    .dq_abort(paste("Required parameters:", paste(required, collapse = ", ")))
  if (length(columns) == 0L && !type %in% c("row_count", "no_blank_rows", "no_duplicate_rows"))
    .dq_abort(paste(type, "requires columns."))
  if (type %in% c("compare", "regex", "multiselect", "date_window", "future", "freshness", "nondecreasing") && length(columns) != 1L)
    .dq_abort(paste(type, "requires exactly one column."))
  if (type %in% c("ratio", "latency", "geo_bounds") && length(columns) != 2L)
    .dq_abort(paste(type, "requires exactly two ordered columns."))
  if (type == "distance" && length(columns) != 4L) .dq_abort("distance requires lat, lon, reference_lat, reference_lon.")
  if (type == "type" && (!is.character(p$type) || length(p$type) != 1L || !p$type %in% .dq_types))
    .dq_abort("Unsupported type.")
  if (type %in% c("range", "ratio", "row_count") && !any(c("min", "max") %in% names(p)))
    .dq_abort("Specify at least one bound.")
  for (nm in intersect(c("min", "max", "min_seconds", "max_seconds", "max_metres", "tolerance", "tolerance_seconds",
                         "lat_min", "lat_max", "lon_min", "lon_max"), names(p))) {
    .dq_num1(p[[nm]], nm, finite = FALSE)
    if (nm %in% c("max_seconds", "max_metres", "tolerance", "tolerance_seconds") && p[[nm]] < 0)
      .dq_abort(paste(nm, "must be non-negative."))
  }
  if (!is.null(p$min) && !is.null(p$max) && p$min > p$max) .dq_abort("min exceeds max.")
  if (type == "row_count" && any(vapply(p, function(x) x < 0 || (is.finite(x) && x != floor(x)), logical(1))))
    .dq_abort("Row count bounds must be non-negative integers or Inf.")
  if (type == "latency" && (p$min_seconds %||% 0) > p$max_seconds) .dq_abort("Invalid latency interval.")
  for (nm in intersect(c("values", "exclusive"), names(p))) {
    if (!is.atomic(p[[nm]]) || anyNA(p[[nm]]) || (nm == "values" && !length(p[[nm]])))
      .dq_abort(paste(nm, "must be a non-missing atomic vector."))
  }
  for (nm in intersect(c("reference", "field", "other", "total", "order_by", "separator", "pattern", "start", "end"), names(p)))
    .dq_chr1(p[[nm]], nm)
  if (type == "regex") tryCatch(grepl(p$pattern, "", perl = TRUE),
                                error = function(e) .dq_abort("Invalid regular expression."))
  if (type == "compare" && !p$operator %in% c("==", "!=", "<", "<=", ">", ">=")) .dq_abort("Invalid comparison operator.")
  if (type == "foreign_key" && (!is.character(p$reference_columns) || length(columns) != length(p$reference_columns)))
    .dq_abort("Foreign key columns must have equal arity.")
  if (!is.null(p$require_one)) .dq_flag(p$require_one, "require_one")
  if (!is.null(p$exclude_zero)) .dq_flag(p$exclude_zero, "exclude_zero")
  invisible(TRUE)
}

.dq_all_columns <- function(data, columns, fun) {
  if (!length(columns)) columns <- names(data)
  out <- rep(TRUE, nrow(data))
  for (nm in columns) out <- out & fun(data[[nm]])
  out
}
.dq_require_numeric <- function(x) {
  if (!.dq_type(x, "numeric")) .dq_abort("Numeric check received a non-numeric column; no automatic coercion is performed.")
  invisible(TRUE)
}
.dq_temporal <- function(x) {
  if (!inherits(x, c("Date", "POSIXct"))) .dq_abort("Temporal checks require Date or POSIXct values, not strings or serial numbers.")
  if (inherits(x, "Date")) as.POSIXct(x, tz = "UTC") else x
}
.dq_eval_spec <- function(d, context, type, cols, p) {
  n <- nrow(d)
  if (!length(cols)) cols <- names(d)
  switch(type,
    not_missing = .dq_all_columns(d, cols, function(x) !.dq_missing(x)),
    type = rep(all(vapply(d[cols], .dq_type, logical(1), type = p$type)), n),
    range = .dq_all_columns(d, cols, function(x) {
      .dq_require_numeric(x); (is.finite(x) & x >= (p$min %||% -Inf) & x <= (p$max %||% Inf)) | ifelse(is.na(x), NA, FALSE)
    }),
    allowed = .dq_all_columns(d, cols, function(x) {
      ans <- x %in% p$values; ans[.dq_missing(x)] <- NA; ans
    }),
    regex = .dq_all_columns(d, cols, function(x) {
      if (!is.character(x) && !is.factor(x)) .dq_abort("Regex checks require text, not numeric ID coercion.")
      ans <- grepl(p$pattern, as.character(x), perl = TRUE); ans[.dq_missing(x)] <- NA; ans
    }),
    unique = {
      key <- .dq_key(d, cols)
      ans <- !(duplicated(key) | duplicated(key, fromLast = TRUE))
      missing <- !.dq_all_columns(d, cols, function(x) !.dq_missing(x)); ans[missing] <- NA; ans
    },
    no_whitespace = .dq_all_columns(d, cols, function(x) {
      if (!is.character(x) && !is.factor(x)) .dq_abort("Whitespace checks require text.")
      as.character(x) == trimws(as.character(x))
    }),
    no_sentinel = .dq_all_columns(d, cols, function(x) !(x %in% p$values)),
    finite = .dq_all_columns(d, cols, function(x) { .dq_require_numeric(x); is.finite(x) }),
    integerish = .dq_all_columns(d, cols, function(x) {
      .dq_require_numeric(x); ans <- is.finite(x) & abs(x - round(x)) <= (p$tolerance %||% 0)
      ans[is.na(x)] <- NA; ans
    }),
    no_blank_rows = !.dq_all_columns(d, cols, .dq_missing),
    no_duplicate_rows = !(duplicated(d[cols]) | duplicated(d[cols], fromLast = TRUE)),
    row_count = n >= (p$min %||% 0) && n <= (p$max %||% Inf),
    foreign_key = {
      ref <- context$references[[p$reference]]
      if (is.null(ref)) .dq_abort(paste("Reference unavailable:", p$reference))
      .dq_df(ref); .dq_cols(ref, p$reference_columns)
      .dq_key_compatible(d, cols, ref, p$reference_columns)
      refkey <- .dq_key(ref, p$reference_columns)
      if (anyDuplicated(refkey) || any(!.dq_all_columns(ref, p$reference_columns, function(x) !.dq_missing(x))))
        .dq_abort("Parent reference keys must be unique and complete.")
      ans <- .dq_key(d, cols) %in% refkey
      ans[!.dq_all_columns(d, cols, function(x) !.dq_missing(x))] <- NA; ans
    },
    compare = {
      .dq_cols(d, p$other); a <- d[[cols[1]]]; b <- d[[p$other]]
      if (!identical(class(a), class(b)) && !(is.numeric(a) && is.numeric(b) &&
            !inherits(a, c("Date", "POSIXt")) && !inherits(b, c("Date", "POSIXt"))))
        .dq_abort("Comparison column types differ.")
      tol <- p$tolerance %||% 0
      if (tol > 0 && !is.numeric(a)) .dq_abort("Tolerance only applies to numeric comparisons.")
      switch(p$operator, `==` = if (tol > 0) abs(a - b) <= tol else a == b,
        `!=` = if (tol > 0) abs(a - b) > tol else a != b, `<` = a < b, `<=` = a <= b,
        `>` = a > b, `>=` = a >= b)
    },
    required_if =, missing_if = {
      .dq_cols(d, p$field); trigger <- d[[p$field]] %in% p$values
      trigger[.dq_missing(d[[p$field]])] <- NA
      valid <- .dq_all_columns(d, cols, function(x) if (type == "required_if") !.dq_missing(x) else .dq_missing(x))
      !trigger | valid
    },
    mutually_exclusive = {
      ok_types <- vapply(d[cols], function(x) is.logical(x) || .dq_type(x, "numeric"), logical(1))
      if (!all(ok_types)) .dq_abort("Selection columns must be logical or binary numeric.")
      m <- as.matrix(d[cols])
      # %in% drops dimensions; restore them explicitly.
      binary <- matrix(m %in% c(0, 1), nrow = n, ncol = length(cols))
      count <- rowSums(m, na.rm = FALSE)
      ans <- rowSums(!binary) == 0 & if (isTRUE(p$require_one)) count == 1 else count <= 1
      ans[rowSums(is.na(m)) > 0] <- NA; ans
    },
    multiselect = {
      x <- d[[cols[1]]]
      if (!is.character(x)) .dq_abort("Multiselect check requires character encoding.")
      vapply(x, function(s) {
        if (is.na(s) || !nzchar(trimws(s))) return(NA)
        z <- strsplit(s, p$separator %||% " ", fixed = TRUE)[[1]]
        all(nzchar(z)) && !anyDuplicated(z) && all(z %in% as.character(p$values)) &&
          !(length(z) > 1 && any(z %in% as.character(p$exclusive %||% character())))
      }, logical(1))
    },
    sum_matches = {
      .dq_cols(d, p$total); lapply(d[c(cols, p$total)], .dq_require_numeric)
      abs(rowSums(as.matrix(d[cols]), na.rm = FALSE) - d[[p$total]]) <= (p$tolerance %||% 0)
    },
    ratio = {
      lapply(d[cols], .dq_require_numeric); a <- d[[cols[1]]]; b <- d[[cols[2]]]
      z <- a / b; ans <- is.finite(z) & z >= (p$min %||% -Inf) & z <= (p$max %||% Inf)
      ans[is.na(a) | is.na(b) | b == 0] <- NA; ans
    },
    date_window = {
      x <- .dq_temporal(d[[cols[1]]])
      start <- .dq_boundary(p$start); end <- .dq_boundary(p$end)
      if (is.na(start) || is.na(end) || start > end) .dq_abort("Invalid time window.")
      x >= start & x <= end
    },
    latency = {
      a <- .dq_temporal(d[[cols[1]]]); b <- .dq_temporal(d[[cols[2]]])
      lag <- as.numeric(difftime(b, a, units = "secs"))
      lag >= (p$min_seconds %||% 0) & lag <= p$max_seconds
    },
    future = .dq_temporal(d[[cols[1]]]) <= context$now + (p$tolerance_seconds %||% 0),
    freshness = {
      x <- .dq_temporal(d[[cols[1]]])
      if (!length(x) || all(is.na(x))) NA else {
        lag <- as.numeric(difftime(context$now, max(x, na.rm = TRUE), units = "secs"))
        lag >= 0 && lag <= p$max_seconds
      }
    },
    geo_bounds = {
      lapply(d[cols], .dq_require_numeric); lat <- d[[cols[1]]]; lon <- d[[cols[2]]]
      lo1 <- p$lat_min %||% -90; hi1 <- p$lat_max %||% 90
      lo2 <- p$lon_min %||% -180; hi2 <- p$lon_max %||% 180
      if (lo1 < -90 || hi1 > 90 || lo2 < -180 || hi2 > 180 || lo1 > hi1 || lo2 > hi2)
        .dq_abort("Invalid longitude/latitude bounding box.")
      ans <- is.finite(lat) & is.finite(lon) & lat >= lo1 & lat <= hi1 & lon >= lo2 & lon <= hi2
      if (isTRUE(p$exclude_zero)) ans <- ans & !(lat == 0 & lon == 0)
      ans[is.na(lat) | is.na(lon)] <- NA; ans
    },
    distance = {
      lapply(d[cols], .dq_require_numeric)
      lat1 <- d[[cols[1]]]; lon1 <- d[[cols[2]]]; lat2 <- d[[cols[3]]]; lon2 <- d[[cols[4]]]
      valid <- abs(lat1) <= 90 & abs(lat2) <= 90 & abs(lon1) <= 180 & abs(lon2) <= 180
      rad <- pi / 180
      a <- sin((lat2 - lat1) * rad / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
      metres <- 2 * 6371008.8 * atan2(sqrt(pmin(1, pmax(0, a))), sqrt(pmax(0, 1 - a)))
      valid & is.finite(metres) & metres <= p$max_metres
    },
    nondecreasing = {
      .dq_cols(d, c(p$group %||% character(), p$order_by)); x <- d[[cols[1]]]
      if (!is.numeric(x) && !inherits(x, c("Date", "POSIXt"))) .dq_abort("Sequence values must be numeric or temporal.")
      ans <- rep(TRUE, n)
      for (idx in split(seq_len(n), .dq_key(d, p$group %||% character()))) {
        ord <- d[[p$order_by]][idx]
        if (anyNA(ord) || anyDuplicated(ord)) { ans[idx] <- NA; next }
        ix <- idx[order(ord)]; if (length(ix) > 1) ans[ix[-1]] <- diff(x[ix]) >= 0
        ans[ix[is.na(x[ix])]] <- NA
      }
      ans
    },
    no_summary_rows = .dq_all_columns(d, cols, function(x) {
      if (!is.character(x) && !is.factor(x)) .dq_abort("Summary-label checks require text.")
      !(tolower(trimws(as.character(x))) %in% tolower(as.character(p$values)))
    }),
    .dq_abort("Unsupported check type.")
  )
}
