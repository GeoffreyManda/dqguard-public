dq_profile <- function(data, by = character()) {
  .dq_df(data); .dq_cols(data, by)
  groups <- if (length(by)) split(seq_len(nrow(data)), .dq_key(data, by)) else list(all = seq_len(nrow(data)))
  rows <- list()
  for (g in names(groups)) for (nm in names(data)) {
    ix <- groups[[g]]; x <- data[[nm]][ix]; missing <- .dq_missing(x)
    is_num <- .dq_type(x, "numeric"); finite <- if (is_num) x[is.finite(x)] else numeric()
    rows[[length(rows) + 1L]] <- data.frame(group = g, field = nm, storage_class = paste(class(x), collapse = "/"),
      n = length(x), n_missing = sum(missing), missing_rate = if (length(x)) mean(missing) else NA_real_,
      n_unique = length(unique(x[!missing])), n_nonfinite = if (is_num) sum(!is.finite(x) & !is.na(x)) else NA_integer_,
      minimum = if (length(finite)) min(finite) else NA_real_, maximum = if (length(finite)) max(finite) else NA_real_,
      median = if (length(finite)) stats::median(finite) else NA_real_,
      mad = if (length(finite)) stats::mad(finite) else NA_real_,
      whitespace = if (is.character(x)) sum(!is.na(x) & x != trimws(x)) else NA_integer_,
      stringsAsFactors = FALSE)
  }
  .dq_bind(rows)
}

dq_missingness <- function(data, fields = names(data), by = character(), applicability = list()) {
  .dq_df(data); .dq_cols(data, c(fields, setdiff(by, fields)))
  groups <- if (length(by)) split(seq_len(nrow(data)), .dq_key(data, by)) else list(all = seq_len(nrow(data)))
  rows <- list(); bits <- matrix("0", nrow(data), length(fields))
  for (k in seq_along(fields)) {
    nm <- fields[k]; missing <- .dq_missing(data[[nm]])
    a <- applicability[[nm]] %||% rep(TRUE, nrow(data))
    if (!is.logical(a) || length(a) != nrow(data) || !is.null(dim(a))) .dq_abort("Applicability must be a logical vector per field.")
    bits[, k] <- ifelse(is.na(a), "?", ifelse(!a, "-", ifelse(missing, "1", "0")))
    for (g in names(groups)) {
      ix <- groups[[g]]; eligible <- ix[a[ix] %in% TRUE]
      rows[[length(rows) + 1L]] <- data.frame(group = g, field = nm, n_rows = length(ix),
        n_applicable = length(eligible), n_unknown_applicability = sum(is.na(a[ix])),
        n_missing = sum(missing[eligible]),
        missing_rate = if (length(eligible)) mean(missing[eligible]) else NA_real_, stringsAsFactors = FALSE)
    }
  }
  pattern <- if (nrow(data)) apply(bits, 1, paste0, collapse = "") else character()
  list(rates = .dq_bind(rows), patterns = as.data.frame(table(pattern), stringsAsFactors = FALSE),
    fields = fields, interpretation = "1=missing; 0=observed; -=not applicable; ?=unknown applicability. Patterns do not establish MCAR, MAR or MNAR.")
}

dq_drift <- function(current, baseline, fields = intersect(names(current), names(baseline)),
                     min_n = 30L, ks_threshold = NULL, tv_threshold = NULL) {
  .dq_df(current); .dq_df(baseline); .dq_cols(current, fields); .dq_cols(baseline, fields)
  .dq_num1(min_n, "min_n", 2)
  if (!is.null(ks_threshold)) .dq_num1(ks_threshold, "ks_threshold", 0, 1)
  if (!is.null(tv_threshold)) .dq_num1(tv_threshold, "tv_threshold", 0, 1)
  rows <- lapply(fields, function(nm) {
    a <- current[[nm]]; b <- baseline[[nm]]; ma <- .dq_missing(a); mb <- .dq_missing(b)
    na <- sum(!ma); nb <- sum(!mb); kind <- "categorical"; stat <- NA_real_; shift <- NA_real_
    status <- "descriptive"; reason <- "Compare equivalent populations and periods."
    if (!identical(class(a), class(b))) { status <- "fail"; reason <- "Storage class changed." }
    else if (na < min_n || nb < min_n) { status <- "not_evaluated"; reason <- "Insufficient non-missing observations." }
    else if (.dq_type(a, "numeric")) {
      kind <- "numeric"; aa <- a[is.finite(a)]; bb <- b[is.finite(b)]
      if (length(aa) < min_n || length(bb) < min_n) { status <- "not_evaluated"; reason <- "Insufficient finite values." }
      else {
        support <- sort(unique(c(aa, bb)))
        stat <- max(abs(stats::ecdf(aa)(support) - stats::ecdf(bb)(support)))
        sd_pool <- sqrt((stats::var(aa) + stats::var(bb)) / 2)
        delta <- mean(aa) - mean(bb)
        shift <- if (sd_pool == 0) if (delta == 0) 0 else sign(delta) * Inf else delta / sd_pool
        if (!is.null(ks_threshold)) status <- if (stat > ks_threshold) "needs_review" else "within_tolerance"
      }
    } else {
      aa <- .dq_token(a[!ma]); bb <- .dq_token(b[!mb]); support <- union(aa, bb)
      pa <- tabulate(match(aa, support), nbins = length(support)) / length(aa)
      pb <- tabulate(match(bb, support), nbins = length(support)) / length(bb)
      stat <- sum(abs(pa - pb)) / 2
      if (!is.null(tv_threshold)) status <- if (stat > tv_threshold) "needs_review" else "within_tolerance"
    }
    data.frame(field = nm, kind = kind, n_current = na, n_baseline = nb,
      missing_rate_delta = if (length(a) && length(b)) mean(ma) - mean(mb) else NA_real_,
      statistic = if (kind == "numeric") "KS distance" else "total variation distance", value = stat,
      standardized_mean_shift = shift, status = status, interpretation = reason, stringsAsFactors = FALSE)
  })
  .dq_bind(rows)
}

dq_reconcile <- function(current, reference, by, value, tolerance_abs = 0, tolerance_rel = 0) {
  .dq_df(current); .dq_df(reference); .dq_cols(current, c(by, value)); .dq_cols(reference, c(by, value))
  .dq_num1(tolerance_abs, "tolerance_abs", 0); .dq_num1(tolerance_rel, "tolerance_rel", 0)
  .dq_key_compatible(current, by, reference, by)
  .dq_require_numeric(current[[value]]); .dq_require_numeric(reference[[value]])
  aggregate_one <- function(d) {
    ix <- split(seq_len(nrow(d)), .dq_key(d, by))
    lapply(ix, function(i) list(total = if (anyNA(d[[value]][i])) NA_real_ else sum(d[[value]][i]),
      n = length(i), valid_group = all(.dq_all_columns(d[i, , drop = FALSE], by, function(x) !.dq_missing(x)))))
  }
  a <- aggregate_one(current); b <- aggregate_one(reference)
  .dq_bind(lapply(union(names(a), names(b)), function(g) {
    aa <- a[[g]]; bb <- b[[g]]; av <- aa$total %||% NA_real_; bv <- bb$total %||% NA_real_
    limit <- tolerance_abs + tolerance_rel * abs(bv)
    status <- if (is.null(aa) || is.null(bb)) "fail" else if (!isTRUE(aa$valid_group) || !isTRUE(bb$valid_group) ||
      !is.finite(av) || !is.finite(bv)) "not_evaluated" else if (abs(av - bv) <= limit) "pass" else "fail"
    data.frame(group = g, current_total = av, reference_total = bv, difference = av - bv,
      tolerance = limit, n_current = aa$n %||% 0L, n_reference = bb$n %||% 0L, status = status,
      reason = if (is.null(aa)) "missing_current_group" else if (is.null(bb)) "missing_reference_group" else
        if (status == "not_evaluated") "unknown_total_or_group" else "aggregate_comparison", stringsAsFactors = FALSE)
  }))
}

dq_coverage <- function(observed, frame, key) {
  .dq_df(observed); .dq_df(frame); .dq_cols(observed, key); .dq_cols(frame, key)
  .dq_key_compatible(observed, key, frame, key)
  fk <- .dq_key(frame, key); ok <- .dq_key(observed, key)
  fm <- !.dq_all_columns(frame, key, function(x) !.dq_missing(x))
  om <- !.dq_all_columns(observed, key, function(x) !.dq_missing(x))
  if (anyDuplicated(fk) || any(fm)) .dq_abort("Sampling frame must have complete unique keys.")
  known <- unique(ok[!om & ok %in% fk])
  list(metrics = data.frame(n_frame = nrow(frame), n_observed = nrow(observed),
    n_frame_units_observed = length(known), n_unobserved_frame_units = length(setdiff(fk, known)),
    n_out_of_frame = sum(!om & !ok %in% fk), n_missing_keys = sum(om),
    coverage = if (nrow(frame)) length(known) / nrow(frame) else NA_real_),
    missing_frame_rows = which(!fk %in% known), out_of_frame_rows = which(!om & !ok %in% fk),
    interpretation = "Coverage is descriptive. Expected sampling fractions, eligibility and inclusion probabilities must be supplied separately.")
}

dq_survey_signals <- function(data, enumerator, start, end, min_duration_seconds,
                              submitted = NULL, response_fields = character()) {
  .dq_df(data); .dq_cols(data, unique(c(enumerator, start, end, submitted, response_fields)))
  .dq_num1(min_duration_seconds, "min_duration_seconds", 0)
  a <- .dq_temporal(data[[start]]); b <- .dq_temporal(data[[end]])
  dur <- as.numeric(difftime(b, a, units = "secs")); rows <- list()
  add <- function(ix, code, severity = "warning") {
    if (length(ix)) rows[[length(rows) + 1L]] <<- data.frame(row = ix, code = code,
      severity = severity, interpretation = if (severity == "error") "Logical violation" else "Review signal, not evidence of fabrication", stringsAsFactors = FALSE)
  }
  add(which(is.na(a) | is.na(b) | .dq_missing(data[[enumerator]])), "missing_paradata", "error")
  add(which(dur < 0), "negative_duration", "error")
  add(which(dur >= 0 & dur < min_duration_seconds), "short_duration")
  for (ix in split(seq_len(nrow(data)), .dq_key(data, enumerator))) {
    ix <- ix[!is.na(a[ix]) & !is.na(b[ix]) & dur[ix] >= 0]
    ix <- ix[order(a[ix])]
    if (length(ix) > 1L) {
      preceding_end <- cummax(as.numeric(b[ix]))[-length(ix)]
      add(ix[-1L][as.numeric(a[ix[-1L]]) < preceding_end], "overlapping_interviews")
    }
  }
  if (!is.null(submitted)) {
    .dq_temporal(data[[submitted]])
    k <- .dq_key(data, c(enumerator, submitted)); repeat_rows <- duplicated(k) | duplicated(k, fromLast = TRUE)
    add(which(repeat_rows & !.dq_missing(data[[submitted]])), "repeated_submission_time")
  }
  if (length(response_fields)) {
    k <- .dq_key(data, response_fields); repeat_rows <- duplicated(k) | duplicated(k, fromLast = TRUE)
    add(which(repeat_rows), "identical_response_pattern")
  }
  .dq_bind(rows, data.frame(row = integer(), code = character(), severity = character(), interpretation = character()))
}

dq_geofence <- function(data, latitude, longitude, polygons) {
  .dq_need("sf"); .dq_df(data); .dq_cols(data, c(latitude, longitude))
  if (!inherits(polygons, c("sf", "sfc")) || is.na(sf::st_crs(polygons))) .dq_abort("Polygons must have a declared CRS.")
  if (!length(sf::st_geometry(polygons)) || !all(sf::st_is_valid(polygons))) .dq_abort("Boundary geometry is empty or invalid.")
  if (!all(as.character(sf::st_geometry_type(polygons)) %in% c("POLYGON", "MULTIPOLYGON"))) .dq_abort("Geofences must be polygon geometries.")
  .dq_require_numeric(data[[latitude]]); .dq_require_numeric(data[[longitude]])
  lat <- data[[latitude]]; lon <- data[[longitude]]
  good <- is.finite(lat) & is.finite(lon) & abs(lat) <= 90 & abs(lon) <= 180
  result <- rep(FALSE, nrow(data)); result[is.na(lat) | is.na(lon)] <- NA
  if (any(good)) {
    pts <- sf::st_as_sf(data[good, c(longitude, latitude), drop = FALSE], coords = c(longitude, latitude), crs = 4326)
    pts <- sf::st_transform(pts, sf::st_crs(polygons))
    result[good] <- lengths(sf::st_intersects(pts, polygons)) > 0L
  }
  result
}
