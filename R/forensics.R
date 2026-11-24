# Fabrication and curbstoning screens for third-party monitoring data, and the sample-size arithmetic
# that tells whether a survey's clustering left it the precision it claims. Each screen returns the
# statistic and the rows or units it marks; a mark sends a record to call-back or back-check.

.dq_haversine_m <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180; a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371008.8 * asin(pmin(1, sqrt(a)))
}

# Completion speed: seconds per answered item, and characters typed per minute of interview. Mobile
# text entry averages about 36 words (some 180 characters) a minute (Palin et al. 2019, MobileHCI),
# so a form whose free text implies a much faster rate was not typed during the interview it records.
dq_speed <- function(duration_min, answered, text_chars = NULL, min_seconds_per_answer = 3, max_chars_per_minute = 300,
                     min_answers = 10L, min_chars = 50L) {
  spa <- ifelse(duration_min > 0, duration_min * 60 / pmax(answered, 1), NA_real_)
  out <- data.frame(seconds_per_answer = round(spa, 2),
    too_fast = !is.na(spa) & answered >= min_answers & spa < min_seconds_per_answer)
  if (!is.null(text_chars)) {
    cpm <- ifelse(duration_min > 0, text_chars / duration_min, NA_real_)
    out$chars_per_minute <- round(cpm, 1)
    out$typing_implausible <- !is.na(cpm) & text_chars >= min_chars & cpm > max_chars_per_minute
  }
  out
}

# Impossible travel: consecutive forms by one interviewer (or account) whose GPS positions imply a
# ground speed above `max_kmh`. Both forms of each offending pair are marked.
dq_travel <- function(who, time, latitude, longitude, max_kmh = 80, min_minutes = 1) {
  n <- length(who); flag <- rep(FALSE, n); speed <- rep(NA_real_, n)
  t <- .dq_temporal(time); ok <- !is.na(who) & !is.na(t) & is.finite(latitude) & is.finite(longitude)
  for (ix in split(which(ok), who[ok])) {
    if (length(ix) < 2) next
    ix <- ix[order(t[ix])]
    km <- .dq_haversine_m(latitude[ix[-length(ix)]], longitude[ix[-length(ix)]], latitude[ix[-1]], longitude[ix[-1]]) / 1000
    h <- pmax(as.numeric(difftime(t[ix[-1]], t[ix[-length(ix)]], units = "mins")), min_minutes) / 60
    v <- km / h; speed[ix[-1]] <- round(v, 1)
    bad <- which(v > max_kmh); flag[c(ix[bad], ix[bad + 1])] <- TRUE
  }
  data.frame(kmh_from_previous = speed, impossible_travel = flag)
}

# Batch entry: forms by one interviewer sharing a timestamp at `unit` resolution ("mins" or "secs").
# Groups of `min_group` or more forms with one stamp were entered or sent together, not interviewed apart.
dq_batch_times <- function(who, time, unit = c("mins", "secs"), min_group = 2L) {
  unit <- match.arg(unit); t <- .dq_temporal(time)
  stamp <- format(t, if (unit == "mins") "%Y-%m-%d %H:%M" else "%Y-%m-%d %H:%M:%S")
  k <- ifelse(is.na(who) | is.na(t), NA, paste(who, stamp, sep = "\r"))
  size <- stats::ave(seq_along(k), k, FUN = length); size[is.na(k)] <- NA
  data.frame(group_size = size, batch = !is.na(size) & size >= min_group)
}

# One-way ANOVA intraclass correlation (Donner 1986), with the adjusted mean cluster size for unequal clusters.
.dq_icc <- function(y, g) {
  ok <- !is.na(y) & !is.na(g); y <- as.numeric(y[ok]); g <- as.character(g[ok])
  m <- table(g); k <- length(m); N <- length(y)
  if (k < 2 || N <= k) return(list(icc = NA_real_, clusters = k, n = N, sizes = as.integer(m)))
  mu <- tapply(y, g, mean); msb <- sum(m * (mu[names(m)] - mean(y))^2) / (k - 1)
  msw <- sum((y - mu[g])^2) / (N - k); m0 <- (N - sum(m^2) / N) / (k - 1)
  icc <- if ((msb + (m0 - 1) * msw) > 0) (msb - msw) / (msb + (m0 - 1) * msw) else NA_real_
  list(icc = max(icc, 0), clusters = k, n = N, sizes = as.integer(m))
}

# Design effect, effective sample size and margin of error for a clustered sample. The design effect for
# unequal cluster sizes is 1 + ((cv^2 + 1) * mean_size - 1) * ICC (Eldridge, Ashby and Kerry 2006, Int J
# Epidemiol 35:1292). Margin is for a proportion at `p` (default the observed mean of a 0/1 outcome).
dq_design_effect <- function(y, cluster, p = NULL, confidence = 0.95, min_clusters = 5L) {
  r <- .dq_icc(y, cluster)
  multi <- sum(r$sizes > 1)
  if (r$clusters < 2 || multi < min_clusters)
    return(data.frame(n = r$n, clusters = r$clusters, clusters_with_repeats = multi, mean_size = NA_real_, cv_size = NA_real_,
      icc = NA_real_, deff = NA_real_, n_effective = NA_real_, margin = NA_real_))
  ms <- mean(r$sizes); cv <- if (length(r$sizes) > 1) stats::sd(r$sizes) / ms else 0
  deff <- 1 + ((cv^2 + 1) * ms - 1) * r$icc; neff <- r$n / deff
  if (is.null(p)) p <- mean(as.numeric(y), na.rm = TRUE)
  z <- stats::qnorm(1 - (1 - confidence) / 2)
  margin <- if (p >= 0 && p <= 1) z * sqrt(p * (1 - p) / neff) else NA_real_
  data.frame(n = r$n, clusters = r$clusters, clusters_with_repeats = multi, mean_size = round(ms, 2), cv_size = round(cv, 3),
    icc = round(r$icc, 4), deff = round(deff, 3), n_effective = round(neff, 1), margin = round(margin, 4))
}

# Interviewer effect (Kish 1962): the intraclass correlation among one interviewer's respondents and the
# variance inflation 1 + (mean workload - 1) * rho. A high rho on an item that should vary by respondent
# shows answers shaped by who asked, or who filled the form.
dq_interviewer_effect <- function(y, interviewer, min_interviewers = 5L) {
  r <- .dq_icc(y, interviewer)
  if (r$clusters < min_interviewers) return(data.frame(interviewers = r$clusters, n = r$n, rho = NA_real_, deff_interviewer = NA_real_))
  data.frame(interviewers = r$clusters, n = r$n, rho = round(r$icc, 4), deff_interviewer = round(1 + (mean(r$sizes) - 1) * r$icc, 3))
}

# Sample size for a proportion (Cochran 1977), inflated by a design effect and optionally corrected for a
# finite population.
dq_required_n <- function(margin, p = 0.5, confidence = 0.95, deff = 1, population = Inf) {
  z <- stats::qnorm(1 - (1 - confidence) / 2); n0 <- z^2 * p * (1 - p) / margin^2 * deff
  ceiling(if (is.finite(population)) n0 / (1 + (n0 - 1) / population) else n0)
}

# Combine screen results per unit (interviewer, account or firm). `signals` is a data frame with a `unit`
# column and logical columns, one per screen (NA where the screen could not run). Units are ranked by
# the number of screens failed, then by the share of available screens failed. Screens are not weighted.
dq_falsification_rank <- function(signals, unit = "unit") {
  tests <- setdiff(names(signals), unit)
  m <- as.matrix(signals[tests]); storage.mode(m) <- "logical"
  out <- data.frame(unit = signals[[unit]], screens_run = rowSums(!is.na(m)), screens_failed = rowSums(m, na.rm = TRUE), stringsAsFactors = FALSE)
  out$share_failed <- ifelse(out$screens_run > 0, round(out$screens_failed / out$screens_run, 3), NA_real_)
  out$failed <- apply(m, 1, function(r) paste(tests[r %in% TRUE], collapse = " "))
  out <- out[order(-out$screens_failed, -out$share_failed), , drop = FALSE]
  out$rank <- seq_len(nrow(out)); rownames(out) <- NULL; out
}
