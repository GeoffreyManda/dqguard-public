# Survey-integrity statistics from published standards. Each returns the statistic, the band or
# test result it falls in, and the counts it rests on; none of them establishes fabrication alone.

# Whipple's index of age heaping on ages 23-62 (United Nations bands, Shryock and Siegel).
dq_whipple <- function(age, min_n = 20L) {
  a <- suppressWarnings(as.numeric(age)); a <- a[!is.na(a) & a >= 23 & a <= 62 & a == floor(a)]
  if (length(a) < min_n) return(list(index = NA_real_, band = NA_character_, n = length(a)))
  idx <- 500 * sum(a %% 5 == 0) / length(a)
  band <- as.character(cut(idx, c(-Inf, 105, 110, 125, 175, Inf), right = FALSE,
    labels = c("highly accurate", "fairly accurate", "approximate", "rough", "very rough")))
  list(index = idx, band = band, n = length(a))
}

# First-digit conformity to Benford's law by mean absolute deviation (Nigrini 2012 bands).
dq_benford <- function(x, min_n = 50L) {
  v <- suppressWarnings(as.numeric(x)); v <- abs(v[!is.na(v) & is.finite(v) & v != 0])
  if (length(v) < min_n) return(list(mad = NA_real_, band = NA_character_, n = length(v)))
  d <- floor(v / 10^floor(log10(v)))
  obs <- tabulate(d, 9) / length(d); mad <- mean(abs(obs - log10(1 + 1 / (1:9))))
  band <- as.character(cut(mad, c(-Inf, 0.006, 0.012, 0.015, Inf), right = TRUE,
    labels = c("close conformity", "acceptable conformity", "marginal conformity", "nonconformity")))
  list(mad = mad, band = band, n = length(v), observed = obs)
}

# Last-digit distribution of whole numbers against a reference set (chi-square test of homogeneity);
# without a reference the test is against a uniform last digit.
dq_last_digit <- function(x, reference = NULL, min_n = 50L) {
  w <- function(z) { z <- suppressWarnings(as.numeric(z)); z <- z[!is.na(z) & z == floor(z)]; abs(z) %% 10 }
  a <- w(x)
  if (length(a) < min_n) return(list(p_value = NA_real_, n = length(a)))
  if (is.null(reference)) return(list(p_value = suppressWarnings(stats::chisq.test(tabulate(a + 1, 10))$p.value), n = length(a)))
  b <- w(reference); if (length(b) < min_n) return(list(p_value = NA_real_, n = length(a)))
  tab <- rbind(tabulate(a + 1, 10), tabulate(b + 1, 10)) + 0.5
  list(p_value = suppressWarnings(stats::chisq.test(tab)$p.value), n = length(a), reference_n = length(b))
}

# Maximum percent match of each row with any other row over jointly answered fields
# (Kuriakose and Robbins 2016). Rows with fewer jointly answered fields than min_joint get NA.
dq_percent_match <- function(data, fields = names(data), min_joint = 10L) {
  .dq_df(data); .dq_cols(data, fields)
  m <- as.matrix(data[, fields, drop = FALSE]); m[] <- trimws(as.character(m)); m[m == ""] <- NA
  n <- nrow(m); share <- rep(NA_real_, n); twin <- rep(NA_integer_, n)
  if (n < 2) return(data.frame(row = seq_len(n), max_match = share, twin = twin))
  answered <- !is.na(m)
  for (i in seq_len(n)) {
    if (sum(answered[i, ]) < min_joint) next
    both <- answered & matrix(answered[i, ], n, ncol(m), byrow = TRUE)
    same <- both & (m == matrix(m[i, ], n, ncol(m), byrow = TRUE))
    jn <- rowSums(both); s <- rowSums(same, na.rm = TRUE) / pmax(jn, 1)
    s[i] <- NA; s[jn < min_joint] <- NA
    if (any(!is.na(s))) { twin[i] <- which.max(s); share[i] <- s[twin[i]] }
  }
  data.frame(row = seq_len(n), max_match = share, twin = twin)
}

# Standardised position of each unit's statistic among its peers (for example an enumerator among
# the same firm's enumerators). A z-score marks an outlier among peers, not an error.
dq_peer_z <- function(value, group = rep("all", length(value))) {
  m <- stats::ave(value, group, FUN = function(v) mean(v, na.rm = TRUE))
  s <- stats::ave(value, group, FUN = function(v) if (sum(!is.na(v)) > 1) stats::sd(v, na.rm = TRUE) else NA_real_)
  ifelse(!is.na(s) & s > 0, (value - m) / s, NA_real_)
}

# Threshold sensitivity: evaluate a flagging function over a grid of threshold values and report how
# many units it flags at each, and how many are flagged at every value (robust flags).
dq_sensitivity <- function(flag_fun, grid, ids = NULL) {
  if (!is.function(flag_fun)) .dq_abort("flag_fun must be a function of one threshold value.")
  res <- lapply(grid, function(g) { f <- flag_fun(g); if (!is.logical(f)) .dq_abort("flag_fun must return a logical vector."); f })
  n <- unique(lengths(res)); if (length(n) != 1L) .dq_abort("flag_fun must return vectors of one length.")
  mat <- do.call(cbind, res); if (is.null(ids)) ids <- seq_len(n)
  list(curve = data.frame(threshold = grid, flagged = colSums(mat, na.rm = TRUE), share = colMeans(mat, na.rm = TRUE)),
       robust = ids[rowSums(mat, na.rm = TRUE) == length(grid)], any = ids[rowSums(mat, na.rm = TRUE) > 0])
}
