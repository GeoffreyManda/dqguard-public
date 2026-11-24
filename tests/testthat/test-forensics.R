test_that("speed screens mark implausible completion and typing rates", {
  s <- dq_speed(duration_min = c(10, 1, 5, 0), answered = c(60, 60, 5, 60), text_chars = c(500, 100, 2000, 10))
  expect_equal(s$too_fast, c(FALSE, TRUE, FALSE, FALSE))
  expect_equal(s$typing_implausible, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("travel screen marks both forms of an impossible move", {
  t <- as.POSIXct("2026-01-01 08:00", tz = "UTC") + c(0, 30, 600, 0) * 60
  # Sana'a to Aden (about 300 km) in 30 minutes, then a slow return; a second interviewer with one form.
  tr <- dq_travel(c("e1", "e1", "e1", "e2"), t, c(15.35, 12.78, 15.35, 15.35), c(44.21, 45.04, 44.21, 44.21))
  expect_equal(tr$impossible_travel, c(TRUE, TRUE, FALSE, FALSE))
  expect_gt(tr$kmh_from_previous[2], 500)
})

test_that("batch screen groups forms by interviewer and minute", {
  t <- as.POSIXct(c("2026-01-01 08:00:05", "2026-01-01 08:00:40", "2026-01-01 08:01:00", "2026-01-01 08:00:10"), tz = "UTC")
  b <- dq_batch_times(c("e1", "e1", "e1", "e2"), t)
  expect_equal(b$batch, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("design effect matches the ANOVA ICC and the Eldridge formula", {
  set.seed(1)
  g <- rep(1:10, each = 20); u <- rnorm(10, 0, 1)[g]; y <- u + rnorm(200)
  de <- dq_design_effect(y, g, p = 0.5)
  mu <- tapply(y, g, mean); msb <- 20 * sum((mu - mean(y))^2) / 9; msw <- sum((y - mu[g])^2) / 190
  icc <- (msb - msw) / (msb + 19 * msw)
  expect_equal(de$icc, round(icc, 4))
  expect_equal(de$deff, round(1 + 19 * icc, 3))
  expect_equal(de$n_effective, round(200 / (1 + 19 * icc), 1))
  # Singleton clusters cannot estimate clustering.
  expect_true(is.na(dq_design_effect(1:20, 1:20)$deff))
})

test_that("required sample size reproduces Cochran's 385 and scales with the design effect", {
  expect_equal(dq_required_n(0.05), 385)
  expect_equal(dq_required_n(0.05, deff = 2), 769)
  expect_lt(dq_required_n(0.05, population = 1000), 385)
})

test_that("interviewer effect is near zero when answers do not depend on the interviewer", {
  set.seed(2)
  ie <- dq_interviewer_effect(rnorm(300), rep(1:15, each = 20))
  expect_lt(ie$rho, 0.05)
  ie2 <- dq_interviewer_effect(rep(1:15, each = 20) + rnorm(300, 0, 0.1), rep(1:15, each = 20))
  expect_gt(ie2$rho, 0.9)
})

test_that("falsification ranking counts failed screens without weighting", {
  sig <- data.frame(unit = c("e1", "e2", "e3"), speed = c(TRUE, FALSE, NA), travel = c(TRUE, FALSE, FALSE), benford = c(FALSE, TRUE, NA))
  r <- dq_falsification_rank(sig)
  expect_equal(r$unit, c("e1", "e2", "e3"))
  expect_equal(r$screens_failed, c(2, 1, 0)); expect_equal(r$screens_run, c(3, 3, 1))
  expect_equal(r$failed[1], "speed travel")
})

test_that("the forensic catalogue lists sourced items with a function or evidence route", {
  skip_if_not(nzchar(system.file("extdata", "forensics.csv", package = "dqguard")))
  f <- dq_catalog("forensics")
  expect_true(all(nzchar(f$source))); expect_false(anyDuplicated(f$item_id) > 0)
  fns <- setdiff(unique(f$`function`), c("project rule", "manual evidence"))
  expect_true(all(fns %in% getNamespaceExports("dqguard")))
})
