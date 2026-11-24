test_that("range validity and placeholders are contract specific", {
  d <- data.frame(x = c(-1, 0, 1, NA, Inf))
  r <- dq_run(d, dq_spec("bounds", "range", "x", list(min = 0, max = 1)))
  expect_equal(sort(r$findings$row), c(1L, 4L, 5L))
  r <- dq_run(d, dq_spec("sentinel", "no_sentinel", "x", list(values = -1), na_policy = "exclude"))
  expect_true(1L %in% r$findings$row)
  expect_false(2L %in% r$findings$row)
})
test_that("categorical regex whitespace and blank row checks work independently", {
  d <- data.frame(id = c("A01", " A02", "", NA), value = c("Y", "N", "", NA))
  r <- dq_run(d, dq_rules(dq_spec("regex", "regex", "id", list(pattern = "^A[0-9]{2}$")),
    dq_spec("enum", "allowed", "value", list(values = c("Y", "N"))),
    dq_spec("trim", "no_whitespace", "id"), dq_spec("blank", "no_blank_rows")))
  expect_equal(r$findings$row[r$findings$rule_id == "blank"], c(3L, 4L))
  expect_true(2L %in% r$findings$row[r$findings$rule_id == "trim"])
})
test_that("composite keys do not collide on concatenation delimiters", {
  d <- data.frame(a = c("ab", "a", "a|b", "a"), b = c("c", "bc", "c", "b|c"))
  r <- dq_run(d, dq_spec("key", "unique", c("a", "b")))
  expect_identical(r$metrics$status, "pass")
})
test_that("foreign keys reject absent ambiguous or wrong-type reference keys", {
  d <- data.frame(facility = c("a", "z"))
  rule <- dq_spec("fk", "foreign_key", "facility", list(reference = "fac", reference_columns = "id"))
  good <- dq_run(d, rule, context = list(references = list(fac = data.frame(id = "a"))))
  expect_equal(good$findings$row, 2L)
  absent <- dq_run(d, rule); expect_identical(absent$metrics$status, "error")
  duplicate <- dq_run(d, rule, context = list(references = list(fac = data.frame(id = c("a", "a")))))
  expect_identical(duplicate$metrics$status, "error")
  typed <- dq_run(d, rule, context = list(references = list(fac = data.frame(id = 1))))
  expect_identical(typed$metrics$status, "error")
})
test_that("skip logic and mutually exclusive responses are checked", {
  d <- data.frame(status = c("yes", "no", "yes"), date = as.Date(c("2026-01-01", NA, NA)), a = c(1, 1, 0), b = c(0, 1, 0))
  r <- dq_run(d, dq_rules(dq_spec("skip", "required_if", "date", list(field = "status", values = "yes")),
    dq_spec("exclusive", "mutually_exclusive", c("a", "b"))))
  expect_equal(r$findings$row[r$findings$rule_id == "skip"], 3L)
  expect_equal(r$findings$row[r$findings$rule_id == "exclusive"], 2L)
})
test_that("multiselect encoding disallows repeated and incompatible options", {
  d <- data.frame(v = c("a b", "a a", "none a", "z", "none"))
  r <- dq_run(d, dq_spec("multi", "multiselect", "v", list(values = c("a", "b", "none"), exclusive = "none")))
  expect_equal(r$findings$row, c(2L, 3L, 4L))
})
test_that("zero denominators never become acceptable ratios", {
  d <- data.frame(a = c(1, 0, 4), b = c(2, 0, 2))
  r <- dq_run(d, dq_spec("ratio", "ratio", c("a", "b"), list(min = 0, max = 1)))
  expect_equal(r$findings$row, c(2L, 3L))
  expect_equal(r$metrics$n_unknown, 1L)
})
test_that("arithmetic and temporal checks preserve exact units and time zones", {
  d <- data.frame(a = c(1, 2), b = c(2, 3), total = c(3, 4))
  r <- dq_run(d, dq_spec("sum", "sum_matches", c("a", "b"), list(total = "total")))
  expect_equal(r$findings$row, 2L)
  t <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  d <- data.frame(start = c(t, t), end = c(t + 60, t - 60))
  r <- dq_run(d, dq_spec("lag", "latency", c("start", "end"), list(max_seconds = 120)))
  expect_equal(r$findings$row, 2L)
  r <- dq_run(d, dq_spec("window", "date_window", "start", list(start = "2026-01-01T11:00:00Z", end = "2026-01-01T13:00:00Z")))
  expect_identical(r$metrics$status, "pass")
  expect_error(dqguard:::.dq_boundary("2026-02-30"))
})
test_that("freshness is a dataset rule and future data cannot pass", {
  t <- as.POSIXct("2026-01-01", tz = "UTC")
  r <- dq_run(data.frame(t = t + 60), dq_spec("fresh", "freshness", "t", list(max_seconds = 120)), context = list(now = t))
  expect_identical(r$metrics$scope, "dataset")
  expect_identical(r$metrics$status, "fail")
})
test_that("geographic limits and zero distance are explicit", {
  d <- data.frame(lat = c(0, 91), lon = c(0, 0), ref_lat = c(0, 0), ref_lon = c(0, 0))
  r <- dq_run(d, dq_rules(dq_spec("box", "geo_bounds", c("lat", "lon")),
    dq_spec("distance", "distance", names(d), list(max_metres = 0))))
  expect_false(1L %in% r$findings$row)
  expect_true(2L %in% r$findings$row)
})
test_that("ordered repeated measurements are not confused with row order", {
  d <- data.frame(id = c("a", "a", "a"), round = c(3, 1, 2), value = c(1, 1, 2))
  r <- dq_run(d, dq_spec("sequence", "nondecreasing", "value", list(group = "id", order_by = "round")))
  expect_equal(r$findings$row, 1L)
})
test_that("external adapters retain engine configuration in rule signatures", {
  fun <- function(data, context, inputs) data$x >= inputs$min
  a <- dq_adapter_rule("x", fun, "x", engine = "project", engine_version = "1", inputs = list(min = 0))
  b <- dq_adapter_rule("x", fun, "x", engine = "project", engine_version = "1", inputs = list(min = 2))
  expect_false(identical(dq_run(data.frame(x = 1), a)$rule_hash, dq_run(data.frame(x = 1), b)$rule_hash))
})
test_that("validate expressions are optional and errors are not swallowed", {
  skip_if_not_installed("validate")
  rule <- dq_validate_rule("x", validate::validator(x > 0), "x")
  r <- dq_run(data.frame(x = c(1, -1)), rule)
  expect_equal(r$findings$row, 2L)
  bad <- dq_validate_rule("bad", validate::validator(unknown_column > 0))
  expect_identical(dq_run(data.frame(x = 1), bad)$metrics$status, "error")
})
