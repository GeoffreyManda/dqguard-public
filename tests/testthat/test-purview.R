test_that("like patterns and escaped enumerations follow the Purview definitions", {
  expect_true(dqguard:::.dq_like("icecream", "ice%"))
  expect_true(dqguard:::.dq_like("YE15", "YE__"))
  expect_false(dqguard:::.dq_like("YE1514", "YE__"))
  expect_true(dqguard:::.dq_like("50%", "50\\%"))
  expect_false(dqguard:::.dq_like("50x", "50\\%"))
  expect_equal(dqguard:::.dq_enum("a \\, b, c"), c("a , b", "c"))
})

test_that("format and unique rules ignore empty values unless an empty rule is on the column", {
  d <- data.frame(id = c("A", "A", "B", NA, ""), sex = c("Male", "Female", "male", NA, "Female"), stringsAsFactors = FALSE)
  r1 <- list(dq_pv_rule("id.unique", "unique", "id"), dq_pv_rule("sex.enum", "format", "sex", list(values = "Male, Female")))
  s1 <- dq_pv_scan(d, r1)$scores
  expect_equal(s1$passed[s1$rule_id == "id.unique"], 1)
  expect_equal(s1$failed[s1$rule_id == "id.unique"], 2)
  expect_equal(s1$ignored[s1$rule_id == "id.unique"], 2)
  expect_equal(s1$score[s1$rule_id == "sex.enum"], 75)
  r2 <- c(r1, list(dq_pv_rule("id.empty", "empty", "id")))
  s2 <- dq_pv_scan(d, r2)$scores
  expect_equal(s2$empty[s2$rule_id == "id.unique"], 2)
  expect_equal(s2$score[s2$rule_id == "id.unique"], 20)
  expect_equal(s2$score[s2$rule_id == "id.empty"], 60)
})

test_that("custom rules apply filter and null expressions and error records locate failing rows", {
  d <- data.frame(pay = c("CRD", "CRD", "CSH", "CRD"), dist = c(2, -1, -5, NA), fare = c(10, 10, 10, 10))
  r <- dq_pv_rule("trip.valid", "custom", c("dist", "fare"), list(row = function(x) x$dist > 0 & x$fare > 0,
    filter = function(x) x$pay == "CRD", null = function(x) is.na(x$dist)))
  out <- dq_pv_scan(d, r, asset = "trips", run_id = "r1")
  expect_equal(out$scores$passed, 1); expect_equal(out$scores$failed, 1); expect_equal(out$scores$ignored, 2)
  expect_equal(out$errors$row, 2L); expect_equal(out$errors$asset, "trips")
})

test_that("type match counts miscast rows and duplicate rows marks every member", {
  d <- data.frame(a = c("1", "2", "x", "1"), b = c("p", "q", "r", "p"), stringsAsFactors = FALSE)
  s <- dq_pv_scan(d, list(dq_pv_rule("a.int", "type_match", "a", list(type = "integer")),
                          dq_pv_rule("ab.dup", "duplicate_rows", c("a", "b"))))$scores
  expect_equal(s$miscast[s$rule_id == "a.int"], 1)
  expect_equal(s$failed[s$rule_id == "ab.dup"], 2)
  expect_error(dq_pv_scan(d, list(dq_pv_rule("d1", "duplicate_rows", c("a", "b")), dq_pv_rule("d2", "duplicate_rows", c("b", "a")))), "at most one")
})

test_that("freshness scores 100 or 0", {
  d <- data.frame(x = 1)
  r <- dq_pv_rule("fresh", "freshness", params = list(max_age_hours = 24))
  now <- as.POSIXct("2026-10-03 12:00:00", tz = "UTC")
  expect_equal(dq_pv_scan(d, r, context = list(last_modified = now - 3600, as_of = now))$scores$score, 100)
  expect_equal(dq_pv_scan(d, r, context = list(last_modified = now - 90000, as_of = now))$scores$score, 0)
})

test_that("roll-up averages rules within columns, columns within assets and assets within products", {
  s <- data.frame(asset = c("a1", "a1", "a1", "a2"), rule_id = c("r1", "r2", "r3", "r4"), column = c("x", "x", "y", "x"),
                  dimension = "completeness", score = c(100, 50, 20, 60))
  up <- dq_pv_rollup(s, levels = data.frame(asset = c("a1", "a2"), product = c("p", "p"), domain = c("d", "d")))
  expect_equal(up$asset$score[up$asset$asset == "a1"], 47.5)
  expect_equal(up$product$score, 53.75)
  expect_equal(up$domain$score, 53.75)
})

test_that("bands, actions and alerts use the Purview cut-offs", {
  expect_equal(dq_pv_band(c(39.9, 40, 79.9, 80)), c("low", "medium", "medium", "high"))
  a <- dq_pv_actions(data.frame(rule_id = c("r1", "r2", "r3", "r4"), score = c(10, 45, 70, 90)), owner = "Firm A")
  expect_equal(a$severity, c("high", "medium", "low")); expect_true(all(a$assigned_to == "Firm A"))
  h <- data.frame(unit = c("u1", "u1", "u2", "u2"), run = c("1", "2", "1", "2"), score = c(90, 70, 50, 35))
  al <- dq_pv_alerts(h, below = 40, drop = 10)
  expect_setequal(paste(al$unit, al$condition), c("u1 score decreased", "u2 score below threshold", "u2 score decreased"))
})
