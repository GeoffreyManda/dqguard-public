make_contract <- function() {
  d <- data.frame(name = c("id", "x"), type = c("character", "numeric"), required = c(TRUE, FALSE))
  d$min <- c(NA, 0); d$max_missing <- c(0, .2)
  dq_contract("example", "1", d, "id", grain = "one row per entity", owner = "steward", purpose = "test")
}
test_that("dictionary and key requirements are enforced", {
  contract <- make_contract()
  good <- dq_run(data.frame(id = c("a", "b"), x = c(1, 2)), contract)
  expect_true(dq_gate(good, error = FALSE))
  bad <- dq_run(data.frame(id = c("a", "a"), x = c(1, -1), extra = 1), contract)
  expect_false(dq_gate(bad, error = FALSE))
  expect_true(any(grepl("schema", bad$metrics$rule_id)))
  missing_key <- dq_run(data.frame(x = c(1, 2)), contract)
  expect_false(dq_gate(missing_key, error = FALSE))
})
test_that("wrong types are not silently converted", {
  r <- dq_run(data.frame(id = "a", x = "1"), make_contract())
  expect_false(dq_gate(r, error = FALSE))
  expect_true(any(r$metrics$status %in% c("error", "fail")))
})
test_that("empty datasets are not valid by default", {
  r <- dq_run(data.frame(id = character(), x = numeric()), make_contract())
  expect_false(dq_gate(r, error = FALSE))
})
test_that("declarative contract roundtrip preserves use and required fields", {
  p <- tempfile(fileext = ".json"); on.exit(unlink(p))
  dq_write_contract(make_contract(), p)
  x <- dq_read_contract(p)
  expect_identical(x$key, "id")
  expect_identical(x$grain, "one row per entity")
  expect_true(dq_gate(dq_run(data.frame(id = "a", x = 1), x), error = FALSE))
  expect_error(dq_write_contract(x, p))
})
test_that("unknown JSON instructions and executable functions are rejected", {
  p <- tempfile(fileext = ".json"); on.exit(unlink(p))
  dq_write_contract(make_contract(), p)
  x <- jsonlite::fromJSON(p, simplifyVector = FALSE); x$execute <- "system('anything')"
  jsonlite::write_json(x, p, auto_unbox = TRUE)
  expect_error(dq_read_contract(p))
  ct <- make_contract(); ct$rules <- list(dq_rule("custom", function(d,c) rep(TRUE, nrow(d))))
  expect_error(dq_write_contract(ct, tempfile()))
})
test_that("unversioned reference mismatch fails before the run", {
  ct <- make_contract(); ct$reference_versions <- list(facility = "v2")
  expect_error(dq_run(data.frame(id = "a", x = 1), ct, context = list(reference_versions = list(facility = "v1"))), class = "dq_reference_version_error")
})
test_that("required fields cannot advertise nonzero missing tolerance", {
  d <- data.frame(name = "id", type = "character", required = TRUE, max_missing = .2)
  expect_error(dq_dictionary(d))
})
