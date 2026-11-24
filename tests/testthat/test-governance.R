test_that("snapshot comparison links changes to exact change approvals", {
  a <- data.frame(id = c("a", "b"), x = c(1, 2)); b <- a; b$x[2] <- 3
  z <- dq_compare_snapshots(a, b, "id")
  expect_equal(nrow(z), 1L)
  expect_identical(z$change_type, "value_changed")
  expect_true(is.na(z$before_value)); expect_false(z$documented)
  approval <- data.frame(change_id = z$change_id, approved_by = "reviewer", evidence_uri = "ticket:123",
    evidence_sha256 = paste(rep("a",64),collapse=""), approved_at = "2026-01-01T00:00:00Z")
  expect_true(dq_compare_snapshots(a,b,"id",approvals=approval)$documented)
  b$x[2] <- 4
  expect_false(dq_compare_snapshots(a,b,"id",approvals=approval)$documented)
})
test_that("missing stale and self-reviewed controls cannot masquerade as passes", {
  controls <- data.frame(control_id="access",domain="security",requirement="Approved access",severity="critical")
  evidence <- data.frame(control_id="access",outcome="pass",evidence_uri="ticket:1",evidence_sha256=paste(rep("a",64),collapse=""),
    reviewer="bob",producer="bob",reviewed_at="2026-01-01T00:00:00Z",rationale="Reviewed")
  at <- as.POSIXct("2026-01-02",tz="UTC")
  expect_identical(dq_assess_controls(controls,evidence,as_of=at)$status,"not_evaluated")
  evidence$reviewer <- "alice"
  expect_identical(dq_assess_controls(controls,evidence,as_of=at)$status,"pass")
  expect_identical(dq_assess_controls(controls,evidence,as_of=at+100*86400)$status,"not_evaluated")
})
test_that("audits must include every explicitly required control", {
  a <- data.frame(control_id="one",status="pass")
  r <- dq_audit_rule("access",a,c("one","two"))
  result <- dq_run(data.frame(x=1),r)
  expect_false(dq_gate(result,error=FALSE))
})
test_that("lexical warnings are not assertions of semantic defects", {
  p <- tempfile(fileext=".R"); on.exit(unlink(p)); writeLines('x %in% c("yes", "no")',p)
  a <- dq_logic_audit(p)
  expect_identical(a$status[a$control_id=="logic.hardcoded_r_lookup"],"needs_review")
})
test_that("evidence file mismatches are detected", {
  p <- tempfile(); on.exit(unlink(p)); writeLines("Evidence",p)
  c <- data.frame(control_id="x",domain="governance",requirement="review",severity="error")
  e <- data.frame(control_id="x",outcome="pass",evidence_uri=p,evidence_sha256=paste(rep("0",64),collapse=""),
    reviewer="r",producer="p",reviewed_at="2026-01-01T00:00:00Z",rationale="Reviewed")
  expect_identical(dq_assess_controls(c,e,as_of=as.POSIXct("2026-01-02",tz="UTC"),verify_local_files=TRUE)$status,"fail")
})
