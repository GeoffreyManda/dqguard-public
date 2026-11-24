workflow_fixture <- function() {
  d <- data.frame(id="a",x=-1)
  rule <- dq_spec("positive","range","x",list(min=0))
  r <- dq_run(d,rule,dataset="test",key="id",identity_secret="test-secret")
  l <- dq_issues(r); id <- l$events$issue_id[1]
  l <- dq_transition(l,id,"triaged","triager","Assigned")
  l <- dq_transition(l,id,"remediating","collector","Correct source")
  ev <- list(uri="ticket:1",sha256=paste(rep("a",64),collapse=""))
  l <- dq_transition(l,id,"verification","collector","Ready",evidence=ev,root_cause="Source entry error",upstream_fix="Entry tool corrected")
  list(data=d,rule=rule,result=r,ledger=l,id=id,evidence=ev)
}
test_that("ledger hashes detect changed history and externally pinned truncation", {
  f <- workflow_fixture(); l <- f$ledger
  expect_true(dq_verify_ledger(l))
  bad <- l; bad$events$reason[1] <- "tampered"
  expect_false(dq_verify_ledger(bad))
  trunc <- l; trunc$events <- head(trunc$events,-1); trunc$head <- tail(trunc$events$event_hash,1)
  expect_true(dq_verify_ledger(trunc))
  expect_false(dq_verify_ledger(trunc,expected_head=l$head))
})
test_that("same-run synchronization is idempotent", {
  f <- workflow_fixture()
  again <- dq_sync_issues(f$ledger,f$result)
  expect_identical(again$head,f$ledger$head)
})
test_that("closure requires independent review and a new successful run", {
  f <- workflow_fixture(); d <- f$data; d$x <- 1
  fresh <- dq_run(d,f$rule,dataset="test",key="id",identity_secret="test-secret")
  expect_error(dq_transition(f$ledger,f$id,"closed","collector","Verified",evidence=f$evidence,verification=fresh))
  l <- dq_transition(f$ledger,f$id,"closed","reviewer","Verified",evidence=f$evidence,verification=fresh)
  expect_identical(tail(l$events$to,1),"closed")
  expect_true(dq_verify_ledger(l))
  expect_equal(dq_resolution_metrics(l)$n_closed_episodes,1L)
})
test_that("deleting an entity or weakening a rule cannot close its issue", {
  f <- workflow_fixture()
  absent <- dq_run(data.frame(id="other",x=1),f$rule,dataset="test",key="id",identity_secret="test-secret")
  expect_error(dq_transition(f$ledger,f$id,"closed","reviewer","Removed",evidence=f$evidence,verification=absent))
  weak <- dq_run(f$data,dq_spec("positive","range","x",list(min=-2)),dataset="test",key="id",identity_secret="test-secret")
  expect_error(dq_transition(f$ledger,f$id,"closed","reviewer","Changed threshold",evidence=f$evidence,verification=weak))
})
test_that("ledger snapshots are immutable through the file API", {
  f <- workflow_fixture(); p <- tempfile(fileext=".json"); on.exit(unlink(p))
  dq_save_ledger(f$ledger,p)
  expect_identical(dq_read_ledger(p,expected_head=f$ledger$head)$head,f$ledger$head)
  expect_error(dq_save_ledger(f$ledger,p))
})
test_that("waivers require expiry and never automatically bypass data gates", {
  d <- data.frame(x=-1); r <- dq_run(d,dq_spec("x","range","x",list(min=0)))
  l <- dq_issues(r); id <- l$events$issue_id[1]
  l <- dq_transition(l,id,"triaged","a","Assigned")
  ev <- list(uri="ticket:2",sha256=paste(rep("b",64),collapse=""))
  expect_error(dq_transition(l,id,"waived","b","Accepted temporarily",evidence=ev))
  l <- dq_transition(l,id,"waived","b","Accepted temporarily",evidence=ev,expires_at=Sys.time()+3600)
  expect_false(dq_gate(r,error=FALSE))
  expect_equal(dq_resolution_metrics(l)$n_waivers,1L)
})
test_that("ticket delivery requires explicit acknowledgement", {
  r <- dq_run(data.frame(x=-1),dq_spec("x","range","x",list(min=0)))
  expect_error(dq_dispatch(r,function(p) list(delivered=FALSE)),class="dq_delivery_error")
  delivered <- dq_dispatch(r,function(p) list(delivered=TRUE,id=p$idempotency_key))
  expect_equal(length(delivered),1L)
})
