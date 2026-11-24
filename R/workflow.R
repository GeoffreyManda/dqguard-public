.dq_event_fields <- c("sequence", "kind", "issue_id", "finding_id", "dataset", "rule_id", "rule_version",
  "rule_hash", "record_id", "stable_identity", "data_hash", "run_id", "severity", "from", "to", "actor", "timestamp",
  "reason", "evidence_uri", "evidence_sha256", "root_cause", "upstream_fix", "expires_at", "previous_hash", "event_hash")
.dq_genesis <- paste(rep("0", 64), collapse = "")
.dq_transitions <- list(open = "triaged", triaged = c("remediating", "waived", "false_positive"),
  remediating = "verification", verification = c("closed", "remediating"), closed = "reopened",
  reopened = "triaged", waived = "reopened", false_positive = "reopened")
.dq_new_ledger <- function() structure(list(schema_version = "1", events = as.data.frame(
  stats::setNames(rep(list(character()), length(.dq_event_fields)), .dq_event_fields), stringsAsFactors = FALSE),
  head = .dq_genesis), class = "dq_ledger")
.dq_event_hash <- function(row) digest::digest(.dq_json(as.list(row[setdiff(.dq_event_fields, "event_hash")])),
                                             algo = "sha256", serialize = FALSE)

dq_verify_ledger <- function(ledger, expected_head = NULL) {
  if (!inherits(ledger, "dq_ledger") || !identical(ledger$schema_version, "1") || !is.data.frame(ledger$events) ||
      !identical(names(ledger$events), .dq_event_fields) ||
      !all(vapply(ledger$events, is.character, logical(1))) || anyNA(ledger$events)) return(FALSE)
  prev <- .dq_genesis
  for (i in seq_len(nrow(ledger$events))) {
    e <- ledger$events[i, , drop = FALSE]
    if (!identical(e$sequence, as.character(i)) || !identical(e$previous_hash, prev) ||
        !identical(e$event_hash, .dq_event_hash(e)) || is.na(.dq_parse_time(e$timestamp))) return(FALSE)
    if (!nzchar(e$issue_id) || !nzchar(e$actor) || !nzchar(e$reason) ||
        !e$kind %in% c("detection", "observed", "transition", "reappearance")) return(FALSE)
    if (e$to == "waived" && is.na(.dq_parse_time(e$expires_at))) return(FALSE)
    if (i > 1L && .dq_parse_time(e$timestamp) < .dq_parse_time(ledger$events$timestamp[i - 1L])) return(FALSE)
    previous <- ledger$events[seq_len(i - 1L), , drop = FALSE]
    same <- previous[previous$issue_id == e$issue_id, , drop = FALSE]
    if (!nrow(same)) {
      if (e$from != "" || e$to != "open" || e$kind != "detection") return(FALSE)
    } else {
      last <- same[nrow(same), , drop = FALSE]
      if (e$from != last$to) return(FALSE)
      if (!(e$kind == "observed" && e$to == e$from) && !e$to %in% .dq_transitions[[e$from]]) return(FALSE)
    }
    prev <- e$event_hash
  }
  identical(prev, ledger$head) && (is.null(expected_head) || identical(prev, expected_head))
}
.dq_ledger_assert <- function(ledger, expected_head = NULL) {
  if (!dq_verify_ledger(ledger, expected_head)) .dq_abort("Ledger verification or expected-head check failed.", "dq_ledger_error")
}
.dq_append_event <- function(ledger, event) {
  row <- stats::setNames(rep(list(""), length(.dq_event_fields)), .dq_event_fields)
  for (nm in intersect(names(event), names(row))) row[[nm]] <- as.character(event[[nm]] %||% "")
  row$sequence <- as.character(nrow(ledger$events) + 1L); row$previous_hash <- ledger$head
  row <- as.data.frame(row, stringsAsFactors = FALSE)
  if (nrow(ledger$events) && .dq_parse_time(row$timestamp) < .dq_parse_time(tail(ledger$events$timestamp, 1)))
    .dq_abort("Event time cannot precede the current ledger head.")
  row$event_hash <- .dq_event_hash(row)
  ledger$events <- rbind(ledger$events, row); rownames(ledger$events) <- NULL
  ledger$head <- row$event_hash; ledger
}

dq_issues <- function(result = NULL, at = Sys.time()) {
  ledger <- .dq_new_ledger()
  if (is.null(result)) return(ledger)
  dq_sync_issues(ledger, result, at = at)
}

dq_current_issues <- function(ledger) {
  .dq_ledger_assert(ledger)
  e <- ledger$events
  e[!duplicated(e$issue_id, fromLast = TRUE), , drop = FALSE]
}

dq_sync_issues <- function(ledger, result, at = Sys.time()) {
  .dq_ledger_assert(ledger); .dq_time(at)
  if (!inherits(result, "dq_result")) .dq_abort("Expected a dq_result.")
  findings <- result$findings[result$findings$action != "none", , drop = FALSE]
  findings <- findings[!duplicated(findings$finding_id), , drop = FALSE]
  for (i in seq_len(nrow(findings))) {
    f <- findings[i, , drop = FALSE]; issue_id <- paste0("DQ-", f$finding_id)
    old <- ledger$events[ledger$events$issue_id == issue_id, , drop = FALSE]
    if (nrow(old) && result$run_id %in% old$run_id) next
    to <- "open"; from <- ""; kind <- "detection"; reason <- "Validation finding recorded."
    if (nrow(old)) {
      last <- old[nrow(old), , drop = FALSE]; from <- last$to; to <- from; kind <- "observed"
      expired <- from == "waived" && nzchar(last$expires_at) && .dq_parse_time(last$expires_at) <= at
      if (from == "closed" || expired) { to <- "reopened"; kind <- "reappearance"; reason <- "Finding recurred after closure or waiver expiry." }
    }
    event <- if (nrow(old)) as.list(old[nrow(old), , drop = FALSE]) else list()
    fresh <- list(kind = kind, issue_id = issue_id, finding_id = f$finding_id,
      dataset = f$dataset, rule_id = f$rule_id, rule_version = f$rule_version,
      rule_hash = result$rule_hash, record_id = f$record_id, stable_identity = if (result$stable_identity) "true" else "false",
      data_hash = result$data_hash, run_id = result$run_id, severity = f$severity,
      from = from, to = to, actor = "dqguard", timestamp = .dq_time(at), reason = reason)
    event[names(fresh)] <- fresh
    ledger <- .dq_append_event(ledger, event)
  }
  ledger
}

dq_transition <- function(ledger, issue_id, to, actor, reason, evidence = NULL,
                          root_cause = NULL, upstream_fix = NULL, verification = NULL,
                          expires_at = NULL, at = Sys.time(), expected_head = NULL) {
  .dq_ledger_assert(ledger, expected_head)
  for (nm in c("issue_id", "to", "actor", "reason")) .dq_chr1(get(nm), nm)
  .dq_time(at)
  history <- ledger$events[ledger$events$issue_id == issue_id, , drop = FALSE]
  if (!nrow(history)) .dq_abort("Issue does not exist.")
  last <- history[nrow(history), , drop = FALSE]; from <- last$to
  if (!to %in% .dq_transitions[[from]]) .dq_abort(paste("Forbidden transition:", from, "to", to), "dq_transition_error")
  if (!is.null(verification) && to != "closed") .dq_abort("Supply the verification result only for closure.")
  requires_evidence <- to %in% c("verification", "closed", "waived", "false_positive")
  if (requires_evidence) {
    if (!is.list(evidence) || !all(c("uri", "sha256") %in% names(evidence))) .dq_abort("Evidence URI and SHA256 are required.")
    .dq_chr1(evidence$uri, "evidence URI"); .dq_chr1(evidence$sha256, "evidence SHA256")
    if (!grepl("^[a-fA-F0-9]{64}$", evidence$sha256)) .dq_abort("Evidence SHA256 must be 64 hexadecimal characters.")
  }
  root_cause <- root_cause %||% last$root_cause
  upstream_fix <- upstream_fix %||% last$upstream_fix
  if (to %in% c("verification", "closed")) {
    .dq_chr1(root_cause, "root_cause"); .dq_chr1(upstream_fix, "upstream_fix")
  }
  implementers <- unique(history$actor[history$to %in% c("remediating", "verification") & history$kind == "transition"])
  if (!length(implementers) && to %in% c("waived", "false_positive"))
    implementers <- unique(history$actor[history$to == "triaged" & history$kind == "transition"])
  if (to %in% c("closed", "waived", "false_positive") && actor %in% implementers)
    .dq_abort("The remediation actor cannot independently approve closure or disposition.")
  if (to == "waived") {
    .dq_time(expires_at)
    if (expires_at <= at) .dq_abort("A waiver must expire in the future.")
  }
  if (to == "closed") {
    if (!inherits(verification, "dq_result")) .dq_abort("Closure requires a fresh dq_result, not a manually supplied TRUE.")
    if (verification$run_id == last$run_id || verification$dataset != last$dataset || verification$rule_hash != last$rule_hash)
      .dq_abort("Closure requires a different run of the same dataset and unchanged rule set.")
    detection <- history[history$kind %in% c("detection", "observed", "reappearance"), , drop = FALSE]
    if (.dq_parse_time(verification$started_at) <= .dq_parse_time(tail(detection$timestamp, 1)))
      .dq_abort("Verification must run after the most recent detection.")
    if (.dq_parse_time(verification$completed_at) > at) .dq_abort("Closure cannot precede the verification run.")
    m <- verification$metrics[verification$metrics$rule_id == last$rule_id & verification$metrics$rule_version == last$rule_version, , drop = FALSE]
    if (nrow(m) != 1L || m$status %in% c("error", "not_evaluated", "not_applicable")) .dq_abort("The original rule was not re-evaluated.")
    if (last$record_id == "__dataset__") {
      if (m$status != "pass" || m$n_failed != 0L) .dq_abort("The dataset-level requirement still fails.")
    } else {
      if (last$stable_identity != "true" || !verification$stable_identity)
        .dq_abort("Record-level closure requires stable keyed identities in both runs; row numbers are not sufficient.")
      ix <- which(verification$record_ids == last$record_id)
      if (!length(ix)) .dq_abort("The original entity is absent from verification; deletion is not proof of correction.")
      state <- verification$states[[last$rule_id]]
      if (is.null(state) || !all(state$applicable[ix] %in% TRUE)) .dq_abort("The affected entity was not applicable to the recheck.")
      loc <- match(ix, state$eligible_rows)
      if (anyNA(loc) || !all(state$values[loc] %in% TRUE)) .dq_abort("The affected entity has not passed the original check.")
    }
  }
  event <- as.list(last)
  event$kind <- "transition"; event$from <- from; event$to <- to; event$actor <- actor
  event$timestamp <- .dq_time(at); event$reason <- reason
  event$root_cause <- root_cause; event$upstream_fix <- upstream_fix
  if (!is.null(evidence)) { event$evidence_uri <- evidence$uri; event$evidence_sha256 <- tolower(evidence$sha256) }
  if (to == "waived") event$expires_at <- .dq_time(expires_at)
  if (to != "waived" && from == "waived") event$expires_at <- ""
  if (!is.null(verification)) { event$run_id <- verification$run_id; event$data_hash <- verification$data_hash }
  .dq_append_event(ledger, event)
}

dq_resolution_metrics <- function(ledger, as_of = Sys.time(), sla_hours = 48) {
  .dq_ledger_assert(ledger); .dq_time(as_of); .dq_num1(sla_hours, "sla_hours", 0)
  e <- ledger$events; current <- dq_current_issues(ledger)
  if (nrow(e) && .dq_parse_time(tail(e$timestamp, 1)) > as_of) .dq_abort("as_of precedes ledger events.")
  closed <- e[e$to == "closed" & e$kind == "transition", , drop = FALSE]
  durations <- vapply(seq_len(nrow(closed)), function(i) {
    h <- e[e$issue_id == closed$issue_id[i] & as.integer(e$sequence) <= as.integer(closed$sequence[i]) &
      e$to %in% c("open", "reopened") & e$kind != "observed", , drop = FALSE]
    as.numeric(difftime(.dq_parse_time(closed$timestamp[i]), .dq_parse_time(tail(h$timestamp, 1)), units = "hours"))
  }, numeric(1))
  active <- current[!current$to %in% c("closed", "waived", "false_positive"), , drop = FALSE]
  ages <- vapply(active$issue_id, function(id) {
    h <- e[e$issue_id == id & e$to %in% c("open", "reopened") & e$kind != "observed", , drop = FALSE]
    as.numeric(difftime(as_of, .dq_parse_time(tail(h$timestamp, 1)), units = "hours"))
  }, numeric(1))
  waivers <- current[current$to == "waived", , drop = FALSE]
  expired <- if (nrow(waivers)) sum(.dq_parse_time(waivers$expires_at) <= as_of) else 0L
  data.frame(n_issues = nrow(current), n_closed_episodes = length(durations),
    median_hours_to_close = if (length(durations)) stats::median(durations) else NA_real_,
    n_active = nrow(active), median_open_age_hours = if (length(ages)) stats::median(ages) else NA_real_,
    p95_open_age_hours = if (length(ages)) unname(stats::quantile(ages, .95)) else NA_real_,
    n_overdue_active = sum(ages > sla_hours), n_waivers = nrow(waivers), n_expired_waivers = expired,
    n_reopened = sum(e$to == "reopened" & e$kind != "observed"))
}

dq_save_ledger <- function(ledger, path) {
  .dq_ledger_assert(ledger)
  # Immutable snapshot files: existing logs are never overwritten.
  .dq_atomic_write(path, function(p) jsonlite::write_json(unclass(ledger), p,
    auto_unbox = TRUE, dataframe = "rows", pretty = TRUE), overwrite = FALSE)
}

dq_read_ledger <- function(path, expected_head = NULL) {
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(x) || !all(c("schema_version", "events", "head") %in% names(x))) .dq_abort("Invalid ledger file.")
  blank <- .dq_new_ledger()
  rows <- lapply(x$events, function(e) {
    if (!is.list(e) || !identical(names(e), .dq_event_fields) || !all(vapply(e, function(z) is.character(z) && length(z) == 1L, logical(1))))
      .dq_abort("Malformed event record.")
    as.data.frame(e, stringsAsFactors = FALSE)
  })
  blank$events <- .dq_bind(rows, blank$events); blank$head <- x$head; blank$schema_version <- x$schema_version
  .dq_ledger_assert(blank, expected_head); blank
}

dq_ticket_payloads <- function(result, breached_only = TRUE) {
  if (!inherits(result, "dq_result")) .dq_abort("Expected a dq_result.")
  .dq_flag(breached_only, "breached_only")
  m <- result$metrics
  if (breached_only) m <- m[m$breached, , drop = FALSE]
  lapply(seq_len(nrow(m)), function(i) {
    f <- result$findings[result$findings$rule_id == m$rule_id[i], , drop = FALSE]
    list(idempotency_key = .dq_hash(list(result$run_id, m$rule_id[i])), dataset = result$dataset,
      run_id = result$run_id, rule_id = m$rule_id[i], owner = m$owner[i], severity = m$severity[i],
      n_failed = m$n_failed[i], n_evaluated = m$n_evaluated[i], status = m$status[i],
      affected_rows = as.list(f$row), finding_ids = as.list(unique(f$finding_id)))
  })
}

dq_dispatch <- function(result, sender) {
  if (!is.function(sender)) .dq_abort("sender must be an explicitly supplied function(payload).")
  payloads <- dq_ticket_payloads(result)
  lapply(payloads, function(p) {
    response <- sender(p)
    if (!is.list(response) || !isTRUE(response$delivered) || is.null(response$id))
      .dq_abort("Sender did not confirm delivery with delivered=TRUE and an external id.", "dq_delivery_error")
    response
  })
}
