dq_run <- function(data, rules, dataset = "dataset", key = NULL, context = list(),
                   trigger = "manual", stage = "ingestion", identity_secret = NULL,
                   include_error_messages = FALSE) {
  .dq_df(data); .dq_chr1(dataset, "dataset"); .dq_chr1(trigger, "trigger"); .dq_chr1(stage, "stage")
  .dq_flag(include_error_messages, "include_error_messages")
  if (!is.list(context)) .dq_abort("context must be a list of versioned inputs.")
  contract <- if (inherits(rules, "dq_contract")) rules else NULL
  if (!is.null(contract)) { dataset <- contract$dataset; key <- contract$key; rules <- .dq_compile_contract(contract) }
  if (!inherits(rules, "dq_rules")) rules <- dq_rules(rules)
  if (!is.null(key) && (!is.character(key) || !length(key) || anyNA(key) || anyDuplicated(key))) .dq_abort("key must contain unique column names.")
  if (!is.null(identity_secret)) .dq_chr1(identity_secret, "identity_secret")
  context$now <- context$now %||% Sys.time(); .dq_time(context$now)
  context$references <- context$references %||% list()
  if (!is.null(contract) && length(contract$reference_versions)) {
    observed <- context$reference_versions %||% list()
    for (nm in names(contract$reference_versions)) {
      if (!identical(observed[[nm]], contract$reference_versions[[nm]]))
        .dq_abort(paste("Reference version does not match contract:", nm), "dq_reference_version_error")
      if (is.null(context$references[[nm]])) .dq_abort(paste("Required reference unavailable:", nm))
    }
  }
  data_hash <- .dq_hash(data)
  rule_hash <- .dq_hash(list(rules = lapply(rules, .dq_rule_signature),
    dictionary = contract$dictionary, contract_version = contract$version,
    key = key, grain = contract$grain, purpose = contract$purpose,
    inputs = context[setdiff(names(context), "now")]))
  context_hash <- .dq_hash(context)
  started <- Sys.time()
  run_id <- paste0("run_", substr(.dq_hash(list(data_hash, rule_hash, context_hash, started, tempfile())), 1, 24))
  n <- nrow(data)
  # Row coordinates are default. Stable cross-snapshot pseudonyms require a secret.
  stable_identity <- !is.null(key) && all(key %in% names(data)) && !is.null(identity_secret)
  record_id <- if (n) paste0("row:", seq_len(n)) else character()
  if (!is.null(key) && all(key %in% names(data)) && !is.null(identity_secret)) {
    token <- .dq_key(data, key)
    record_id <- vapply(token, function(z) paste0("id:", digest::hmac(identity_secret, z,
      algo = "sha256", serialize = FALSE)), character(1))
  }
  out_metrics <- vector("list", length(rules)); out_findings <- vector("list", length(rules))
  out_states <- vector("list", length(rules)); names(out_states) <- vapply(rules, `[[`, character(1), "id")
  for (j in seq_along(rules)) {
    r <- rules[[j]]; error_detail <- ""; t0 <- proc.time()[[3]]
    z <- tryCatch(withCallingHandlers({
      .dq_cols(data, r$columns)
      applicable <- if (r$scope == "dataset") TRUE else rep(TRUE, n)
      if (!is.null(r$when)) {
        applicable <- r$when(data, context)
        if (!is.logical(applicable) || !is.null(dim(applicable)) || length(applicable) != n)
          .dq_abort("Applicability must be a logical vector of exactly nrow(data) values.")
      }
      ix <- which(applicable %in% TRUE)
      unknown_app <- which(is.na(applicable))
      valid <- if (r$scope == "dataset") r$check(data, context) else if (length(ix)) r$check(data[ix, , drop = FALSE], context) else logical()
      expected <- if (r$scope == "dataset") 1L else length(ix)
      if (!is.logical(valid) || !is.null(dim(valid)) || length(valid) != expected)
        .dq_abort("Rule must return a logical vector of the exact declared scope; scalar recycling is prohibited.")
      missing <- is.na(valid)
      vals <- valid
      if (r$na_policy == "fail") vals[missing] <- FALSE
      if (r$na_policy == "pass") vals[missing] <- TRUE
      fail_local <- which(vals %in% FALSE)
      bad <- c(ix[fail_local], unknown_app)
      codes <- c(ifelse(missing[fail_local], "unknown_value", "violation"), rep("unknown_applicability", length(unknown_app)))
      denominator <- sum(!is.na(vals)) + length(unknown_app)
      excluded <- if (r$na_policy == "exclude") sum(missing) else 0L
      fail_n <- length(bad)
      no_op <- denominator == 0L
      observed_inapplicable <- r$scope == "row" && n > 0L && all(applicable %in% FALSE)
      status <- if (no_op) {
        if (observed_inapplicable || r$allow_empty) "not_applicable" else "not_evaluated"
      } else if (fail_n) "fail" else "pass"
      list(status = status, denominator = denominator, failed = fail_n, excluded = excluded,
        applicable = length(ix) + length(unknown_app), unknown = sum(missing) + length(unknown_app),
        bad = bad, codes = codes, states = list(applicable = applicable, values = valid, eligible_rows = ix))
    }, warning = function(w) .dq_abort(paste("Rule warning:", conditionMessage(w)), "dq_rule_warning")),
    error = function(e) {
      error_detail <<- if (include_error_messages) conditionMessage(e) else paste0("Execution failed (", class(e)[1], ").")
      list(status = "error", denominator = 0L, failed = 0L, excluded = 0L, applicable = 0L,
        unknown = 0L, bad = integer(), codes = character(), states = NULL)
    })
    rate <- if (z$denominator > 0) z$failed / z$denominator else NA_real_
    breached <- z$status %in% c("error", "not_evaluated") ||
      (!is.na(rate) && (rate > r$max_fail_rate || z$failed > r$max_fail_count))
    out_metrics[[j]] <- data.frame(run_id = run_id, dataset = dataset, rule_id = r$id,
      rule_version = r$version, dimension = r$dimension, severity = r$severity,
      action = r$action, scope = r$scope, status = z$status, n_applicable = z$applicable,
      n_evaluated = z$denominator, n_excluded = z$excluded, n_unknown = z$unknown,
      n_failed = z$failed, fail_rate = rate, max_fail_rate = r$max_fail_rate,
      max_fail_count = r$max_fail_count, breached = breached, owner = r$owner,
      elapsed_seconds = unname(proc.time()[[3]] - t0), error = error_detail, stringsAsFactors = FALSE)
    out_states[j] <- list(z$states)
    bad <- z$bad; codes <- z$codes
    if (r$scope == "dataset" && length(bad)) bad[] <- NA_integer_
    if (z$status %in% c("error", "not_evaluated")) { bad <- NA_integer_; codes <- z$status }
    if (length(bad)) {
      rid <- ifelse(is.na(bad), "__dataset__", record_id[bad])
      # Unkeyed findings are snapshot-specific; no claim of anonymous identifiers.
      fid <- vapply(rid, function(id) .dq_hash(list(dataset, r$id, r$version, id,
        if (!stable_identity) data_hash else NULL)), character(1))
      out_findings[[j]] <- data.frame(run_id = run_id, dataset = dataset, rule_id = r$id,
        rule_version = r$version, dimension = r$dimension, severity = r$severity,
        row = as.integer(bad), record_id = rid, finding_id = fid,
        fields = paste(r$columns, collapse = ","), code = codes,
        message = if (z$status %in% c("error", "not_evaluated")) paste("Rule", z$status) else
          if (nzchar(r$rationale)) r$rationale else paste("Requirement violated:", r$id),
        action = r$action, stringsAsFactors = FALSE)
    }
  }
  structure(list(run_id = run_id, dataset = dataset, contract_version = contract$version %||% NA_character_,
    data_hash = data_hash, rule_hash = rule_hash, context_hash = context_hash,
    n_rows = n, n_columns = ncol(data), key = key, record_ids = record_id, stable_identity = stable_identity,
    started_at = .dq_time(started), completed_at = .dq_time(), evaluated_at = .dq_time(context$now),
    trigger = trigger, stage = stage, metrics = .dq_bind(out_metrics),
    findings = .dq_bind(out_findings, .dq_empty_findings()), states = out_states,
    rule_ids = vapply(rules, `[[`, character(1), "id"),
    runtime = list(R = as.character(getRversion()), package = as.character(utils::packageVersion("dqguard")))), class = "dq_result")
}

dq_gate <- function(result, mode = c("full", "partition"), error = TRUE) {
  if (!inherits(result, "dq_result")) .dq_abort("Expected a dq_result.")
  mode <- match.arg(mode); .dq_flag(error, "error")
  m <- result$metrics; f <- result$findings
  blocking <- m$action %in% c("block", "quarantine")
  hold <- any(blocking & m$breached)
  # Full-snapshot release must not include any quarantinable row, even below tolerance.
  if (mode == "full") hold <- hold || any(f$action == "quarantine" & !is.na(f$row))
  if (hold && error) .dq_abort(paste("Data release blocked for", result$dataset, "in", mode, "mode."), "dq_gate_error")
  !hold
}

dq_partition <- function(data, result) {
  .dq_df(data)
  if (!inherits(result, "dq_result") || !identical(.dq_hash(data), result$data_hash))
    .dq_abort("Result does not describe this exact dataset snapshot.", "dq_stale_result")
  q <- sort(unique(result$findings$row[result$findings$action == "quarantine" & !is.na(result$findings$row)]))
  remaining <- setdiff(seq_len(nrow(data)), q)
  hold <- !dq_gate(result, mode = "partition", error = FALSE)
  a <- if (hold) integer() else remaining; h <- if (hold) remaining else integer()
  list(accepted = data[a, , drop = FALSE], quarantine = data[q, , drop = FALSE],
    held = data[h, , drop = FALSE], indices = list(accepted = a, quarantine = q, held = h),
    run_id = result$run_id, release_allowed = !hold)
}

print.dq_result <- function(x, ...) {
  cat("Data quality:", x$dataset, "\n")
  cat(x$n_rows, "rows;", nrow(x$metrics), "rules;", nrow(x$findings), "findings.\n")
  print(x$metrics[c("rule_id", "status", "n_evaluated", "n_failed", "fail_rate", "breached")], row.names = FALSE)
  invisible(x)
}
