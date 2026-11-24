# DBI is optional. Raw records are written only by the explicit routing API.
.dq_db_check <- function(con) {
  .dq_need("DBI")
  if (!DBI::dbIsValid(con)) .dq_abort("Database connection is not valid.")
}
.dq_db_name <- function(con, table) {
  if (!(is.character(table) && length(table) == 1L && !is.na(table) && nzchar(table)) &&
      !inherits(table, "Id")) .dq_abort("Use a single table name or DBI::Id; SQL fragments are not accepted.")
  as.character(DBI::dbQuoteIdentifier(con, table))
}
.dq_db_prefix <- function(prefix) {
  .dq_chr1(prefix, "prefix")
  if (!grepl("^[A-Za-z][A-Za-z0-9_]{0,30}$", prefix)) .dq_abort("Invalid audit table prefix.")
  prefix
}
.dq_db_literal <- function(con, x) as.character(DBI::dbQuoteString(con, as.character(x)))
.dq_db_table <- function(prefix, suffix) paste0(.dq_db_prefix(prefix), "_", suffix)

dq_db_read <- function(con, table, max_rows = 1000000L) {
  .dq_db_check(con); .dq_num1(max_rows, "max_rows", 1, .Machine$integer.max - 1)
  if (max_rows != floor(max_rows)) .dq_abort("max_rows must be an integer.")
  query <- DBI::dbSendQuery(con, paste("SELECT * FROM", .dq_db_name(con, table)))
  on.exit(DBI::dbClearResult(query), add = TRUE)
  data <- DBI::dbFetch(query, n = as.integer(max_rows + 1L))
  if (nrow(data) > max_rows || !DBI::dbHasCompleted(query))
    .dq_abort("Snapshot exceeds max_rows; no sampled data-quality pass is returned.", "dq_capacity_error")
  .dq_df(data); data
}

dq_run_db <- function(con, table, rules, max_rows = 1000000L, context = list(), ...) {
  data <- dq_db_read(con, table, max_rows)
  context$source_table <- .dq_db_name(con, table)
  dq_run(data, rules, context = context, ...)
}

dq_db_audit <- function(con, table, contract) {
  .dq_db_check(con)
  if (!inherits(contract, "dq_contract")) .dq_abort("Expected a dq_contract.")
  qtable <- .dq_db_name(con, table); out <- list()
  add <- function(id, status, detail) {
    out[[length(out) + 1L]] <<- .dq_audit_row(id, "database", status, detail, qtable)
  }
  tryCatch({
    fields <- DBI::dbListFields(con, table)
    expected <- contract$dictionary$name
    add("db.columns", if (all(expected %in% fields) && (contract$allow_extra || setequal(fields, expected))) "pass" else "fail",
      paste("Missing:", paste(setdiff(expected, fields), collapse = ","), "Extra:", paste(setdiff(fields, expected), collapse = ",")))
    if (inherits(con, "SQLiteConnection") && is.character(table)) {
      info <- DBI::dbGetQuery(con, paste0("PRAGMA table_info(", qtable, ")"))
      for (nm in contract$dictionary$name[contract$dictionary$required]) {
        i <- match(nm, info$name)
        # Do not infer NOT NULL from PRIMARY KEY: SQLite has documented exceptions.
        status <- if (is.na(i)) "fail" else if (info$notnull[i] == 1L) "pass" else "not_evaluated"
        add(paste0("db.not_null.", nm), status, "Explicit NOT NULL checked. Implicit key/STRICT/rowid semantics require separate review.")
      }
      pk <- info$name[order(info$pk)][sort(info$pk) > 0L]
      add("db.primary_key", if (identical(pk, contract$key)) "pass" else "fail", "Compared declared primary-key columns and order with the contract grain.")
      fk <- DBI::dbGetQuery(con, "PRAGMA foreign_keys")
      add("db.foreign_keys_enabled", if (nrow(fk) && isTRUE(as.integer(fk[[1]][1]) == 1L)) "pass" else "fail", "SQLite foreign-key enforcement is connection-specific.")
      ignore <- DBI::dbGetQuery(con, "PRAGMA ignore_check_constraints")
      add("db.checks_enabled", if (nrow(ignore) && isTRUE(as.integer(ignore[[1]][1]) == 0L)) "pass" else "fail", "Checked ignore_check_constraints for this connection.")
      add("db.storage_types", "not_evaluated", "Declared SQLite types do not prove strict stored-value types. Validate the snapshot and review STRICT-table configuration.")
    } else {
      add("db.primary_key", "not_evaluated", "This release provides native constraint introspection for SQLite only. Supply reviewed catalogue evidence for this database driver.")
      for (nm in contract$dictionary$name[contract$dictionary$required])
        add(paste0("db.not_null.", nm), "not_evaluated", "Driver-specific nullability introspection is not implemented.")
    }
    add("db.constraint_semantics", "not_evaluated", "CHECK/CASE/trigger equivalence to business definitions requires semantic review; name matching is not proof.")
  }, error = function(e) add("db.inspection", "not_evaluated", conditionMessage(e)))
  .dq_bind(out)
}

dq_db_init <- function(con, prefix = "dq") {
  .dq_db_check(con); .dq_db_prefix(prefix)
  definitions <- list(
    runs = "run_id TEXT PRIMARY KEY NOT NULL, dataset TEXT NOT NULL, data_hash TEXT NOT NULL, rule_hash TEXT NOT NULL, payload TEXT NOT NULL",
    metrics = "run_id TEXT NOT NULL, rule_id TEXT NOT NULL, payload TEXT NOT NULL, PRIMARY KEY (run_id, rule_id)",
    findings = "run_id TEXT NOT NULL, rule_id TEXT NOT NULL, row_index INTEGER NOT NULL, payload TEXT NOT NULL, PRIMARY KEY (run_id, rule_id, row_index)",
    routes = "run_id TEXT PRIMARY KEY NOT NULL, data_hash TEXT NOT NULL, accepted_n INTEGER NOT NULL, quarantine_n INTEGER NOT NULL, held_n INTEGER NOT NULL, routed_at TEXT NOT NULL",
    ledger_heads = "ledger_id TEXT PRIMARY KEY NOT NULL, head TEXT NOT NULL",
    ledger_events = "ledger_id TEXT NOT NULL, sequence INTEGER NOT NULL, event_hash TEXT NOT NULL, payload TEXT NOT NULL, PRIMARY KEY (ledger_id, sequence)")
  DBI::dbWithTransaction(con, {
    for (nm in names(definitions)) DBI::dbExecute(con, paste0("CREATE TABLE IF NOT EXISTS ",
      .dq_db_name(con, .dq_db_table(prefix, nm)), " (", definitions[[nm]], ")"))
  })
  invisible(TRUE)
}

dq_db_store <- function(con, result, prefix = "dq") {
  .dq_db_check(con); .dq_db_prefix(prefix)
  if (!inherits(result, "dq_result")) .dq_abort("Expected a dq_result.")
  metadata <- result[setdiff(names(result), c("metrics", "findings", "states", "record_ids"))]
  runs <- data.frame(run_id = result$run_id, dataset = result$dataset, data_hash = result$data_hash,
    rule_hash = result$rule_hash, payload = .dq_json(metadata), stringsAsFactors = FALSE)
  m <- result$metrics; f <- result$findings
  metrics <- data.frame(run_id = m$run_id, rule_id = m$rule_id,
    payload = vapply(seq_len(nrow(m)), function(i) .dq_json(as.list(m[i, , drop = FALSE])), character(1)))
  findings <- data.frame(run_id = f$run_id, rule_id = f$rule_id, row_index = ifelse(is.na(f$row), 0L, f$row),
    payload = vapply(seq_len(nrow(f)), function(i) .dq_json(as.list(f[i, , drop = FALSE])), character(1)))
  DBI::dbWithTransaction(con, {
    DBI::dbAppendTable(con, .dq_db_table(prefix, "runs"), runs)
    if (nrow(metrics)) DBI::dbAppendTable(con, .dq_db_table(prefix, "metrics"), metrics)
    if (nrow(findings)) DBI::dbAppendTable(con, .dq_db_table(prefix, "findings"), findings)
  })
  invisible(result$run_id)
}

dq_db_route <- function(con, data, result, accepted_table, quarantine_table, held_table, prefix = "dq") {
  .dq_db_check(con); .dq_db_prefix(prefix)
  tables <- list(accepted_table, quarantine_table, held_table)
  quoted <- vapply(tables, function(t) .dq_db_name(con, t), character(1))
  if (anyDuplicated(quoted)) .dq_abort("Accepted, quarantine and held destinations must be distinct tables.")
  reserved <- vapply(c("runs", "metrics", "findings", "routes", "ledger_heads", "ledger_events"),
    function(s) .dq_db_name(con, .dq_db_table(prefix, s)), character(1))
  if (any(quoted %in% reserved)) .dq_abort("Raw data cannot be routed into audit tables.")
  for (t in tables) {
    if (!DBI::dbExistsTable(con, t)) .dq_abort("All protected destination tables must be provisioned before routing.")
    if (!identical(DBI::dbListFields(con, t), names(data))) .dq_abort("Routing destination schema does not match the snapshot.")
  }
  partition <- dq_partition(data, result)
  route <- data.frame(run_id = result$run_id, data_hash = result$data_hash,
    accepted_n = nrow(partition$accepted), quarantine_n = nrow(partition$quarantine),
    held_n = nrow(partition$held), routed_at = .dq_time(), stringsAsFactors = FALSE)
  DBI::dbWithTransaction(con, {
    # Inserting the unique receipt first makes retries conflict rather than duplicate rows.
    DBI::dbAppendTable(con, .dq_db_table(prefix, "routes"), route)
    for (i in seq_along(tables)) if (nrow(partition[[i]])) DBI::dbAppendTable(con, tables[[i]], partition[[i]])
  })
  invisible(route)
}

dq_db_load_ledger <- function(con, ledger_id = "main", prefix = "dq", expected_head = NULL) {
  .dq_db_check(con); .dq_chr1(ledger_id, "ledger_id")
  lit <- .dq_db_literal(con, ledger_id)
  heads <- DBI::dbGetQuery(con, paste("SELECT head FROM", .dq_db_name(con, .dq_db_table(prefix, "ledger_heads")), "WHERE ledger_id =", lit))
  ledger <- .dq_new_ledger()
  if (!nrow(heads)) {
    if (!is.null(expected_head) && expected_head != .dq_genesis) .dq_abort("Expected ledger does not exist.")
    return(ledger)
  }
  events <- DBI::dbGetQuery(con, paste("SELECT payload FROM", .dq_db_name(con, .dq_db_table(prefix, "ledger_events")),
    "WHERE ledger_id =", lit, "ORDER BY sequence"))
  rows <- lapply(events$payload, function(p) {
    row <- jsonlite::fromJSON(p, simplifyVector = FALSE)
    if (!identical(names(row), .dq_event_fields)) .dq_abort("Stored ledger event fields are invalid.")
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  ledger$events <- .dq_bind(rows, ledger$events); ledger$head <- heads$head[1]
  .dq_ledger_assert(ledger, expected_head); ledger
}

dq_db_save_ledger <- function(con, ledger, ledger_id = "main", expected_head, prefix = "dq") {
  .dq_db_check(con); .dq_ledger_assert(ledger); .dq_chr1(expected_head, "expected_head"); .dq_chr1(ledger_id, "ledger_id")
  headtable <- .dq_db_name(con, .dq_db_table(prefix, "ledger_heads"))
  lit <- .dq_db_literal(con, ledger_id)
  DBI::dbWithTransaction(con, {
    old <- dq_db_load_ledger(con, ledger_id, prefix, expected_head)
    n <- nrow(old$events)
    if (nrow(ledger$events) < n || (n && !identical(ledger$events$event_hash[seq_len(n)], old$events$event_hash)))
      .dq_abort("New ledger does not extend the stored history.", "dq_ledger_conflict")
    exists <- DBI::dbGetQuery(con, paste("SELECT head FROM", headtable, "WHERE ledger_id =", lit))
    if (!nrow(exists)) DBI::dbAppendTable(con, .dq_db_table(prefix, "ledger_heads"),
      data.frame(ledger_id = ledger_id, head = ledger$head)) else {
      changed <- DBI::dbExecute(con, paste("UPDATE", headtable, "SET head =", .dq_db_literal(con, ledger$head),
        "WHERE ledger_id =", lit, "AND head =", .dq_db_literal(con, expected_head)))
      if (changed != 1L) .dq_abort("Ledger head changed; reload before retrying.", "dq_ledger_conflict")
    }
    if (nrow(ledger$events) > n) {
      ix <- seq.int(n + 1L, nrow(ledger$events))
      DBI::dbAppendTable(con, .dq_db_table(prefix, "ledger_events"), data.frame(
        ledger_id = rep(ledger_id, length(ix)), sequence = ix, event_hash = ledger$events$event_hash[ix],
        payload = vapply(ix, function(i) .dq_json(as.list(ledger$events[i, , drop = FALSE])), character(1))))
    }
  })
  invisible(ledger$head)
}
