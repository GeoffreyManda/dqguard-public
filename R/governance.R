dq_git_audit <- function(path, files, expected_commit = NULL) {
  .dq_chr1(path, "path")
  if (!is.character(files) || !length(files) || anyNA(files)) .dq_abort("Supply the required relative file paths.")
  if (any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$))", files))) .dq_abort("Required files must be inside the repository.")
  git <- Sys.which("git")
  if (!nzchar(git)) return(.dq_audit_row("git.available", "version_control", "not_evaluated", "Git executable unavailable."))
  cmd <- function(args) {
    z <- suppressWarnings(system2(git, c("-C", shQuote(path), args), stdout = TRUE, stderr = TRUE))
    list(ok = is.null(attr(z, "status")) || attr(z, "status") == 0L, text = z)
  }
  head <- cmd(c("rev-parse", "HEAD"))
  if (!head$ok) return(.dq_audit_row("git.repository", "version_control", "not_evaluated", "Repository or commit unavailable."))
  commit <- head$text[1]
  rows <- list(.dq_audit_row("git.commit", "version_control", if (is.null(expected_commit) || identical(commit, expected_commit)) "pass" else "fail", commit))
  for (f in files) {
    tracked <- cmd(c("ls-files", "--error-unmatch", "--", shQuote(f)))
    dirty <- cmd(c("status", "--porcelain", "--", shQuote(f)))
    history <- cmd(c("log", "-1", "--format=%H %cI", "--", shQuote(f)))
    rows[[length(rows) + 1L]] <- .dq_audit_row("git.required_file", "version_control",
      if (!tracked$ok || length(dirty$text)) "fail" else if (!dirty$ok || !history$ok || !length(history$text)) "not_evaluated" else "pass",
      if (!tracked$ok) "Required file is not tracked." else if (length(dirty$text)) "Tracked file has uncommitted changes or is deleted." else paste(history$text, collapse = " "), f)
  }
  .dq_bind(rows)
}

dq_logic_audit <- function(files, max_bytes = 5 * 1024^2) {
  patterns <- c(hardcoded_r_lookup = "%in%[[:space:]]*c[[:space:]]*\\(",
    hardcoded_sql_lookup = "\\bIN[[:space:]]*\\([^)]*['\"]",
    conditional_sql = "\\bCASE[[:space:]]+WHEN\\b", possible_silent_coercion = "as\\.(numeric|integer|Date)[[:space:]]*\\(",
    possible_overwrite = "(write\\.(csv|table)|write_[a-z]+|dbWriteTable)[[:space:]]*\\(")
  rows <- list()
  for (f in files) {
    if (!file.exists(f) || file.info(f)$size > max_bytes) {
      rows[[length(rows) + 1L]] <- .dq_audit_row("logic.readable", "logic", "not_evaluated", "File unavailable or too large.", basename(f)); next
    }
    lines <- readLines(f, warn = FALSE)
    for (id in names(patterns)) {
      ix <- grep(patterns[[id]], lines, ignore.case = TRUE, perl = TRUE)
      rows[[length(rows) + 1L]] <- .dq_audit_row(paste0("logic.", id), "logic",
        if (length(ix)) "needs_review" else "pass",
        if (length(ix)) paste("Review lines", paste(ix, collapse = ","), "; this is a static signal, not a demonstrated defect.") else
          "Pattern not found. Absence does not prove semantic correctness.", basename(f), "warning")
    }
  }
  .dq_bind(rows)
}

dq_compare_snapshots <- function(before, after, key, fields = intersect(names(before), names(after)),
                                 approvals = NULL, include_values = FALSE) {
  .dq_df(before); .dq_df(after); .dq_cols(before, key); .dq_cols(after, key)
  .dq_flag(include_values, "include_values")
  .dq_key_compatible(before, key, after, key)
  kb <- .dq_key(before, key); ka <- .dq_key(after, key)
  if (anyDuplicated(kb) || anyDuplicated(ka) || any(!.dq_all_columns(before, key, function(x) !.dq_missing(x))) ||
      any(!.dq_all_columns(after, key, function(x) !.dq_missing(x)))) .dq_abort("Snapshot comparison requires complete, unique keys.")
  .dq_cols(before, fields); .dq_cols(after, fields)
  rows <- list()
  add <- function(kind, token, field, i, j, old, new) {
    oh <- .dq_hash(old); nh <- .dq_hash(new)
    cid <- .dq_hash(list(kind, token, field, oh, nh))
    rows[[length(rows) + 1L]] <<- data.frame(change_id = cid, change_type = kind,
      field = field, row_before = i, row_after = j, before_hash = oh, after_hash = nh,
      before_value = if (include_values) .dq_json(old) else NA_character_,
      after_value = if (include_values) .dq_json(new) else NA_character_, stringsAsFactors = FALSE)
  }
  for (nm in setdiff(names(before), names(after))) add("schema_removed", "schema", nm, NA_integer_, NA_integer_, class(before[[nm]]), NULL)
  for (nm in setdiff(names(after), names(before))) add("schema_added", "schema", nm, NA_integer_, NA_integer_, NULL, class(after[[nm]]))
  for (nm in intersect(names(before), names(after))) if (!identical(class(before[[nm]]), class(after[[nm]])))
    add("schema_type", "schema", nm, NA_integer_, NA_integer_, class(before[[nm]]), class(after[[nm]]))
  for (i in which(!kb %in% ka)) add("record_removed", kb[i], "__row__", i, NA_integer_, as.list(before[i, , drop = FALSE]), NULL)
  for (j in which(!ka %in% kb)) add("record_added", ka[j], "__row__", NA_integer_, j, NULL, as.list(after[j, , drop = FALSE]))
  for (i in which(kb %in% ka)) {
    j <- match(kb[i], ka)
    for (nm in fields) if (!identical(before[[nm]][i], after[[nm]][j]))
      add("value_changed", kb[i], nm, i, j, before[[nm]][i], after[[nm]][j])
  }
  out <- .dq_bind(rows, data.frame(change_id = character(), change_type = character(), field = character(),
    row_before = integer(), row_after = integer(), before_hash = character(), after_hash = character(),
    before_value = character(), after_value = character()))
  out$documented <- rep(FALSE, nrow(out))
  if (!is.null(approvals)) {
    .dq_df(approvals); .dq_cols(approvals, c("change_id", "approved_by", "evidence_uri", "evidence_sha256", "approved_at"))
    if (anyDuplicated(approvals$change_id)) .dq_abort("Duplicate change approvals.")
    valid <- !.dq_missing(approvals$approved_by) & !.dq_missing(approvals$evidence_uri) &
      grepl("^[a-fA-F0-9]{64}$", approvals$evidence_sha256) & !is.na(.dq_parse_time(approvals$approved_at))
    valid[is.na(valid)] <- FALSE
    out$documented <- out$change_id %in% approvals$change_id[valid]
  }
  out
}

dq_assess_controls <- function(controls, evidence, as_of = Sys.time(), max_age_days = 90,
                               require_independent = TRUE, verify_local_files = FALSE) {
  .dq_df(controls); .dq_cols(controls, c("control_id", "domain", "requirement", "severity"))
  .dq_df(evidence); .dq_cols(evidence, c("control_id", "outcome", "evidence_uri", "evidence_sha256", "reviewer", "producer", "reviewed_at", "rationale"))
  .dq_time(as_of); .dq_num1(max_age_days, "max_age_days", 0)
  .dq_flag(require_independent, "require_independent"); .dq_flag(verify_local_files, "verify_local_files")
  if (anyDuplicated(controls$control_id) || anyDuplicated(evidence$control_id)) .dq_abort("Controls and evidence must have one row per control_id.")
  rows <- lapply(seq_len(nrow(controls)), function(i) {
    id <- controls$control_id[i]; j <- match(id, evidence$control_id)
    status <- "not_evaluated"; detail <- "No review evidence supplied."
    if (!is.na(j)) {
      e <- evidence[j, , drop = FALSE]; time <- .dq_parse_time(e$reviewed_at)
      age <- as.numeric(difftime(as_of, time, units = "days"))
      incomplete <- any(vapply(e[c("evidence_uri", "evidence_sha256", "reviewer", "producer", "rationale")], function(z) any(.dq_missing(z)), logical(1)))
      if (incomplete || !grepl("^[a-fA-F0-9]{64}$", e$evidence_sha256) || is.na(age)) detail <- "Evidence metadata is incomplete or invalid."
      else if (age < 0 || age > max_age_days) detail <- "Review is future-dated or stale."
      else if (require_independent && identical(e$reviewer, e$producer)) detail <- "Independent review is required."
      else if (!e$outcome %in% c("pass", "fail", "not_applicable")) detail <- "Unsupported evidence outcome."
      else {
        status <- e$outcome; detail <- e$rationale
        if (verify_local_files) {
          uri <- e$evidence_uri
          if (grepl("^[A-Za-z]+://", uri)) { status <- "not_evaluated"; detail <- "Remote evidence is not fetched or authenticated by this function." }
          else if (!file.exists(uri)) { status <- "not_evaluated"; detail <- "Evidence file unavailable." }
          else if (!identical(tolower(e$evidence_sha256), digest::digest(file = uri, algo = "sha256"))) {
            status <- "fail"; detail <- "Evidence checksum does not match."
          }
        }
      }
    }
    .dq_audit_row(id, controls$domain[i], status, detail, severity = controls$severity[i])
  })
  .dq_bind(rows)
}

dq_audit_rule <- function(id, audit, required_controls, ...) {
  .dq_df(audit); .dq_cols(audit, c("control_id", "status"))
  if (!is.character(required_controls) || !length(required_controls)) .dq_abort("Explicit required_controls are necessary.")
  r <- dq_rule(id, function(data, context) {
    if (length(setdiff(required_controls, audit$control_id))) return(NA)
    s <- audit$status[audit$control_id %in% required_controls]
    if (any(s == "fail")) FALSE else if (all(s %in% c("pass", "not_applicable"))) TRUE else NA
  }, action = "block", scope = "dataset", ...)
  r$inputs_hash <- .dq_hash(list(audit, required_controls))
  r
}

dq_catalog <- function(type = c("controls", "standards", "tools", "checklist")) {
  type <- match.arg(type)
  path <- system.file("extdata", paste0(type, ".csv"), package = "dqguard")
  if (!nzchar(path)) .dq_abort("Catalogue unavailable; install the package before calling dq_catalog().")
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = "")
}
