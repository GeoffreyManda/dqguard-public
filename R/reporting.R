.dq_html_table <- function(data, max_rows = nrow(data)) {
  if (!nrow(data)) return("<p>No records in this section.</p>")
  data <- utils::head(data, max_rows)
  rows <- vapply(seq_len(nrow(data)), function(i) paste0("<tr>", paste0("<td>",
    vapply(data, function(x) .dq_escape(if (is.na(x[i])) "Not available" else as.character(x[i])), character(1)),
    "</td>", collapse = ""), "</tr>"), character(1))
  paste0("<div class='table-wrap'><table><thead><tr>", paste0("<th>", .dq_escape(names(data)), "</th>", collapse = ""),
    "</tr></thead><tbody>", paste0(rows, collapse = ""), "</tbody></table></div>")
}

dq_report <- function(result, path, max_findings = 200L, overwrite = FALSE) {
  if (!inherits(result, "dq_result")) .dq_abort("Expected a dq_result.")
  .dq_num1(max_findings, "max_findings", 0, .Machine$integer.max)
  m <- result$metrics; f <- result$findings
  full <- dq_gate(result, "full", error = FALSE); partition <- dq_gate(result, "partition", error = FALSE)
  doc <- paste0("<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<meta http-equiv='Content-Security-Policy' content=\"default-src 'none'; style-src 'unsafe-inline'\">",
    "<title>", .dq_escape(result$dataset), " | Data quality findings</title>",
    "<style>body{font-family:system-ui,sans-serif;max-width:1200px;margin:3rem auto;padding:0 1.5rem;line-height:1.6;color:#20252b}",
    "h1{font-size:2rem}h2{margin-top:2.5rem}table{border-collapse:collapse;width:100%;font-size:.9rem}",
    "td,th{text-align:left;vertical-align:top;padding:.65rem;border-bottom:1px solid #d7dce1}th{background:#f3f5f6}",
    ".table-wrap{overflow-x:auto}code{overflow-wrap:anywhere}.small{font-size:.85rem;color:#505b64}@media print{body{margin:0;max-width:none}}</style></head><body>",
    "<h1>", .dq_escape(result$dataset), ": data quality findings</h1>",
    "<p>", result$n_rows, " observations; ", result$n_columns, " columns; ", nrow(m), " rules.</p>",
    "<p><strong>Full snapshot release: ", if (full) "allowed by configured rules" else "blocked",
    ". Accepted-partition release: ", if (partition) "allowed by configured thresholds" else "blocked", ".</strong></p>",
    "<p>These decisions cover the evaluated rules only. They do not certify source accuracy, representative coverage, security or legal compliance.</p>",
    "<h2>Rule results and denominators</h2>", .dq_html_table(m[c("rule_id", "dimension", "severity", "status", "n_applicable", "n_evaluated", "n_excluded", "n_unknown", "n_failed", "fail_rate", "breached", "action", "owner")]),
    "<h2>Affected observations</h2><p>Showing ", min(nrow(f), max_findings), " of ", nrow(f),
    " findings. Exported machine-readable findings retain every affected row. Row numbers refer to the exact snapshot, not a later file.</p>",
    .dq_html_table(f[c("rule_id", "row", "record_id", "fields", "code", "message", "action")], max_findings),
    "<h2>Interpretation</h2><p>A failed check is distinct from a threshold breach. Unknown values follow each rule's declared policy. ",
    "Execution errors and unevaluated blocking rules hold release. Review-only signals do not establish fabrication or automatically remove records.</p>",
    "<p class='small'>Dataset snapshot: <code>", .dq_escape(result$data_hash), "</code><br>Rule set: <code>",
    .dq_escape(result$rule_hash), "</code><br>Evaluated at: ", .dq_escape(result$evaluated_at), "</p></body></html>")
  .dq_atomic_write(path, function(p) writeLines(doc, p, useBytes = TRUE), overwrite)
}

dq_export <- function(result, path, format = c("json", "csv", "html"),
                      component = c("findings", "metrics"), overwrite = FALSE) {
  if (!inherits(result, "dq_result")) .dq_abort("Expected a dq_result.")
  format <- match.arg(format); component <- match.arg(component)
  if (format == "html") return(dq_report(result, path, overwrite = overwrite))
  if (format == "csv") return(.dq_atomic_write(path, function(p) utils::write.csv(
    .dq_csv_safe(result[[component]]), p, row.names = FALSE, na = ""), overwrite))
  # State vectors and stable identities are not released wholesale in reports.
  public <- result[setdiff(names(result), c("states", "record_ids"))]
  public$serialization_notes <- "Unbounded max_fail_count is represented as null. Observed unavailable rates are also null; consult status and n_evaluated."
  .dq_atomic_write(path, function(p) jsonlite::write_json(public, p, auto_unbox = TRUE,
    pretty = TRUE, dataframe = "rows", na = "null", null = "null", digits = 16), overwrite)
}
