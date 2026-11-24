.dq_audit_row <- function(control_id, domain, status, detail, subject = "", severity = "error")
  data.frame(control_id = control_id, domain = domain, subject = subject, status = status,
    severity = severity, detail = detail, stringsAsFactors = FALSE)

.dq_zip_xml <- function(path, member, limit) {
  .dq_need("xml2")
  con <- unz(path, member, open = "rb"); on.exit(close(con), add = TRUE)
  bytes <- readBin(con, "raw", n = as.integer(limit + 1))
  if (length(bytes) > limit) .dq_abort("XML member exceeds configured limit.")
  text <- rawToChar(bytes)
  if (grepl("<!DOCTYPE|<!ENTITY", text, ignore.case = TRUE)) .dq_abort("DTD/entity declarations are not accepted.")
  xml2::read_xml(text, options = "NONET")
}
.dq_attr <- function(node, name) xml2::xml_find_chr(node, paste0("string(@*[local-name()='", name, "'])"))
.dq_member <- function(base, target) {
  if (grepl("^[A-Za-z]+:|\\\\", target)) .dq_abort("External or invalid package relationship.")
  parts <- strsplit(if (startsWith(target, "/")) substring(target, 2) else paste0(base, "/", target), "/", fixed = TRUE)[[1]]
  out <- character()
  for (p in parts) if (p == "..") {
    if (!length(out)) .dq_abort("Relationship escapes the package root.")
    out <- head(out, -1)
  } else if (nzchar(p) && p != ".") out <- c(out, p)
  paste(out, collapse = "/")
}

# One row per cell: row and column number, cell type, style index and displayed value.
.dq_sheet_cells <- function(sheet_xml, shared) {
  cs <- xml2::xml_find_all(sheet_xml, ".//*[local-name()='sheetData']/*[local-name()='row']/*[local-name()='c']")
  if (!length(cs)) return(data.frame(row = integer(), col = integer(), type = character(), style = integer(), value = character()))
  # Cell attributes carry no namespace, so the vectorised accessor reads them directly.
  ref <- xml2::xml_attr(cs, "r"); type <- xml2::xml_attr(cs, "t", default = "")
  style <- suppressWarnings(as.integer(xml2::xml_attr(cs, "s", default = "0"))); style[is.na(style)] <- 0L
  # Node-set queries run once for the whole set; per-node string() queries rebuild the namespace table each time.
  v <- xml2::xml_text(xml2::xml_find_first(cs, "./*[local-name()='v']")); v[is.na(v)] <- ""
  inl <- xml2::xml_text(xml2::xml_find_first(cs, "./*[local-name()='is']")); inl[is.na(inl)] <- ""
  si <- suppressWarnings(as.integer(v)) + 1L
  value <- ifelse(type == "s", ifelse(!is.na(si) & si >= 1 & si <= length(shared), shared[pmax(1L, pmin(length(shared), si))], ""),
                  ifelse(type == "inlineStr", inl, v))
  letters_part <- toupper(sub("[0-9]+$", "", ref)); ul <- unique(letters_part)
  idx <- vapply(strsplit(ul, ""), function(ch) sum((match(ch, LETTERS)) * 26^(rev(seq_along(ch)) - 1)), numeric(1))
  col <- idx[match(letters_part, ul)]
  data.frame(row = as.integer(sub("^[A-Za-z]+", "", ref)), col = as.integer(col), type = type, style = style, value = value, stringsAsFactors = FALSE)
}

# Style indices whose fill is a solid pattern (highlighted cells).
.dq_filled_styles <- function(path, names_in, max_bytes) {
  if (!"xl/styles.xml" %in% names_in) return(integer())
  st <- .dq_zip_xml(path, "xl/styles.xml", max_bytes)
  fills <- xml2::xml_find_all(st, ".//*[local-name()='fills']/*[local-name()='fill']")
  solid <- vapply(fills, function(f) identical(xml2::xml_find_chr(f, "string(./*[local-name()='patternFill']/@patternType)"), "solid"), logical(1))
  xf <- xml2::xml_find_all(st, ".//*[local-name()='cellXfs']/*[local-name()='xf']")
  fid <- suppressWarnings(as.integer(vapply(xf, .dq_attr, character(1), "fillId")))
  which(!is.na(fid) & fid + 1L <= length(solid) & solid[pmax(1L, fid + 1L)]) - 1L
}

dq_file_audit <- function(path, filename_pattern = NULL, sheets = NULL, header_row = 1L,
                          max_bytes = 100 * 1024^2, max_members = 10000L) {
  .dq_chr1(path, "path"); .dq_num1(max_bytes, "max_bytes", 1, .Machine$integer.max - 1)
  .dq_num1(max_members, "max_members", 1); .dq_num1(header_row, "header_row", 1, 1048576)
  if (header_row != floor(header_row)) .dq_abort("header_row must be an integer.")
  subject <- basename(path); out <- list()
  add <- function(id, domain, status, detail, sub = subject, severity = "error") {
    out[[length(out) + 1L]] <<- .dq_audit_row(id, domain, status, detail, sub, severity)
  }
  readable <- file.exists(path) && !dir.exists(path) && file.access(path, 4) == 0
  add("file.readable", "access", if (readable) "pass" else "fail", if (readable) "File can be read." else "File is unavailable or unreadable.")
  if (!readable) return(.dq_bind(out))
  size <- file.info(path)$size
  add("file.size_limit", "structure", if (size <= max_bytes) "pass" else "not_evaluated", paste("Bytes:", size))
  if (size > max_bytes) return(.dq_bind(out))
  add("file.name", "naming", if (is.null(filename_pattern)) "not_evaluated" else
    if (grepl(filename_pattern, subject, perl = TRUE)) "pass" else "fail",
    if (is.null(filename_pattern)) "No approved naming pattern supplied." else "Compared with the supplied naming pattern.")
  add("file.encryption_access_policy", "access", "not_evaluated", "Readability or worksheet protection does not establish encryption, ACL correctness or authorized access.")
  add("file.snapshot", "provenance", "pass", paste0("SHA256:", digest::digest(file = path, algo = "sha256")), severity = "info")
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    if (ext == "csv") {
      widths <- utils::count.fields(path, sep = ",", quote = '"', blank.lines.skip = FALSE, comment.char = "")
      known <- widths[!is.na(widths) & widths > 0L]
      if (!length(known) || any(known != known[1])) .dq_abort("CSV records do not match the header width; refusing implicit row-name shifting.")
      d <- utils::read.csv(path, check.names = FALSE, colClasses = "character", na.strings = NULL,
        blank.lines.skip = FALSE, fill = FALSE, stringsAsFactors = FALSE, comment.char = "")
      add("columns.unique", "structure", if (!anyDuplicated(names(d))) "pass" else "fail", "Original header names retained.")
      add("columns.named", "structure", if (length(names(d)) && all(nzchar(trimws(names(d))))) "pass" else "fail", "Checked empty column names.")
      blank <- if (ncol(d)) Reduce(`&`, lapply(d, .dq_missing)) else rep(TRUE, nrow(d))
      add("structure.blank_rows", "structure", if (any(blank)) "fail" else "pass", paste("Blank observations:", sum(blank)))
      add("structure.csv_parsed", "structure", "pass", paste(nrow(d), "observations parsed without padded short rows."))
    } else if (ext %in% c("xlsx", "xlsm")) {
      .dq_need("xml2")
      members <- utils::unzip(path, list = TRUE)
      if (nrow(members) > max_members || sum(members$Length) > max_bytes ||
          anyDuplicated(members$Name) || any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|\\\\)", members$Name)))
        .dq_abort("Unsafe archive layout or decompression budget exceeded.")
      names_in <- members$Name
      if (!all(c("xl/workbook.xml", "xl/_rels/workbook.xml.rels") %in% names_in)) .dq_abort("Required workbook parts are missing.")
      read <- function(member) {
        if (!member %in% names_in) .dq_abort(paste("Missing package member:", member))
        .dq_zip_xml(path, member, max_bytes)
      }
      wb <- read("xl/workbook.xml"); rel <- read("xl/_rels/workbook.xml.rels")
      nodes <- xml2::xml_find_all(wb, ".//*[local-name()='sheet']")
      rels <- xml2::xml_find_all(rel, ".//*[local-name()='Relationship']")
      shared <- character()
      if ("xl/sharedStrings.xml" %in% names_in) {
        sx <- read("xl/sharedStrings.xml")
        # Phonetic guide runs are not part of the displayed text; drop them, then read every entry in one call.
        xml2::xml_remove(xml2::xml_find_all(sx, ".//*[local-name()='rPh']"))
        shared <- xml2::xml_text(xml2::xml_find_all(sx, ".//*[local-name()='si']"))
      }
      available <- vapply(nodes, .dq_attr, character(1), "name")
      if (!is.null(sheets) && any(!sheets %in% available)) .dq_abort("A requested worksheet is missing.")
      if (!length(nodes)) .dq_abort("Workbook contains no worksheets.")
      for (node in nodes) {
        sheet <- .dq_attr(node, "name")
        if (!is.null(sheets) && !sheet %in% sheets) next
        state <- .dq_attr(node, "state")
        add("xlsx.visible_sheet", "structure", if (state %in% c("hidden", "veryHidden")) "fail" else "pass", paste("Sheet visibility:", state), sheet)
        rid <- .dq_attr(node, "id")
        idx <- which(vapply(rels, .dq_attr, character(1), "Id") == rid)
        if (length(idx) != 1L) .dq_abort("Worksheet relationship is missing or duplicated.")
        rr <- rels[[idx]]
        if (.dq_attr(rr, "TargetMode") == "External") .dq_abort("External worksheet relationships are not followed.")
        part <- .dq_member("xl", .dq_attr(rr, "Target")); sheet_xml <- read(part)
        count <- function(xpath) length(xml2::xml_find_all(sheet_xml, xpath))
        checks <- list(
          "xlsx.no_merged_cells" = ".//*[local-name()='mergeCell']",
          "xlsx.no_hidden_rows" = ".//*[local-name()='row'][@hidden='1' or @hidden='true']",
          "xlsx.no_hidden_columns" = ".//*[local-name()='col'][@hidden='1' or @hidden='true']",
          "xlsx.no_active_filters" = ".//*[local-name()='filterColumn'] | .//*[local-name()='sheetPr'][@filterMode='1' or @filterMode='true']",
          "xlsx.no_error_cells" = ".//*[local-name()='c'][@t='e']")
        for (id in names(checks)) {
          num <- count(checks[[id]])
          add(id, "structure", if (num) "fail" else "pass", paste("Matching XML elements:", num), sheet)
        }
        formula_n <- count(".//*[local-name()='c'][*[local-name()='f']]")
        add("xlsx.formulas", "structure", if (formula_n) "needs_review" else "pass", paste("Formula cells:", formula_n, "Cached results are not recalculated."), sheet, "warning")
        # Filter buttons left on a range hide nothing until criteria are applied; report them separately.
        af <- xml2::xml_find_all(sheet_xml, ".//*[local-name()='autoFilter']")
        add("xlsx.saved_filter_buttons", "structure", if (length(af)) "needs_review" else "pass",
          if (length(af)) paste("Filter buttons saved on", paste(vapply(af, .dq_attr, character(1), "ref"), collapse = ", ")) else "No saved filter range.", sheet, "warning")
        cells <- .dq_sheet_cells(sheet_xml, shared)
        body <- cells[cells$row > header_row, , drop = FALSE]
        if (nrow(cells)) {
          # Every column that holds data below the header needs a header cell.
          hdr_cols <- cells$col[cells$row == header_row & nzchar(trimws(cells$value))]
          data_cols <- sort(unique(body$col[nzchar(trimws(body$value))]))
          unnamed <- setdiff(data_cols, hdr_cols)
          add("columns.named_positions", "structure", if (length(unnamed)) "fail" else "pass",
            paste("Columns holding data without a header cell:", length(unnamed)), sheet)
          keep <- nzchar(trimws(body$value))
          by_row <- split(body$value[keep], body$row[keep])
          sep <- as.integer(names(by_row)[vapply(by_row, function(v) length(v) >= 3L && length(unique(trimws(v))) == 1L, logical(1))])
          add("structure.separator_rows", "structure", if (length(sep)) "fail" else "pass",
            paste0("Rows repeating one value across every populated cell: ", length(sep), if (length(sep)) paste0(" (rows ", paste(head(sep, 10), collapse = ", "), ")") else ""), sheet)
          lab <- vapply(by_row, function(v) v[1], character(1))
          summ <- as.integer(names(by_row)[grepl("^\\s*(total|sub-?total|grand total|sum|average|mean)\\b", lab, ignore.case = TRUE)])
          add("structure.summary_rows", "structure", if (length(summ)) "fail" else "pass", paste("Rows labelled as totals or averages:", length(summ)), sheet)
          txt <- body[body$type %in% c("s", "str", "inlineStr"), , drop = FALSE]
          num_txt <- sum(grepl("^\\s*-?[0-9]+(\\.[0-9]+)?\\s*$", txt$value))
          add("format.numbers_as_text", "format", if (num_txt) "fail" else "pass", paste("Numeric values stored as text:", num_txt), sheet)
          date_txt <- sum(grepl("^\\s*[0-9]{1,4}[/.-][0-9]{1,2}[/.-][0-9]{1,4}", txt$value))
          add("format.dates_as_text", "format", if (date_txt) "fail" else "pass", paste("Date values stored as text:", date_txt), sheet)
          ws <- sum(nzchar(txt$value) & grepl("^\\s|\\s$", txt$value))
          add("format.text_whitespace", "format", if (ws) "fail" else "pass", paste("Text cells with leading or trailing spaces:", ws), sheet)
          filled <- .dq_filled_styles(path, names_in, max_bytes)
          hl <- sum(body$style %in% filled & nzchar(body$value))
          add("processing.highlighted_cells", "processing", if (hl) "needs_review" else "pass",
            paste("Data cells with a solid fill (manual edit signal without a recorded reason):", hl), sheet, "warning")
        }
        # Header values from the cell table read once above; per-cell queries are slow on wide header rows.
        hc <- cells[cells$row == header_row, , drop = FALSE]
        h <- hc$value[order(hc$col)]
        h[hc$type[order(hc$col)] == "s" & !nzchar(h)] <- NA_character_
        add("columns.unique", "structure", if (length(h) && !anyNA(h) && !anyDuplicated(h)) "pass" else "fail", "Checked original header cell values.", sheet)
        add("columns.named", "structure", if (length(h) && !anyNA(h) && all(nzchar(trimws(h)))) "pass" else "fail", "Checked populated header cells; missing header positions require dictionary alignment after import.", sheet)
        populated <- xml2::xml_find_all(sheet_xml, ".//*[local-name()='row'][*[local-name()='c']/*[local-name()='v' or local-name()='is' or local-name()='f']]")
        row_nums <- suppressWarnings(as.numeric(xml2::xml_attr(populated, "r")))
        if (anyNA(row_nums) || any(row_nums < 1 | row_nums > 1048576 | row_nums != floor(row_nums)) || anyDuplicated(row_nums))
          .dq_abort("Invalid or repeated worksheet row coordinates.")
        row_nums <- row_nums[row_nums >= header_row]
        gaps <- if (length(row_nums)) length(setdiff(seq.int(header_row, max(row_nums)), row_nums)) else 0L
        add("structure.blank_rows", "structure", if (gaps) "fail" else "pass", paste("Unpopulated row positions between header and last populated row:", gaps), sheet)
      }
      table_parts <- grep("^xl/tables/[^/]+\\.xml$", names_in, value = TRUE)
      for (tp in table_parts) {
        tab <- read(tp); nfilters <- length(xml2::xml_find_all(tab, ".//*[local-name()='filterColumn']"))
        add("xlsx.table_filters", "structure", if (nfilters) "fail" else "pass", paste("Table filter criteria:", nfilters), tp)
      }
      add("xlsx.external_links", "provenance", if (any(grepl("^xl/externalLinks/", names_in))) "needs_review" else "pass", "External links are detected, never fetched.", severity = "warning")
      add("xlsx.macros", "security", if (any(grepl("vbaProject", names_in, ignore.case = TRUE))) "needs_review" else "pass", "Macro parts are detected, never executed.", severity = "warning")
    } else add("file.format_supported", "structure", "not_evaluated", "Raw structural inspection supports CSV and unencrypted XLSX/XLSM; other formats require an appropriate reader.")
  }, error = function(e) add("file.inspection", "structure", "not_evaluated", conditionMessage(e)))
  .dq_bind(out)
}
