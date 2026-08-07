# Generate .xlsx benchmark/test fixtures by writing OOXML parts directly and
# packaging them with the system zip. No package dependencies.
# Usage: Rscript tools/make_fixtures.R [output_dir]   (default tools/fixtures)

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[[1]] else file.path("tools", "fixtures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(42)

NS <- 'xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"'
NSR <- 'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"'
DECL <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'

col_letter <- function(j) {  # 1-based -> "A", "Z", "AA", ...
  vapply(j, function(x) {
    s <- ""
    while (x > 0) { r <- (x - 1) %% 26; s <- paste0(LETTERS[r + 1], s); x <- (x - 1) %/% 26 }
    s
  }, "")
}

xml_esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

content_types <- function(sheets = 1L, sst = FALSE) {
  ov <- paste0('<Override PartName="/xl/worksheets/sheet', seq_len(sheets),
               '.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
               collapse = "")
  paste0(DECL,
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
    ov,
    '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
    if (sst) '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>' else "",
    '</Types>')
}

root_rels <- paste0(DECL,
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
  '</Relationships>')

workbook_xml <- function(sheet_names = "Sheet1", date1904 = FALSE) {
  sh <- paste0('<sheet name="', xml_esc(sheet_names), '" sheetId="', seq_along(sheet_names),
               '" r:id="rId', seq_along(sheet_names), '"/>', collapse = "")
  paste0(DECL, '<workbook ', NS, ' ', NSR, '>',
         if (date1904) '<workbookPr date1904="1"/>' else '<workbookPr/>',
         '<sheets>', sh, '</sheets></workbook>')
}

workbook_rels <- function(sheets = 1L, sst = FALSE) {
  n <- sheets
  rel <- paste0('<Relationship Id="rId', seq_len(n),
                '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet',
                seq_len(n), '.xml"/>', collapse = "")
  rel <- paste0(rel, '<Relationship Id="rId', n + 1,
                '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>')
  if (sst)
    rel <- paste0(rel, '<Relationship Id="rId', n + 2,
                  '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>')
  paste0(DECL, '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">', rel, '</Relationships>')
}

# xf 0: general, xf 1: builtin date (14), xf 2: custom datetime format (tests fmt_code_is_date)
styles_xml <- paste0(DECL, '<styleSheet ', NS, '>',
  '<numFmts count="1"><numFmt numFmtId="164" formatCode="yyyy\\-mm\\-dd\\ hh:mm"/></numFmts>',
  '<fonts count="1"><font/></fonts><fills count="1"><fill/></fills><borders count="1"><border/></borders>',
  '<cellStyleXfs count="1"><xf numFmtId="0"/></cellStyleXfs>',
  '<cellXfs count="3"><xf numFmtId="0"/><xf numFmtId="14" applyNumberFormat="1"/><xf numFmtId="164" applyNumberFormat="1"/></cellXfs>',
  '</styleSheet>')

sst_xml <- function(strings) {
  paste0(DECL, '<sst ', NS, ' count="', length(strings), '" uniqueCount="', length(strings), '">',
         paste0('<si><t>', xml_esc(strings), '</t></si>', collapse = ""),
         '</sst>')
}

sheet_xml <- function(dim_ref, rows) {
  paste0(DECL, '<worksheet ', NS, '><dimension ref="', dim_ref, '"/><sheetData>',
         paste0(rows, collapse = ""), '</sheetData></worksheet>')
}

# cols: list of per-column cell-body vectors (already "<c ...>...</c>" strings, "" = absent)
make_rows <- function(row_nums, cols) {
  do.call(paste0, c(list('<row r="', row_nums, '">'), cols, list('</row>')))
}

header_row_inline <- function(names, row = 1L, first_col = 1L) {
  refs <- paste0(col_letter(first_col + seq_along(names) - 1L), row)
  paste0('<row r="', row, '">',
         paste0('<c r="', refs, '" t="inlineStr"><is><t>', xml_esc(names), '</t></is></c>', collapse = ""),
         '</row>')
}

num_cells <- function(refs, vals) paste0('<c r="', refs, '"><v>', vals, '</v></c>')

write_xlsx_parts <- function(path, parts) {
  td <- tempfile("xlsxparts"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  for (nm in names(parts)) {
    f <- file.path(td, nm)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    con <- file(f, open = "wb")
    writeChar(parts[[nm]], con, eos = NULL, useBytes = TRUE)
    close(con)
  }
  path <- file.path(normalizePath(dirname(path), mustWork = TRUE), basename(path))
  if (file.exists(path)) unlink(path)
  old <- setwd(td); on.exit(setwd(old), add = TRUE)
  status <- utils::zip(path, files = ".", flags = "-r9Xq")
  if (status != 0) stop("zip failed for ", path)
  invisible(path)
}

base_parts <- function(sheet1, sheet_names = "Sheet1", sst = NULL, date1904 = FALSE) {
  n <- length(sheet_names)
  parts <- list(
    "[Content_Types].xml" = content_types(n, !is.null(sst)),
    "_rels/.rels" = root_rels,
    "xl/workbook.xml" = workbook_xml(sheet_names, date1904),
    "xl/_rels/workbook.xml.rels" = workbook_rels(n, !is.null(sst)),
    "xl/styles.xml" = styles_xml,
    "xl/worksheets/sheet1.xml" = sheet1)
  if (!is.null(sst)) parts[["xl/sharedStrings.xml"]] <- sst_xml(sst)
  parts
}

gen <- function(name, expr) {
  path <- file.path(out_dir, name)
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  cat(sprintf("%-18s %8.1f KB  (%.1fs)\n", name,
              file.info(path)$size / 1024, proc.time()[["elapsed"]] - t0))
}

fmt_num <- function(x) sprintf("%.15g", x)

## ---- numeric.xlsx: 100k x 20, all numeric --------------------------------
gen("numeric.xlsx", {
  nr <- 100000L; nc <- 20L
  rows <- 2:(nr + 1L)
  letters20 <- col_letter(1:nc)
  cols <- vector("list", nc)
  for (j in 1:nc) {
    v <- switch(1L + (j %% 4L),
      fmt_num(round(rnorm(nr) * 1000, 3)),          # short decimals
      as.character(sample.int(1000000L, nr, TRUE)), # integers
      fmt_num(runif(nr) * 1e6),                     # full precision
      as.character(sample.int(100L, nr, TRUE)))     # small ints
    cols[[j]] <- num_cells(paste0(letters20[j], rows), v)
  }
  s1 <- sheet_xml(paste0("A1:", letters20[nc], nr + 1L),
                  c(header_row_inline(paste0("num", 1:nc)), make_rows(rows, cols)))
  write_xlsx_parts(file.path(out_dir, "numeric.xlsx"), base_parts(s1))
})

## ---- sst.xlsx: 50k x 10 shared strings, 5k unique ------------------------
gen("sst.xlsx", {
  nr <- 50000L; nc <- 10L
  uniq <- c(paste0("Item ", 1:4995, " — ", sample(c("alpha", "beta", "café", "naïve", "gamma"), 4995, TRUE)),
            "a&b <tag> \"q\"", "  padded  ", "", "line1\nline2", "ümläut 中文")
  hdr_idx <- length(uniq) + seq_len(nc) - 1L        # header strings appended
  uniq <- c(uniq, paste0("col_", 1:nc))
  rows <- 2:(nr + 1L)
  cols <- vector("list", nc)
  for (j in 1:nc) {
    idx <- sample.int(5000L, nr, TRUE) - 1L
    cols[[j]] <- paste0('<c r="', col_letter(j), rows, '" t="s"><v>', idx, '</v></c>')
  }
  hdr <- paste0('<row r="1">',
                paste0('<c r="', col_letter(1:nc), '1" t="s"><v>', hdr_idx, '</v></c>', collapse = ""),
                '</row>')
  s1 <- sheet_xml(paste0("A1:", col_letter(nc), nr + 1L), c(hdr, make_rows(rows, cols)))
  write_xlsx_parts(file.path(out_dir, "sst.xlsx"), base_parts(s1, sst = uniq))
})

## ---- inline.xlsx: 20k x 5 inline strings ---------------------------------
gen("inline.xlsx", {
  nr <- 20000L; nc <- 5L
  pool <- paste0("inline value ", 1:100)
  rows <- 2:(nr + 1L)
  cols <- vector("list", nc)
  for (j in 1:nc) {
    s <- sample(pool, nr, TRUE)
    cols[[j]] <- paste0('<c r="', col_letter(j), rows, '" t="inlineStr"><is><t>',
                        xml_esc(s), '</t></is></c>')
  }
  s1 <- sheet_xml(paste0("A1:", col_letter(nc), nr + 1L),
                  c(header_row_inline(paste0("s", 1:nc)), make_rows(rows, cols)))
  write_xlsx_parts(file.path(out_dir, "inline.xlsx"), base_parts(s1))
})

## ---- mixed.xlsx: 100k x 15 mixed types -----------------------------------
gen("mixed.xlsx", {
  nr <- 100000L; nc <- 15L
  uniq <- paste0("cat_", 1:200)
  rows <- 2:(nr + 1L)
  cols <- vector("list", nc)
  ref <- function(j) paste0(col_letter(j), rows)
  for (j in 1:4) cols[[j]] <- num_cells(ref(j), fmt_num(round(rnorm(nr) * 100, 4)))
  for (j in 5:6) cols[[j]] <- num_cells(ref(j), as.character(sample.int(100000L, nr, TRUE)))
  for (j in 7:9) {
    idx <- sample.int(200L, nr, TRUE) - 1L
    cols[[j]] <- paste0('<c r="', ref(j), '" t="s"><v>', idx, '</v></c>')
  }
  cols[[10]] <- paste0('<c r="', ref(10), '" s="1"><v>', 40000L + sample.int(2000L, nr, TRUE), '</v></c>')
  cols[[11]] <- paste0('<c r="', ref(11), '" s="2"><v>', fmt_num(40000 + runif(nr) * 2000), '</v></c>')
  cols[[12]] <- paste0('<c r="', ref(12), '" t="b"><v>', sample(0:1, nr, TRUE), '</v></c>')
  stray <- runif(nr) < 0.01
  c13 <- num_cells(ref(13), fmt_num(round(runif(nr) * 1e4, 2)))
  c13[stray] <- paste0('<c r="', ref(13)[stray], '" t="s"><v>', sample.int(200L, sum(stray), TRUE) - 1L, '</v></c>')
  cols[[13]] <- c13
  c14 <- num_cells(ref(14), as.character(sample.int(50L, nr, TRUE)))
  c14[runif(nr) < 0.9] <- ""
  cols[[14]] <- c14
  cols[[15]] <- num_cells(ref(15), fmt_num(rnorm(nr)))
  hdr_idx <- length(uniq) + seq_len(nc) - 1L
  uniq <- c(uniq, paste0("mix_", 1:nc))
  hdr <- paste0('<row r="1">',
                paste0('<c r="', col_letter(1:nc), '1" t="s"><v>', hdr_idx, '</v></c>', collapse = ""),
                '</row>')
  s1 <- sheet_xml(paste0("A1:", col_letter(nc), nr + 1L), c(hdr, make_rows(rows, cols)))
  write_xlsx_parts(file.path(out_dir, "mixed.xlsx"), base_parts(s1, sst = uniq))
})

## ---- wide.xlsx: 1k x 500 numeric -----------------------------------------
gen("wide.xlsx", {
  nr <- 1000L; nc <- 500L
  rows <- 2:(nr + 1L)
  lets <- col_letter(1:nc)
  cols <- vector("list", nc)
  for (j in 1:nc)
    cols[[j]] <- num_cells(paste0(lets[j], rows), as.character(sample.int(10000L, nr, TRUE)))
  s1 <- sheet_xml(paste0("A1:", lets[nc], nr + 1L),
                  c(header_row_inline(paste0("w", 1:nc)), make_rows(rows, cols)))
  write_xlsx_parts(file.path(out_dir, "wide.xlsx"), base_parts(s1))
})

## ---- tiny.xlsx: 100 x 10 -------------------------------------------------
gen("tiny.xlsx", {
  nr <- 100L; nc <- 10L
  rows <- 2:(nr + 1L)
  cols <- vector("list", nc)
  for (j in 1:5) cols[[j]] <- num_cells(paste0(col_letter(j), rows), fmt_num(round(rnorm(nr), 3)))
  for (j in 6:10) {
    s <- paste0("t", sample.int(20L, nr, TRUE))
    cols[[j]] <- paste0('<c r="', col_letter(j), rows, '" t="inlineStr"><is><t>', s, '</t></is></c>')
  }
  s1 <- sheet_xml(paste0("A1:", col_letter(nc), nr + 1L),
                  c(header_row_inline(paste0("c", 1:nc)), make_rows(rows, cols)))
  write_xlsx_parts(file.path(out_dir, "tiny.xlsx"), base_parts(s1))
})

## ---- edge.xlsx: handcrafted corner cases ---------------------------------
gen("edge.xlsx", {
  body <- paste0(
    header_row_inline(c("name", "txt", "ws", "flag", "when", "err"), row = 3L, first_col = 3L),
    '<row r="4">',
      '<c r="C4"><v>1.5</v></c>',
      '<c r="D4" t="str"><v>a&amp;b</v></c>',
      '<c r="E4" t="str"><v>   </v></c>',
      '<c r="F4" t="b"><v>1</v></c>',
      '<c r="G4" s="1"><v>61</v></c>',
      '<c r="H4" t="e"><v>#DIV/0!</v></c>',
    '</row>',
    '<row>',                                     # no r=: implied row 5
      '<c r="C5"><v>2</v></c>',
      '<c t="str"><v>x</v></c>',                 # no r=: implied col D
    '</row>',
    '<row r="6"><c r="C6"/><c r="D6" s="1"/></row>',
    '<row r="7"><c r="D7" t="str"><v>&lt;A&gt; &quot;q&quot; &apos;a&apos; &#65;&#x42;</v></c></row>',
    '<row r="8"><c r="G8" s="1"><v>30</v></c></row>',
    '<row r="12"><c r="H12"><v>99</v></c></row>')
  s1 <- sheet_xml("C3:H12", body)
  write_xlsx_parts(file.path(out_dir, "edge.xlsx"), base_parts(s1))
})

## ---- date1904.xlsx -------------------------------------------------------
gen("date1904.xlsx", {
  body <- paste0(
    header_row_inline("d", row = 1L),
    '<row r="2"><c r="A2" s="1"><v>0</v></c></row>',
    '<row r="3"><c r="A3" s="1"><v>100</v></c></row>')
  s1 <- sheet_xml("A1:A3", body)
  write_xlsx_parts(file.path(out_dir, "date1904.xlsx"), base_parts(s1, date1904 = TRUE))
})

## ---- multisheet.xlsx: 3 sheets -------------------------------------------
gen("multisheet.xlsx", {
  mk <- function(v) sheet_xml("A1:A2", paste0(header_row_inline("x"),
                                              '<row r="2"><c r="A2"><v>', v, '</v></c></row>'))
  parts <- base_parts(mk(1), sheet_names = c("Alpha", "Beta", "Gamma"))
  parts[["xl/worksheets/sheet2.xml"]] <- mk(2)
  parts[["xl/worksheets/sheet3.xml"]] <- mk(3)
  write_xlsx_parts(file.path(out_dir, "multisheet.xlsx"), parts)
})

cat("fixtures written to ", out_dir, "\n", sep = "")
