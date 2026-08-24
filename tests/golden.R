# Correctness net for rcxl. Three layers:
#   1. hand-written asserts on handcrafted fixtures (edge/date1904/multisheet)
#   2. snapshot comparison against tools/golden/*.rds (captured at baseline)
#   3. tolerant cross-check against readxl on the bulk fixtures
# In the dev tree, fixtures come from tools/fixtures (run tools/make_fixtures.R).
# Elsewhere (R CMD check on the built tarball) the small fixtures shipped in
# inst/extdata are used and sections needing the generated ones skip.

find_root <- function() {
  for (d in c(".", "..", "../..")) {
    if (dir.exists(file.path(d, "tools", "fixtures"))) return(normalizePath(d))
  }
  NULL
}
root <- find_root()
if (!is.null(root)) {
  fixdir <- file.path(root, "tools", "fixtures")
  lib <- file.path(root, "tools", "lib")
  if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
} else {
  fixdir <- system.file("extdata", package = "rcxl")
  if (fixdir == "") { cat("fixtures not found; skipping golden tests\n"); quit(save = "no") }
}
library(rcxl)

fails <- 0L
check <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); fails <<- fails + 1L }
}
utc <- function(s) as.POSIXct(s, tz = "UTC")
fx <- function(name) file.path(fixdir, paste0(name, ".xlsx"))

## ---- 1. hand-written asserts --------------------------------------------

e <- read_xlsx(fx("edge"))
check(identical(names(e), c("name", "txt", "ws", "flag", "when", "err")), "edge names")
check(nrow(e) == 9, "edge nrow")
check(identical(e$name, c(1.5, 2, rep(NA_real_, 7))), "edge$name")
check(identical(e$txt, c("a&b", "x", NA, "<A> \"q\" 'a' AB", rep(NA_character_, 5))), "edge$txt")
check(is.logical(e$ws) && all(is.na(e$ws)), "edge$ws all-NA logical")
check(identical(e$flag, c(TRUE, rep(NA, 8))), "edge$flag")
check(inherits(e$when, "POSIXct"), "edge$when POSIXct")
check(identical(as.numeric(e$when), as.numeric(c(utc("1900-03-01"), NA, NA, NA, utc("1900-01-30"), rep(NA, 4)))),
      "edge$when values (incl. pre-1900 adjustment)")
check(identical(e$err, c(rep(NA_real_, 8), 99)), "edge$err")

d <- read_xlsx(fx("date1904"))
check(identical(as.numeric(d$d), as.numeric(c(utc("1904-01-01"), utc("1904-04-10")))), "date1904 values")

check(identical(xlsx_sheets(fx("multisheet")), c("Alpha", "Beta", "Gamma")), "sheet names")
check(read_xlsx(fx("multisheet"), sheet = 2)$x == 2, "sheet by index")
check(read_xlsx(fx("multisheet"), sheet = "Gamma")$x == 3, "sheet by name")

tw <- read_xlsx(fx("edge"), trim_ws = FALSE)
check(identical(tw$txt[1], "a&b"), "trim_ws=FALSE basic")
nh <- read_xlsx(fx("tiny"), col_names = FALSE)
check(nrow(nh) == 101, "col_names=FALSE keeps header row")

## ---- 2. snapshot comparison ---------------------------------------------

golddir <- if (is.null(root)) "" else file.path(root, "tools", "golden")
if (dir.exists(golddir)) {
  for (rds in list.files(golddir, pattern = "\\.rds$", full.names = TRUE)) {
    name <- sub("\\.rds$", "", basename(rds))
    if (!file.exists(fx(name))) next
    got <- read_xlsx(fx(name))
    want <- readRDS(rds)
    ok <- isTRUE(all.equal(got, want, tolerance = 1e-12, check.attributes = TRUE))
    check(ok, paste0("snapshot ", name))
    if (!ok) print(all.equal(got, want, tolerance = 1e-12))
  }
} else cat("note: no golden snapshots (tools/golden); run tools/make_golden.R at a known-good build\n")

## ---- 3. tolerant cross-check vs readxl ----------------------------------

if (requireNamespace("readxl", quietly = TRUE)) {
  cmp_col <- function(a, b, what) {
    if (inherits(a, "POSIXct")) a <- as.numeric(a)
    if (inherits(b, "POSIXct")) b <- as.numeric(b)
    if (is.logical(a) && is.numeric(b)) a <- as.numeric(a)
    if (is.logical(b) && is.numeric(a)) b <- as.numeric(b)
    check(identical(is.na(a), is.na(b)), paste(what, "NA pattern"))
    i <- !is.na(a)
    if (is.numeric(a) && is.numeric(b)) {
      check(isTRUE(all.equal(a[i], b[i], tolerance = 1e-9)), paste(what, "numeric values"))
    } else if (is.character(a) && is.character(b)) {
      check(identical(a[i], b[i]), paste(what, "character values"))
    } else {
      # type mismatch: compare through character with numeric normalization
      an <- suppressWarnings(as.numeric(as.character(a[i])))
      bn <- suppressWarnings(as.numeric(as.character(b[i])))
      both_num <- !is.na(an) & !is.na(bn)
      check(isTRUE(all.equal(an[both_num], bn[both_num], tolerance = 1e-9)) &&
              identical(as.character(a[i])[!both_num], as.character(b[i])[!both_num]),
            paste(what, "cross-type values"))
    }
  }
  for (name in c("tiny", "numeric", "sst", "inline", "wide")) {
    if (!file.exists(fx(name))) next
    got <- read_xlsx(fx(name))
    ref <- as.data.frame(suppressMessages(readxl::read_excel(fx(name), progress = FALSE)))
    check(identical(dim(got), dim(ref)), paste(name, "dims"))
    check(identical(names(got), names(ref)), paste(name, "names"))
    if (identical(dim(got), dim(ref)))
      for (j in seq_along(got)) cmp_col(got[[j]], ref[[j]], paste0(name, "[", j, "]"))
  }
} else cat("note: readxl not available; skipping cross-check\n")

## ---- 4. range / skip / n_max --------------------------------------------

if (requireNamespace("readxl", quietly = TRUE)) {
  # value parity ignoring naming conventions (rcxl V1 vs readxl ...1) and
  # readxl's millisecond rounding of POSIXct
  cmp_df <- function(a, b, what) {
    check(identical(dim(a), dim(b)), paste(what, "dims"))
    if (!identical(dim(a), dim(b))) return(invisible())
    for (j in seq_along(a)) {
      x <- a[[j]]; y <- b[[j]]
      check(identical(inherits(x, "POSIXct"), inherits(y, "POSIXct")),
            paste0(what, "[", j, "] date typing"))
      if (inherits(x, "POSIXct")) { x <- as.numeric(x); y <- as.numeric(y) }
      if (is.numeric(x) && is.numeric(y)) {
        check(isTRUE(all.equal(x, y, tolerance = 1e-6)), paste0(what, "[", j, "] values"))
      } else
        check(identical(as.character(x), as.character(y)), paste0(what, "[", j, "] values"))
    }
  }
  rdx <- function(...) as.data.frame(suppressMessages(
    readxl::read_excel(..., progress = FALSE)))
  lim <- cellranger::cell_limits

  m <- fx("mixed")
  if (file.exists(m)) {
    cmp_df(read_xlsx(m, range = "B3:D10"), rdx(m, range = "B3:D10"), "range rect")
    cmp_df(read_xlsx(m, skip = 5), rdx(m, skip = 5), "skip")
    cmp_df(read_xlsx(m, n_max = 7), rdx(m, n_max = 7), "n_max")
    cmp_df(read_xlsx(m, n_max = 0), rdx(m, n_max = 0), "n_max=0")
    cmp_df(read_xlsx(m, skip = 3, n_max = 5, col_names = FALSE),
           rdx(m, skip = 3, n_max = 5, col_names = FALSE), "skip+n_max")
    cmp_df(read_xlsx(m, range = "B:C"),
           rdx(m, range = lim(c(NA, 2), c(NA, 3))), "col-only range")
    cmp_df(read_xlsx(m, range = "3:10", col_names = FALSE),
           rdx(m, range = lim(c(3, NA), c(10, NA)), col_names = FALSE),
           "row-only range")
    cmp_df(read_xlsx(m, range = "B3"), rdx(m, range = "B3"), "single cell")
  }

  t <- fx("tiny")
  cmp_df(read_xlsx(t, range = "A99:C110"), rdx(t, range = "A99:C110"),
         "range past rows")
  cmp_df(read_xlsx(t, range = "A99:E103"), rdx(t, range = "A99:E103"),
         "range past cols")
  # divergence from readxl: a range with no data at all keeps its rectangle
  # (all NA) where readxl collapses to 0x0
  pd <- read_xlsx(t, range = "A200:C205")
  check(identical(dim(pd), c(5L, 3L)) &&
          all(vapply(pd, function(x) all(is.na(x)), TRUE)),
        "fully-past-data range keeps its rectangle")

  ms <- fx("multisheet")
  check(read_xlsx(ms, range = "Beta!A1:A2")$x == 2, "sheet-qualified range")
  check(read_xlsx(ms, range = "'Beta'!A1:A2")$x == 2, "quoted sheet range")

  e <- fx("edge")
  cmp_df(read_xlsx(e, range = "F2:F4"), rdx(e, range = "F2:F4"),
         "date typing through range")

  al <- read_xlsx_all(ms, skip = 1, col_names = FALSE)
  check(all(vapply(al, nrow, 0L) == 1) && al$Beta[[1]] == 2,
        "read_xlsx_all skip")

  al <- read_xlsx_all(ms, col_names = "y")
  check(all(vapply(al, names, "") == "y") && al$Beta$y[[1]] == "x",
        "read_xlsx_all character col_names")

  nm15 <- paste0("n", 1:15)
  if (file.exists(m)) {
    cmp_df(read_xlsx(m, col_names = nm15), rdx(m, col_names = nm15),
           "character col_names")
    cn <- read_xlsx(m, col_names = nm15)
    check(identical(names(cn), nm15), "character col_names applied")
    cn <- read_xlsx(m, col_names = nm15, n_max = 3)
    check(identical(names(cn), nm15) && nrow(cn) == 3,
          "col_names + n_max keeps header row as data")
    cn <- read_xlsx(m, col_names = nm15,
                    col_types = c("skip", rep("guess", 14)))
    check(identical(names(cn), nm15[-1]), "skipped column's name is dropped")
    cn <- read_xlsx(m, range = "B3:D10", col_names = c("p", "q", "r"))
    check(identical(names(cn), c("p", "q", "r")) && nrow(cn) == 8,
          "col_names sized to the range")
    cn <- read_xlsx(m, col_names = rep("a", 15))
    check(identical(names(cn)[1:3], c("a", "a_1", "a_2")),
          "duplicate col_names deduplicated")
    check(inherits(try(read_xlsx(m, col_names = c("a", "b")), silent = TRUE),
                   "try-error"),
          "wrong-length col_names errors")
    check(inherits(try(read_xlsx(m, col_names = c(nm15[-1], NA)),
                       silent = TRUE), "try-error"),
          "NA in col_names errors")
  }

  check(inherits(try(read_xlsx(t, range = "banana"), silent = TRUE), "try-error"),
        "malformed range errors")
  check(inherits(try(read_xlsx(t, skip = -1), silent = TRUE), "try-error"),
        "negative skip errors")
  check(inherits(try(read_xlsx(t, range = "A1:ZZZ9"), silent = TRUE), "try-error"),
        "out-of-limit column errors")
} else cat("note: readxl not available; skipping range/skip/n_max checks\n")

## ---- 5. col_types ---------------------------------------------------------

ty <- fx("types")
if (file.exists(ty)) {
  warns <- function(expr) {
    w <- character()
    val <- withCallingHandlers(expr, warning = function(c) {
      w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning")
    })
    list(val = val, w = w)
  }

  tx <- read_xlsx(ty, col_types = "text")
  check(all(vapply(tx, is.character, TRUE)), "col_types text: all chr")
  check(identical(tx$id, c("007", "00042", "12345", NA)),
        "col_types text keeps leading zeros")
  check(identical(tx$amount, c("1.5", "-2", "0", "1000000")),
        "col_types text renders numerics")
  check(identical(tx$flag, c("TRUE", "FALSE", NA, "TRUE")),
        "col_types text renders booleans")
  check(identical(tx$stamp, c("44197", "44562.5", "30", NA)),
        "col_types text renders date serials")

  r <- warns(read_xlsx(ty, col_types = "numeric"))
  nm <- r$val
  check(all(vapply(nm, is.numeric, TRUE)), "col_types numeric: all num")
  check(identical(nm$id, c(7, 42, 12345, NA)), "text-to-numeric coercion")
  check(identical(nm$flag, c(1, 0, NA, 1)), "bool-to-numeric coercion")
  # divergence from readxl (which yields NA): dates become their serial,
  # matching what guessing already does for mixed numeric/date columns
  check(identical(nm$stamp, c(44197, 44562.5, 30, NA)),
        "date-to-numeric gives the serial")
  check(identical(nm$mix, c(10, NA, 1, 61)), "mixed-to-numeric")
  check(identical(nm$words, c(NA, NA, 3.5, NA)), "words-to-numeric")
  check(length(r$w) == 2 && any(grepl("column 'mix'.*first at E3", r$w)) &&
          any(grepl("3 cells in column 'words'", r$w)),
        "numeric coercion warnings")

  r <- warns(read_xlsx(ty, col_types = "logical"))
  lg <- r$val
  check(identical(lg$words, c(TRUE, FALSE, NA, NA)), "text-to-logical")
  check(identical(lg$amount, c(TRUE, TRUE, FALSE, TRUE)), "num-to-logical")
  check(identical(lg$stamp, c(NA, NA, NA, NA)) &&
          any(grepl("column 'stamp'.*logical", r$w)),
        "date-to-logical fails with warning")

  r <- warns(read_xlsx(ty, col_types = "date"))
  dt <- r$val
  check(identical(dt$stamp, read_xlsx(ty)$stamp), "forced date == guessed date")
  check(inherits(dt$amount, "POSIXct") &&
          identical(as.numeric(dt$amount[1]),
                    as.numeric(utc("1900-01-01 12:00:00"))),
        "numeric-to-date treats value as serial")
  check(is.na(dt$amount[2]) && any(grepl("column 'amount'.*first at B3", r$w)),
        "negative serial is not a date")
  check(all(is.na(dt$words)) && any(grepl("column 'words'.*date", r$w)),
        "text never parses as date")

  li <- read_xlsx(ty, col_types = "list")$mix
  check(is.list(li) && identical(li[[1]], 10) &&
          identical(li[[2]], "twenty") && identical(li[[3]], TRUE) &&
          inherits(li[[4]], "POSIXct"),
        "list column cell types")
  check(identical(read_xlsx(ty, col_types = "list")$id[[4]], NA),
        "list column blank is logical NA")

  sk <- read_xlsx(ty, col_types = c("text", "skip", "guess", "skip", "skip", "guess"))
  check(identical(names(sk), c("id", "flag", "words")), "skip drops columns")
  check(ncol(read_xlsx(ty, col_types = "skip")) == 0, "all-skip gives 0 cols")

  nh <- read_xlsx(ty, col_names = FALSE, col_types = "text")
  check(identical(nh[[1]][1:2], c("id", "007")),
        "col_names=FALSE types the header row too")

  check(inherits(try(read_xlsx(ty, col_types = c("text", "text")), silent = TRUE),
                 "try-error"), "col_types length mismatch errors")
  check(inherits(try(read_xlsx(ty, col_types = "banana"), silent = TRUE),
                 "try-error"), "unknown col_types errors")
  check(inherits(try(read_xlsx(ty, col_types = character()), silent = TRUE),
                 "try-error"), "empty col_types errors")

  if (requireNamespace("readxl", quietly = TRUE)) {
    rdx2 <- function(...) as.data.frame(suppressWarnings(suppressMessages(
      readxl::read_excel(..., progress = FALSE))))
    a <- read_xlsx(ty, col_types = "text")
    b <- rdx2(ty, col_types = "text")
    for (j in c("id", "flag", "stamp", "mix", "words"))
      check(identical(a[[j]], b[[j]]), paste0("readxl parity: text ", j))
    a <- suppressWarnings(
      read_xlsx(ty, col_types = c("skip", "numeric", "logical", "date", "skip", "skip")))
    b <- rdx2(ty, col_types = c("skip", "numeric", "logical", "date", "skip", "skip"))
    check(identical(a$amount, b$amount), "readxl parity: forced numeric")
    check(identical(a$flag, b$flag), "readxl parity: forced logical")
    check(identical(as.numeric(a$stamp), as.numeric(b$stamp)),
          "readxl parity: forced date")
    a <- read_xlsx(ty, col_types = "list")
    b <- rdx2(ty, col_types = "list")
    check(identical(a$words, b$words) && identical(a$id, b$id),
          "readxl parity: list column")
  }

  # spec applies per sheet in read_xlsx_all
  at <- read_xlsx_all(fx("multisheet"), col_types = "text")
  check(all(vapply(seq_along(at), function(i) identical(at[[i]]$x, as.character(i)), TRUE)),
        "read_xlsx_all col_types")
} else cat("note: types.xlsx fixture missing; skipping col_types checks\n")

## ---- 6. na strings --------------------------------------------------------

naf <- fx("na")
if (file.exists(naf)) {
  warns2 <- function(expr) {
    w <- character()
    val <- withCallingHandlers(expr, warning = function(c) {
      w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning")
    })
    list(val = val, w = w)
  }

  d0 <- read_xlsx(naf)
  check(identical(d0$num, c(1, -999, 3, 1000)), "na default keeps sentinels")
  check(identical(d0$txt, c("ok", "N/A", "N/A", "n/a")), "na default strings")
  check(identical(d0$mix, c("5", "-999", "TRUE", "N/A")),
        "na default mixed col is text")
  check(identical(d0$emp, c(NA, NA, "z", NA)),
        "na default: empty text reads NA")
  check(identical(d0$gone, rep("N/A", 4L)), "na default keeps gone col")

  d <- read_xlsx(naf, na = c("", "N/A", "-999"))
  check(identical(d$num, c(1, NA, 3, 1000)), "numeric sentinel NA by value")
  check(identical(d$txt, c("ok", NA, NA, "n/a")),
        "string na: padded matches, case does not")
  check(identical(d$mix, c(5, NA, 1, NA)), "na frees mixed col to numeric")
  check(is.logical(d$gone) && all(is.na(d$gone)), "all-na col is logical NA")

  d2 <- read_xlsx(naf, na = "N&A")
  check(identical(d2$raw, c(NA, "x&y", "<z>", "q")),
        "entity-laden value matches decoded na")
  check(identical(d2$emp, c("", "", "z", NA)),
        "na without \"\" keeps empty strings")

  check(identical(read_xlsx(naf, na = "1e3")$num, c(1, -999, 3, NA)),
        "numeric na matches by value, not text")

  d4 <- read_xlsx(naf, na = "44562.5")
  check(inherits(d4$dt, "POSIXct") && is.na(d4$dt[2]) &&
          identical(is.na(d4$dt), c(FALSE, TRUE, FALSE, FALSE)),
        "date cell na by serial value")

  ct <- rep("guess", 7); ct[4] <- "numeric"
  r <- warns2(read_xlsx(naf, na = "N&A", col_types = ct))
  check(all(is.na(r$val$raw)) && length(r$w) == 1 &&
          grepl("3 cells in column 'raw'", r$w),
        "forced numeric: na cells are silent, the rest warn")

  tx <- read_xlsx(naf, na = "-999", col_types = "text")
  check(identical(tx$num, c("1", NA, "3", "1000")),
        "forced text renders na-matched numeric as NA")

  al <- read_xlsx_all(naf, na = "N/A")
  check(all(is.na(al[[1]]$gone)), "read_xlsx_all passes na through")

  if (requireNamespace("readxl", quietly = TRUE)) {
    ref <- as.data.frame(suppressWarnings(suppressMessages(
      readxl::read_excel(naf, na = c("N/A", "-999"), progress = FALSE))))
    got <- suppressWarnings(read_xlsx(naf, na = c("N/A", "-999")))
    check(identical(got$num, ref$num), "readxl parity: na numeric col")
    check(identical(got$txt, ref$txt), "readxl parity: na string col")
    check(identical(is.na(got$mix), is.na(ref$mix)),
          "readxl parity: na mixed col NA pattern")
  }

  check(inherits(try(read_xlsx(naf, na = NA), silent = TRUE), "try-error"),
        "na containing NA errors")
} else cat("note: na.xlsx fixture missing; skipping na checks\n")

## ---- 7. name_repair -------------------------------------------------------

m <- fx("mixed")
if (file.exists(m)) {
  dup <- rep("a", 15)
  r <- read_xlsx(m, col_names = dup, name_repair = "minimal")
  check(identical(names(r), dup), "name_repair minimal keeps duplicates")
  err <- try(read_xlsx(m, col_names = dup, name_repair = "check_unique"),
             silent = TRUE)
  check(inherits(err, "try-error") &&
          grepl("duplicate column names.*'a'", err),
        "name_repair check_unique errors on duplicates")
  r <- read_xlsx(m, name_repair = "check_unique")
  check(is.data.frame(r), "name_repair check_unique passes unique names")
  err <- try(read_xlsx_all(m, col_names = dup, name_repair = "check_unique"),
             silent = TRUE)
  check(inherits(err, "try-error") &&
          grepl(paste0("sheet '", xlsx_sheets(m)[1], "'"), err),
        "read_xlsx_all check_unique names the sheet")
  al <- read_xlsx_all(m, col_names = dup, name_repair = "minimal")
  check(identical(names(al[[1]]), dup), "read_xlsx_all minimal")
  check(inherits(try(read_xlsx(m, name_repair = "banana"), silent = TRUE),
                 "try-error"),
        "unknown name_repair errors")
} else cat("note: mixed.xlsx fixture missing; skipping name_repair checks\n")

## ---- 8. hardening: date precision, encoding, malformed input -------------
# These build their own workbooks in tempdir() rather than using a fixture,
# so they run under R CMD check on the tarball too, where the generated
# fixtures are absent.

hd_DECL <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
hd_NS <- paste0('xmlns="http://schemas.openxmlformats.org/spreadsheetml',
                '/2006/main" xmlns:r="http://schemas.openxmlformats.org',
                '/officeDocument/2006/relationships"')
hd_ct <- paste0(hd_DECL, '<Types xmlns="http://schemas.openxmlformats.org',
  '/package/2006/content-types"><Default Extension="rels" ContentType="',
  'application/vnd.openxmlformats-package.relationships+xml"/><Default ',
  'Extension="xml" ContentType="application/xml"/><Override PartName="',
  '/xl/workbook.xml" ContentType="application/vnd.openxmlformats-office',
  'document.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/works',
  'heets/sheet1.xml" ContentType="application/vnd.openxmlformats-office',
  'document.spreadsheetml.worksheet+xml"/><Override PartName="/xl/styles',
  '.xml" ContentType="application/vnd.openxmlformats-officedocument.spre',
  'adsheetml.styles+xml"/></Types>')
hd_rels <- function(body)
  paste0(hd_DECL, '<Relationships xmlns="http://schemas.openxmlformats.org',
         '/package/2006/relationships">', body, '</Relationships>')
hd_parts <- function(sheet) list(
  "[Content_Types].xml" = hd_ct,
  "_rels/.rels" = hd_rels(paste0('<Relationship Id="rId1" Type="http://sch',
    'emas.openxmlformats.org/officeDocument/2006/relationships/officeDocum',
    'ent" Target="xl/workbook.xml"/>')),
  "xl/workbook.xml" = paste0(hd_DECL, '<workbook ', hd_NS, '><sheets><sheet',
    ' name="Sheet1" sheetId="1" r:id="rId1"/></sheets></workbook>'),
  "xl/_rels/workbook.xml.rels" = hd_rels(paste0(
    '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/offic',
    'eDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml',
    '"/><Relationship Id="rId2" Type="http://schemas.openxmlformats.org/of',
    'ficeDocument/2006/relationships/styles" Target="styles.xml"/>')),
  # xf 1 is numFmtId 22, a builtin date-time format
  "xl/styles.xml" = paste0(hd_DECL, '<styleSheet ', hd_NS, '><fonts count=',
    '"1"><font/></fonts><fills count="1"><fill/></fills><borders count="1">',
    '<border/></borders><cellStyleXfs count="1"><xf numFmtId="0"/></cellSty',
    'leXfs><cellXfs count="2"><xf numFmtId="0"/><xf numFmtId="22" applyNumb',
    'erFormat="1"/></cellXfs></styleSheet>'),
  "xl/worksheets/sheet1.xml" = sheet)
hd_sheet <- function(body)
  paste0(hd_DECL, '<worksheet ', hd_NS, '><sheetData>', body,
         '</sheetData></worksheet>')
hd_hdr <- function(nm)
  paste0('<row r="1"><c r="A1" t="inlineStr"><is><t>', nm,
         '</t></is></c></row>')

if (nzchar(Sys.which(Sys.getenv("R_ZIPCMD", "zip")))) {
  # sheet may be a character scalar or a raw vector (for invalid-UTF-8 bytes)
  hd_write <- function(name, sheet) {
    td <- file.path(tempdir(), paste0("hd_", name))
    unlink(td, recursive = TRUE)
    dir.create(td, recursive = TRUE)
    parts <- hd_parts(sheet)
    for (nm in names(parts)) {
      f <- file.path(td, nm)
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      con <- file(f, open = "wb")
      v <- parts[[nm]]
      writeBin(if (is.raw(v)) v else charToRaw(v), con)
      close(con)
    }
    p <- file.path(tempdir(), paste0("hd_", name, ".xlsx"))
    if (file.exists(p)) unlink(p)
    old <- setwd(td)
    on.exit(setwd(old), add = TRUE)
    utils::zip(p, ".", flags = "-r9Xq")
    p
  }

  # 8a. a whole second must survive as a whole second.  The day fraction has
  # no exact binary form, so an unrounded conversion lands just under the
  # second and formats one second early.
  hd_want <- c("2020-01-01 12:34:56", "2021-07-04 23:59:59",
               "1999-12-31 00:00:01", "2024-11-05 17:45:33",
               "2007-03-14 08:09:10")
  hd_t <- as.POSIXct(hd_want, tz = "UTC")
  hd_ser <- 25569 + as.numeric(hd_t) / 86400
  hd_r <- seq_along(hd_ser) + 1L
  p <- hd_write("dates", hd_sheet(paste0(hd_hdr("when"), paste0(
    '<row r="', hd_r, '"><c r="A', hd_r, '" s="1"><v>',
    sprintf("%.15g", hd_ser), '</v></c></row>', collapse = ""))))
  hd_got <- read_xlsx(p)$when
  check(inherits(hd_got, "POSIXct"), "hardening: datetime column typed")
  check(identical(format(hd_got, "%Y-%m-%d %H:%M:%S", tz = "UTC"), hd_want),
        "hardening: whole seconds read back as whole seconds")

  # 8b. text that is not valid UTF-8 must not reach R tagged as UTF-8:
  # nchar()/substr() throw on such a string, so the repair happens here.
  p <- hd_write("utf8", c(
    charToRaw(paste0(hd_DECL, '<worksheet ', hd_NS, '><sheetData>',
                     hd_hdr("txt"),
                     '<row r="2"><c r="A2" t="inlineStr"><is><t>A')),
    as.raw(c(0xff, 0xfe, 0x80)),
    charToRaw('B</t></is></c></row></sheetData></worksheet>')))
  hd_u <- read_xlsx(p)$txt
  check(validUTF8(hd_u[1]), "hardening: invalid UTF-8 repaired on read")
  check(!inherits(try(nchar(hd_u), silent = TRUE), "try-error"),
        "hardening: nchar() works on repaired text")
  check(!inherits(try(toupper(hd_u), silent = TRUE), "try-error"),
        "hardening: toupper() works on repaired text")

  # 8c. a malformed character reference must stay literal text, not decode
  # to codepoint 0 and put a NUL in the middle of an R string
  hd_lit <- "&notanentity; &#; &#xZZ; &#999999999; ok"
  p <- hd_write("entity", hd_sheet(paste0(hd_hdr("txt"),
    '<row r="2"><c r="A2" t="inlineStr"><is><t>', hd_lit,
    '</t></is></c></row>')))
  hd_e <- try(read_xlsx(p), silent = TRUE)
  check(!inherits(hd_e, "try-error"),
        "hardening: malformed character reference does not abort the read")
  if (!inherits(hd_e, "try-error"))
    check(identical(hd_e$txt[1], hd_lit),
          "hardening: malformed character reference kept literal")

  # 8d. a row number past the sheet limit sizes every column array from a
  # bogus extent, so it must be rejected rather than allocated for
  p <- hd_write("bigrow", hd_sheet(paste0(hd_hdr("x"),
    '<row r="999999999999"><c r="A999999999999"><v>1</v></c></row>')))
  hd_b <- try(read_xlsx(p), silent = TRUE)
  check(inherits(hd_b, "try-error") && grepl("1048576", hd_b),
        "hardening: row past the sheet limit errors instead of allocating")

  # 8e. structurally broken containers must error, never crash
  hd_src <- fx("edge")
  if (file.exists(hd_src)) {
    hd_raw <- readBin(hd_src, "raw", file.size(hd_src))
    hd_bad <- list(empty = raw(0),
                   notzip = charToRaw(strrep("not a zip ", 100)),
                   truncated = hd_raw[seq_len(length(hd_raw) %/% 2)],
                   header_only = hd_raw[1:4])
    for (nm in names(hd_bad)) {
      bp <- file.path(tempdir(), paste0("hd_broken_", nm, ".xlsx"))
      writeBin(hd_bad[[nm]], bp)
      check(inherits(try(read_xlsx(bp), silent = TRUE), "try-error"),
            paste0("hardening: broken container '", nm, "' errors in read_xlsx"))
      check(inherits(try(xlsx_sheets(bp), silent = TRUE), "try-error"),
            paste0("hardening: broken container '", nm,
                   "' errors in xlsx_sheets"))
    }
  }

  # 8f. every shipped fixture is far below the 4 MB threaded threshold, so
  # without this the threaded parser and its chunk merge never run under
  # R CMD check. RCXL_THREADS bypasses the threshold; a sheet with a few
  # thousand mixed rows is read serially and threaded and must agree exactly.
  # Multibyte strings ensure a chunk boundary can land inside a UTF-8
  # sequence; _R_CHECK_LIMIT_CORES_ permits the 2 workers asked for.
  hd_n <- 5000L
  hd_i <- seq_len(hd_n)
  hd_bool <- ifelse(hd_i %% 3L == 0L,
                    paste0('<c r="D', hd_i + 1L, '" t="b"><v>',
                           hd_i %% 2L, '</v></c>'), "")
  p <- hd_write("mt", hd_sheet(paste0(
    '<row r="1">',
    '<c r="A1" t="inlineStr"><is><t>num</t></is></c>',
    '<c r="B1" t="inlineStr"><is><t>txt</t></is></c>',
    '<c r="C1" t="inlineStr"><is><t>when</t></is></c>',
    '<c r="D1" t="inlineStr"><is><t>flag</t></is></c></row>',
    paste0('<row r="', hd_i + 1L, '">',
           '<c r="A', hd_i + 1L, '"><v>', hd_i, '.5</v></c>',
           '<c r="B', hd_i + 1L, '" t="inlineStr"><is><t>ré', hd_i,
           '€</t></is></c>',
           '<c r="C', hd_i + 1L, '" s="1"><v>', 40000L + hd_i %% 1000L,
           '</v></c>', hd_bool, '</row>', collapse = ""))))
  hd_thr <- Sys.getenv("RCXL_THREADS", unset = NA)
  Sys.setenv(RCXL_THREADS = "1")
  hd_ser <- read_xlsx(p)
  Sys.setenv(RCXL_THREADS = "2")
  hd_par <- read_xlsx(p)
  if (is.na(hd_thr)) Sys.unsetenv("RCXL_THREADS") else
    Sys.setenv(RCXL_THREADS = hd_thr)
  check(identical(hd_ser, hd_par),
        "hardening: threaded read identical to serial")
  check(nrow(hd_par) == hd_n && identical(sum(hd_par$num),
        hd_n * (hd_n + 1) / 2 + hd_n * 0.5), "hardening: threaded numerics")
  check(all(validUTF8(hd_par$txt)) &&
        identical(hd_par$txt[hd_n], paste0("ré", hd_n, "€")),
        "hardening: threaded multibyte strings")
  check(inherits(hd_par$when, "POSIXct") && !anyNA(hd_par$when),
        "hardening: threaded date column")
  check(is.logical(hd_par$flag) &&
        sum(!is.na(hd_par$flag)) == hd_n %/% 3L,
        "hardening: threaded bools and blanks")
} else cat("note: no zip command available; skipping hardening checks\n")

## ---- 9. foreign writer: LibreOffice Calc ---------------------------------
# flights.xlsx was saved by LibreOffice (sharedStrings, style-driven dates,
# its own styles.xml), not by rcxl's fixture generator, so these checks read
# a dialect no other part of the suite produces.
lo <- if (is.null(root)) fx("flights") else
  file.path(root, "inst", "extdata", "flights.xlsx")
if (file.exists(lo)) {
  fl <- read_xlsx(lo)
  check(identical(names(fl), c("YEAR", "FL_DATE", "OP_UNIQUE_CARRIER",
        "TAIL_NUM", "ORIGIN", "ORIGIN_CITY_NAME", "DEP_TIME", "DEP_DELAY",
        "DEP_DEL15", "ARR_DELAY_NEW")), "libreoffice: names")
  check(nrow(fl) == 100, "libreoffice: nrow")
  check(inherits(fl$FL_DATE, "POSIXct") &&
        all(fl$FL_DATE == utc("2026-01-01")), "libreoffice: date column")
  check(is.numeric(fl$YEAR) && all(fl$YEAR == 2026), "libreoffice: numeric column")
  check(identical(fl$DEP_DELAY[1:4], c(85, 71, 27, 1)), "libreoffice: values")
  check(identical(fl$TAIL_NUM[1], "N101NN"), "libreoffice: shared string")
  check("Dallas/Fort Worth, TX" %in% fl$ORIGIN_CITY_NAME,
        "libreoffice: shared string with comma")
  check(sum(is.na(fl$ARR_DELAY_NEW)) == 1 && !anyNA(fl[-10]),
        "libreoffice: blank cell placement")
  flt <- read_xlsx(lo, col_types = "text")
  check(all(vapply(flt, is.character, NA)) &&
        identical(flt$DEP_DELAY[1], "85"), "libreoffice: forced text")
  flr <- read_xlsx(lo, range = "A1:C5")
  check(identical(dim(flr), c(4L, 3L)) &&
        identical(names(flr), c("YEAR", "FL_DATE", "OP_UNIQUE_CARRIER")),
        "libreoffice: range read")
} else cat("note: flights.xlsx fixture missing; skipping LibreOffice checks\n")

## ---- 10. archive integrity: member CRC-32 -------------------------------
# Parts are extracted straight out of the mapped file rather than through
# miniz's own reader, so the CRC-32 each zip member records has to be checked
# on the way out. These archives are assembled byte by byte in R with stored
# (uncompressed) members, so a payload byte can be flipped while the recorded
# CRC stays put -- the silent-corruption case -- and no zip command is needed.

crc_le16 <- function(x) as.raw(bitwAnd(bitwShiftR(x, 8L * (0:1)), 255L))
crc_le32 <- function(x) as.raw(bitwAnd(bitwShiftR(x, 8L * (0:3)), 255L))

# base R has no crc32(), but a gzip stream carries one in its last 8 bytes
crc_of <- function(bytes) {
  f <- tempfile()
  con <- gzfile(f, "wb")
  writeBin(bytes, con)
  close(con)
  g <- readBin(f, "raw", file.size(f))
  unlink(f)
  g[length(g) - 7L:4L]
}

# tweak: named list of functions applied to a part's bytes *after* its CRC is
# taken, so the archive carries a correct CRC over the pre-tweak content
crc_zip <- function(path, parts, tweak = list()) {
  local <- raw(0)
  central <- raw(0)
  for (nm in names(parts)) {
    body <- parts[[nm]]
    if (!is.raw(body)) body <- charToRaw(body)
    crc <- crc_of(body)
    if (!is.null(tweak[[nm]])) body <- tweak[[nm]](body)
    nmr <- charToRaw(nm)
    n <- length(body)
    off <- length(local)
    local <- c(local,
      as.raw(c(0x50, 0x4b, 0x03, 0x04)), crc_le16(20L), crc_le16(0L),
      crc_le16(0L), crc_le16(0L), crc_le16(0L), crc, crc_le32(n),
      crc_le32(n), crc_le16(length(nmr)), crc_le16(0L), nmr, body)
    central <- c(central,
      as.raw(c(0x50, 0x4b, 0x01, 0x02)), crc_le16(20L), crc_le16(20L),
      crc_le16(0L), crc_le16(0L), crc_le16(0L), crc_le16(0L), crc,
      crc_le32(n), crc_le32(n), crc_le16(length(nmr)), crc_le16(0L),
      crc_le16(0L), crc_le16(0L), crc_le16(0L), crc_le32(0L),
      crc_le32(off), nmr)
  }
  writeBin(c(local, central,
    as.raw(c(0x50, 0x4b, 0x05, 0x06)), crc_le16(0L), crc_le16(0L),
    crc_le16(length(parts)), crc_le16(length(parts)),
    crc_le32(length(central)), crc_le32(length(local)), crc_le16(0L)), path)
  path
}

crc_path <- function(name) file.path(tempdir(), paste0("crc_", name, ".xlsx"))
# 1.5 -> 9.5 in the stored bytes: a value change no XML check would notice
crc_bend <- function(body) {
  at <- gregexpr("1.5", rawToChar(body), fixed = TRUE)[[1]][1]
  body[at] <- charToRaw("9")
  body
}
crc_bite <- function(body) {
  body[length(body) %/% 2L] <- as.raw(bitwXor(as.integer(body[length(body) %/% 2L]), 1L))
  body
}

crc_sheet <- hd_sheet(paste0(hd_hdr("x"),
  '<row r="2"><c r="A2"><v>1.5</v></c></row>'))
crc_parts <- c(hd_parts(crc_sheet),
  list("xl/sharedStrings.xml" = paste0(hd_DECL, '<sst ', hd_NS,
       ' count="1" uniqueCount="1"><si><t>alpha</t></si></sst>')))

# 10a. the stored-member archive itself must read, or the corruption checks
# below would pass for the wrong reason
crc_good <- crc_zip(crc_path("good"), crc_parts)
crc_d <- try(read_xlsx(crc_good), silent = TRUE)
check(!inherits(crc_d, "try-error") && identical(crc_d$x, 1.5),
      "crc: stored-member archive reads")
check(identical(try(xlsx_sheets(crc_good), silent = TRUE), "Sheet1"),
      "crc: stored-member sheet names")

# 10b. control: the same flip with the CRC taken over the flipped bytes. The
# cell reads 9.5 and every other check in the reader is happy with it, which
# is exactly why the CRC is the only thing standing between 10c and silence.
crc_ctl <- hd_parts(rawToChar(crc_bend(charToRaw(crc_sheet))))
crc_d <- try(read_xlsx(crc_zip(crc_path("control"), crc_ctl)), silent = TRUE)
check(!inherits(crc_d, "try-error") && identical(crc_d$x, 9.5),
      "crc: control -- flipped byte is invisible once the CRC agrees")

# 10c. a flipped worksheet byte changes a cell value with nothing else to
# give it away; only the member CRC catches it
crc_bad <- crc_zip(crc_path("sheet"), crc_parts,
                   list("xl/worksheets/sheet1.xml" = crc_bend))
crc_e <- try(read_xlsx(crc_bad), silent = TRUE)
check(inherits(crc_e, "try-error"), "crc: corrupt worksheet errors")
if (inherits(crc_e, "try-error"))
  check(grepl("corrupt", crc_e), "crc: corrupt worksheet says so")

# 10d. the optional parts must not be silently skipped when they are corrupt:
# dropping sharedStrings would turn every shared cell into NA
for (crc_part in c("xl/sharedStrings.xml", "xl/styles.xml",
                   "xl/_rels/workbook.xml.rels", "xl/workbook.xml")) {
  crc_bad <- crc_zip(crc_path("part"), crc_parts,
                     setNames(list(crc_bite), crc_part))
  crc_e <- try(read_xlsx(crc_bad), silent = TRUE)
  check(inherits(crc_e, "try-error") && grepl("corrupt", crc_e),
        paste0("crc: corrupt ", crc_part, " errors in read_xlsx"))
}

# 10e. xlsx_sheets extracts the workbook part on its own path
crc_bad <- crc_zip(crc_path("names"), crc_parts,
                   list("xl/workbook.xml" = crc_bite))
crc_e <- try(xlsx_sheets(crc_bad), silent = TRUE)
check(inherits(crc_e, "try-error") && grepl("corrupt", crc_e),
      "crc: corrupt workbook errors in xlsx_sheets")

## ---- 11. hardening: oversized integers in untrusted XML ------------------
# Row numbers, column refs, the declared dimension and uniqueCount all come
# from the archive and all feed sizing decisions, so a digit run long enough
# to overflow the accumulator must saturate into a rejected value rather than
# wrap into a small (or negative) one.

crc_ovf <- function(name, sheet, extra = list()) {
  crc_zip(crc_path(name), c(hd_parts(sheet), extra))
}

# 11a. a row number too long to fit in a long: rejected like any other row
# past the sheet limit, not wrapped into one inside it
ov <- try(read_xlsx(crc_ovf("row", hd_sheet(paste0(hd_hdr("x"),
  '<row r="99999999999999999999999999"><c r="A2"><v>1</v></c></row>')))),
  silent = TRUE)
check(inherits(ov, "try-error") && grepl("1048576", ov),
      "overflow: absurd row number rejected")

# 11b. a column ref of 40 letters: past the sheet width, so the cell is
# dropped and the read still completes
ov <- try(read_xlsx(crc_ovf("col", hd_sheet(paste0(hd_hdr("x"),
  '<row r="2"><c r="A2"><v>1</v></c><c r="', strrep("Z", 40),
  '2"><v>2</v></c></row>')))), silent = TRUE)
check(!inherits(ov, "try-error") && identical(ov$x, 1) && ncol(ov) == 1,
      "overflow: absurd column ref dropped, read completes")

# 11c. <dimension> presizes the grid, so an absurd extent must be clamped to
# the sheet limits instead of sizing an allocation from it
ov <- try(read_xlsx(crc_ovf("dim", paste0(hd_DECL, '<worksheet ', hd_NS,
  '><dimension ref="A1:', strrep("Z", 30), '99999999999999999999"/><sheetData>',
  hd_hdr("x"), '<row r="2"><c r="A2"><v>1</v></c></row>',
  '</sheetData></worksheet>'))), silent = TRUE)
check(!inherits(ov, "try-error") && identical(ov$x, 1),
      "overflow: absurd <dimension> clamped")

# 11d. uniqueCount only presizes the shared-string table, which grows on its
# own, so an absurd count must not become an absurd allocation
ov <- try(read_xlsx(crc_ovf("sst", hd_sheet(paste0(
  '<row r="1"><c r="A1" t="inlineStr"><is><t>x</t></is></c></row>',
  '<row r="2"><c r="A2" t="s"><v>0</v></c></row>')),
  list("xl/sharedStrings.xml" = paste0(hd_DECL, '<sst ', hd_NS,
       ' count="1" uniqueCount="99999999999999999999"><si><t>alpha</t></si>',
       '</sst>')))), silent = TRUE)
check(!inherits(ov, "try-error") && identical(ov$x, "alpha"),
      "overflow: absurd uniqueCount ignored")

if (fails > 0L) stop(fails, " golden check(s) failed")
cat("all golden checks passed\n")
