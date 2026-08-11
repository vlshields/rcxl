# Correctness net for rcxl. Three layers:
#   1. hand-written asserts on handcrafted fixtures (edge/date1904/multisheet)
#   2. snapshot comparison against tools/golden/*.rds (captured at baseline)
#   3. tolerant cross-check against readxl on the bulk fixtures
# Skips quietly if fixtures are absent (e.g. plain R CMD check without setup).

find_root <- function() {
  for (d in c(".", "..", "../..")) {
    if (dir.exists(file.path(d, "tools", "fixtures"))) return(normalizePath(d))
  }
  NULL
}
root <- find_root()
if (is.null(root)) { cat("fixtures not found; skipping golden tests\n"); quit(save = "no") }
fixdir <- file.path(root, "tools", "fixtures")
lib <- file.path(root, "tools", "lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
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

golddir <- file.path(root, "tools", "golden")
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

  check(inherits(try(read_xlsx(m, range = "banana"), silent = TRUE), "try-error"),
        "malformed range errors")
  check(inherits(try(read_xlsx(m, skip = -1), silent = TRUE), "try-error"),
        "negative skip errors")
  check(inherits(try(read_xlsx(m, range = "A1:ZZZ9"), silent = TRUE), "try-error"),
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

if (fails > 0L) stop(fails, " golden check(s) failed")
cat("all golden checks passed\n")
