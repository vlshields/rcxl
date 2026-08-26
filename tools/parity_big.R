# Cell-for-cell parity of rcxl vs readxl on the large bench workbooks, which
# were written by XlsxWriter rather than rcxl's own fixture generator. Both
# files are big enough to run the threaded parser.
# Usage: Rscript tools/parity_big.R
# Assumes rcxl is installed into tools/lib (see tools/build.sh).

lib <- file.path("tools", "lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages({ library(rcxl); library(readxl) })

files <- c("data", "T_ONTIME_REPORTING")
fails <- 0L
check <- function(cond, what) {
  if (!isTRUE(cond)) { cat("FAIL:", what, "\n"); fails <<- fails + 1L }
}

for (f in files) {
  path <- file.path("tools", "bench_data", paste0(f, ".xlsx"))
  if (!file.exists(path)) { cat("skip missing", path, "\n"); next }
  cat("==", f, "==\n")
  a <- rcxl::read_xlsx(path)
  # guess_max = Inf so readxl types from every cell, as rcxl does
  b <- suppressWarnings(suppressMessages(
    readxl::read_excel(path, guess_max = Inf, progress = FALSE)))

  check(identical(dim(a), dim(b)), paste0(f, ": dimensions ",
        paste(dim(a), collapse = "x"), " vs ", paste(dim(b), collapse = "x")))
  check(identical(names(a), names(b)), paste0(f, ": column names"))
  if (!identical(dim(a), dim(b))) next

  for (j in seq_along(a)) {
    x <- a[[j]]; y <- b[[j]]; nm <- names(a)[j]
    id <- paste0(f, "$", nm)
    if (inherits(y, "POSIXct")) {
      check(inherits(x, "POSIXct"), paste0(id, ": rcxl type ", class(x)[1],
            " vs readxl POSIXct"))
      if (inherits(x, "POSIXct")) {
        dx <- abs(as.numeric(x) - as.numeric(y))
        bad <- which(dx > 0 | xor(is.na(x), is.na(y)))
        check(length(bad) == 0, paste0(id, ": ", length(bad),
              " datetime mismatches, first at row ", bad[1],
              ", max diff ", format(max(dx, na.rm = TRUE), digits = 3), "s"))
      }
    } else {
      check(identical(class(x), class(y)), paste0(id, ": rcxl type ",
            class(x)[1], " vs readxl ", class(y)[1]))
      if (!identical(x, y)) {
        bad <- which(x != y | xor(is.na(x), is.na(y)))
        check(FALSE, paste0(id, ": ", length(bad), " value mismatches",
              if (length(bad)) paste0(", first at row ", bad[1], ": ",
                deparse(x[bad[1]]), " vs ", deparse(y[bad[1]])) else ""))
      }
    }
  }
  rm(a, b); gc(FALSE)
}

if (fails > 0L) stop(fails, " parity check(s) failed")
cat("full parity with readxl on all large workbooks\n")
