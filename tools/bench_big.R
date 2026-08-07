# Benchmark rcxl vs readxl on the real-world on-time reporting file.
# Usage: Rscript tools/bench_big.R [label]
#   BENCH_REPS (default 5)
# Assumes rcxl is installed into tools/lib (see tools/build.sh) and
# tests/T_ONTIME_REPORTING.xlsx exists (see scratchpad convert script).

lib <- file.path("tools", "lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages({ library(rcxl); library(readxl) })

label <- if (length(commandArgs(TRUE)) >= 1) commandArgs(TRUE)[[1]] else "big"
reps <- as.integer(Sys.getenv("BENCH_REPS", "5"))
path <- file.path("tests", "T_ONTIME_REPORTING.xlsx")
stopifnot(file.exists(path))

time_read <- function(fun, path, reps) {
  x <- fun(path)  # warm-up (page cache, JIT, lazy loads)
  dims <- dim(x)
  rm(x)
  ts <- numeric(reps)
  for (i in seq_len(reps)) {
    gc(FALSE)
    t0 <- proc.time()[["elapsed"]]
    x <- fun(path)
    ts[i] <- proc.time()[["elapsed"]] - t0
    rm(x)
  }
  list(ts = ts, dims = dims)
}

r_rc <- time_read(function(p) rcxl::read_xlsx(p), path, reps)
r_rx <- time_read(function(p) suppressMessages(readxl::read_excel(p, progress = FALSE)), path, reps)

cat(sprintf("dims: rcxl %d x %d, readxl %d x %d\n",
            r_rc$dims[1], r_rc$dims[2], r_rx$dims[1], r_rx$dims[2]))
cat(sprintf("rcxl   times: %s\n", paste(sprintf("%.2f", r_rc$ts), collapse = " ")))
cat(sprintf("readxl times: %s\n", paste(sprintf("%.2f", r_rx$ts), collapse = " ")))
cat(sprintf("ontime   rcxl %7.3fs   readxl %7.3fs   ratio %5.2fx\n",
            median(r_rc$ts), median(r_rx$ts), median(r_rx$ts) / median(r_rc$ts)))

sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "nogit")
res <- rbind(
  data.frame(fixture = "ontime", engine = "rcxl",   median_s = median(r_rc$ts), min_s = min(r_rc$ts)),
  data.frame(fixture = "ontime", engine = "readxl", median_s = median(r_rx$ts), min_s = min(r_rx$ts)))
res$sha <- sha; res$label <- label; res$reps <- reps
dir.create(file.path("tools", "bench_results"), showWarnings = FALSE)
out <- file.path("tools", "bench_results", paste0(sha, "-", label, ".csv"))
write.csv(res, out, row.names = FALSE)
cat("saved ", out, "\n", sep = "")
