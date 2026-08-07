# Benchmark rcxl vs readxl on the fixtures.
# Usage: Rscript tools/bench.R [label]
#   BENCH_REPS (default 7), BENCH_FIXTURES (comma list, default all big ones)
# Assumes rcxl is installed into tools/lib (see tools/build.sh).

lib <- file.path("tools", "lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages({ library(rcxl); library(readxl) })

label <- if (length(commandArgs(TRUE)) >= 1) commandArgs(TRUE)[[1]] else "run"
reps <- as.integer(Sys.getenv("BENCH_REPS", "7"))
fixture_dir <- file.path("tools", "fixtures")
default_fx <- c("numeric", "sst", "inline", "mixed", "wide", "tiny")
fx <- strsplit(Sys.getenv("BENCH_FIXTURES", paste(default_fx, collapse = ",")), ",")[[1]]

time_read <- function(fun, path, reps) {
  fun(path)  # warm-up (page cache, JIT, lazy loads)
  ts <- numeric(reps)
  for (i in seq_len(reps)) {
    gc(FALSE)
    t0 <- proc.time()[["elapsed"]]
    x <- fun(path)
    ts[i] <- proc.time()[["elapsed"]] - t0
  }
  ts
}

res <- data.frame()
for (f in fx) {
  path <- file.path(fixture_dir, paste0(f, ".xlsx"))
  if (!file.exists(path)) { cat("skip missing", path, "\n"); next }
  t_rc <- time_read(function(p) rcxl::read_xlsx(p), path, reps)
  t_rx <- time_read(function(p) suppressMessages(readxl::read_excel(p, progress = FALSE)), path, reps)
  res <- rbind(res,
    data.frame(fixture = f, engine = "rcxl",   median_s = median(t_rc), min_s = min(t_rc)),
    data.frame(fixture = f, engine = "readxl", median_s = median(t_rx), min_s = min(t_rx)))
  cat(sprintf("%-8s rcxl %7.3fs   readxl %7.3fs   ratio %5.2fx\n",
              f, median(t_rc), median(t_rx), median(t_rx) / median(t_rc)))
}

sha <- tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "nogit")
res$sha <- sha; res$label <- label; res$reps <- reps
dir.create(file.path("tools", "bench_results"), showWarnings = FALSE)
out <- file.path("tools", "bench_results", paste0(sha, "-", label, ".csv"))
write.csv(res, out, row.names = FALSE)
cat("saved ", out, "\n", sep = "")
