# Benchmark rcxl vs readxl with the bench package.
# Usage: Rscript tools/bench_mark.R [label]
# Assumes rcxl is installed into tools/lib (see tools/build.sh, or
# R CMD INSTALL --library=tools/lib rcxl_<version>.tar.gz for a release),
# fixtures exist (tools/make_fixtures.R), and bench is installed in the
# user library. The on-time file is skipped if absent.

lib <- file.path("tools", "lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
suppressMessages(library(bench))

label <- if (length(commandArgs(TRUE)) >= 1) commandArgs(TRUE)[[1]] else "bench"

files <- c(
  numeric = "tools/fixtures/numeric.xlsx",
  mixed   = "tools/fixtures/mixed.xlsx",
  sst     = "tools/fixtures/sst.xlsx",
  wide    = "tools/fixtures/wide.xlsx",
  ontime  = "tools/bench_data/T_ONTIME_REPORTING.xlsx"
)
files <- files[file.exists(files)]

read_serial <- function(path) {
  old <- Sys.getenv("RCXL_THREADS", unset = NA)
  Sys.setenv(RCXL_THREADS = "1")
  on.exit(if (is.na(old)) Sys.unsetenv("RCXL_THREADS") else Sys.setenv(RCXL_THREADS = old))
  rcxl::read_xlsx(path)
}

run <- function(path, iters) {
  # warm the page cache and lazy loads before timing
  invisible(rcxl::read_xlsx(path))
  invisible(readxl::read_excel(path, progress = FALSE))
  bench::mark(
    rcxl   = rcxl::read_xlsx(path),
    rcxl_1 = read_serial(path),
    readxl = readxl::read_excel(path, progress = FALSE),
    iterations = iters,
    check = FALSE,      # tibble vs data.frame; parity is checked separately
    filter_gc = FALSE   # large reads gc on most iterations
  )
}

res <- lapply(names(files), function(n) {
  r <- run(files[[n]], if (n == "ontime") 5L else 15L)
  r$file <- n
  r$expression <- as.character(r$expression)
  r
})
res <- do.call(rbind, res)

env <- list(
  r = R.version.string,
  rcxl = as.character(packageVersion("rcxl")),
  readxl = as.character(packageVersion("readxl")),
  bench = as.character(packageVersion("bench")),
  cpu = tryCatch(trimws(sub("model name\\s*:", "", grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[1])), error = function(e) NA),
  cores = parallel::detectCores(),
  os = paste(Sys.info()[c("sysname", "release")], collapse = " "),
  sha = tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) "nogit"),
  date = format(Sys.Date())
)

cat(sprintf("rcxl %s, readxl %s, bench %s\n%s\n%s, %d cores, %s\n\n",
            env$rcxl, env$readxl, env$bench, env$r, env$cpu, env$cores, env$os))
print(res[, c("file", "expression", "min", "median", "n_itr", "mem_alloc", "n_gc")], n = Inf)

dir.create(file.path("tools", "bench_results"), showWarnings = FALSE)
out <- file.path("tools", "bench_results", sprintf("%s-%s.rds", env$sha, label))
saveRDS(list(results = res, env = env), out)
cat("\nsaved ", out, "\n", sep = "")
