# Capture golden snapshots of read_xlsx output for every fixture.
# Run this ONCE at a known-good build; later phases are diffed against it
# by tests/golden.R. Usage: Rscript tools/make_golden.R

lib <- file.path("tools", "lib")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
library(rcxl)

fixdir <- file.path("tools", "fixtures")
golddir <- file.path("tools", "golden")
dir.create(golddir, showWarnings = FALSE)
for (f in list.files(fixdir, pattern = "\\.xlsx$")) {
  name <- sub("\\.xlsx$", "", f)
  x <- read_xlsx(file.path(fixdir, f))
  saveRDS(x, file.path(golddir, paste0(name, ".rds")), version = 2)
  cat(sprintf("%-14s %d x %d\n", name, nrow(x), ncol(x)))
}
