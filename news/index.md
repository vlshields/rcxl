# Changelog

## rcxl 0.1.2

- Worker threads are now joined before an R error propagates. Previously
  an error while building text columns or parsing workbook metadata left
  them running and writing into freed memory, which could crash the
  session or corrupt data. A raw NUL byte in a cell was enough to
  trigger it.

- Documentation corrections. `RCXL_THREADS` overrides the 4 MB serial
  threshold rather than being bound by it, the architecture description
  no longer claims cells are decoded straight into R vectors, and the
  readxl comparison notes that `na` matches numeric cells by value.

## rcxl 0.1.1

CRAN release: 2026-10-05

- Fixed a compile failure on macOS with the macOS 11 SDK, whose stdio.h
  declares a BSD `zopen()` that collided with an internal helper of the
  same name. The helper family is now prefixed `zsrc_`.

## rcxl 0.1.0

CRAN release: 2026-09-17

- Initial CRAN release.
