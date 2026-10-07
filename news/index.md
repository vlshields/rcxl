# Changelog

## rcxl 0.1.1

CRAN release: 2026-10-05

- Fixed a compile failure on macOS with the macOS 11 SDK, whose stdio.h
  declares a BSD `zopen()` that collided with an internal helper of the
  same name. The helper family is now prefixed `zsrc_`.

## rcxl 0.1.0

CRAN release: 2026-09-17

- Initial CRAN release.
