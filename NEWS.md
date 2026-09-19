# rcxl 0.1.1

* Fixed a compile failure on macOS with the macOS 11 SDK, whose stdio.h
  declares a BSD `zopen()` that collided with an internal helper of the same
  name. The helper family is now prefixed `zsrc_`.

# rcxl 0.1.0

* Initial CRAN release.
