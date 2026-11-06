## Patch release 0.1.2

This release fixes two bugs and corrects the documentation.

* Worker threads are now joined before an R error propagates. In 0.1.1 an
  error raised while building text columns or parsing workbook metadata left
  the threads running and writing into freed memory, which could crash the
  session or corrupt data. A raw NUL byte in a cell was enough to trigger it.
  A regression test is included.

* An `na` marker stored XML-escaped in the sheet (for example `N&amp;A` for
  `"N&A"`), or split across the rich-text runs of an inline string, is now
  matched before the column type is guessed. In 0.1.1 such a cell counted as
  text, so a numeric column holding it was returned as character.

* The README and package documentation were corrected where they overstated
  the implementation. There is no change in behaviour from these edits.

## Test environments

* local: Debian GNU/Linux 13 (trixie), R 4.5.0
* win-builder: Windows Server 2022 x64, R 4.6.2 (release) and R 4.5.3
  (oldrelease)
* GitHub Actions: macOS (R release), Windows (R release), Ubuntu (R devel,
  release, and oldrel-1), all with --as-cran

The mac-builder service was unavailable at the time of submission, so macOS
was checked via GitHub Actions instead.

The full test suite was also run locally under valgrind and on a build with
the address and undefined-behaviour sanitizers, with no reports.

## R CMD check results

0 errors | 0 warnings | 0 notes

The local check alone adds one NOTE from the future file timestamps check,
"unable to verify current time". It does not appear on win-builder.

The incoming feasibility check may report "decompressors" in DESCRIPTION as
possibly misspelled. It is spelled correctly; it is the plural of
"decompressor" and refers to the two bundled decompression libraries ('miniz'
and 'libdeflate').

## Bundled source

The package bundles two third-party C libraries, 'miniz' (MIT, Rich
Geldreich, Tenacious Software LLC, and RAD Game Tools and Valve Software) and
'libdeflate' (MIT, Eric Biggers and Google LLC). Both are recorded in
Authors@R with role "cph", and the full licence texts are in inst/COPYRIGHTS.
There are no compiled-code dependencies outside the package.

The 'miniz' build on Windows emits two -Wunused-function warnings
(mz_freopen, mz_stat64). These come from the vendored upstream sources: the
functions are only referenced by the archive *writing* paths, which the package
compiles out via -DMINIZ_NO_ARCHIVE_WRITING_APIS.
