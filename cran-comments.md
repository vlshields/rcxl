## Resubmission

This is a resubmission. In response to the CRAN reviewer's comments:

* Removed the single quotes around "xlsx" in the Title and Description, as
  requested. The quotes around 'miniz' and 'libdeflate' (software names) are
  kept.

* Added every author, contributor and copyright holder of the bundled code to
  Authors@R. The bundled miniz sources credit, besides Rich Geldreich,
  Tenacious Software LLC and RAD Game Tools and Valve Software (already
  listed), a 2016 copyright by Martin Raiber on the ZIP reader, a
  public-domain PNG writer by Alex Evans, and a minimum-redundancy routine
  by Alistair Moffat and Jyrki Katajainen. Martin Raiber is now listed with
  roles "ctb" and "cph"; Alex Evans, Alistair Moffat and Jyrki Katajainen
  with role "ctb". The library authors Rich Geldreich and Eric Biggers now
  carry "ctb" in addition to "cph". inst/COPYRIGHTS (referenced from the
  Copyright field) was updated to record the same contributions and their
  licence terms, and to correct a stale note about a build flag.

## Test environments

* local: Debian GNU/Linux 13 (trixie), R 4.5.0
* win-builder: Windows Server 2022 x64, R 4.6.1 (release)
* GitHub Actions: macOS (R release), Windows (R release), Ubuntu (R devel,
  release, and oldrel-1), all with --as-cran

The mac-builder service was unavailable at the time of submission, so macOS
was checked via GitHub Actions instead.

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Vincent Shields <vince.shields913@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  decompressors (20:18)
```

This is a new submission.

"decompressors" is spelled correctly; it is the plural of "decompressor" and
refers to the two bundled decompression libraries ('miniz' and 'libdeflate').

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
