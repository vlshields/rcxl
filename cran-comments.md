## Patch release 0.1.1

This release fixes the installation failure of 0.1.0 reported on the CRAN
check page for r-release-macos-x86_64, r-oldrel-macos-arm64 and
r-oldrel-macos-x86_64. Those builders use the macOS 11 SDK, whose stdio.h
declares a BSD `zopen()`; the package defined a static helper of the same
name, which clang rejects as a redeclaration. The helper family has been
renamed with a `zsrc_` prefix. r-release-macos-arm64 (macOS 14.5 SDK) and all
Linux flavors were unaffected.

Every function and macro defined in the package sources was checked against
the macOS 11 SDK headers; `zopen` was the only collision.

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

Days since last update: 1

Possibly misspelled words in DESCRIPTION:
  decompressors (20:18)
```

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
