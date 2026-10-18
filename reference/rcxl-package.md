# rcxl: Read xlsx Files with a Native Parser

Reads tabular data from xlsx files with a specialized C parser.
Worksheet XML is scanned in a single pass into compact per-column
buffers, with no intermediate document model, and the R vectors are
built from those. Bundles the 'miniz' and 'libdeflate' decompressors to
read the underlying archive.

## Details

By default, worksheets with at least 4 MB of uncompressed XML are parsed
on several threads and smaller ones are read serially. The worker count
defaults to the number of available cores, capped at 8. Set the
`RCXL_THREADS` environment variable to an explicit count to override
both the count and the size threshold. When `_R_CHECK_LIMIT_CORES_` is
set, as it is under `R CMD check`, no more than two workers are used
whatever the setting.

## See also

Useful links:

- <https://github.com/vlshields/rcxl>

- Report bugs at <https://github.com/vlshields/rcxl/issues>

## Author

**Maintainer**: Vincent Shields <vince.shields913@gmail.com> \[copyright
holder\]

Authors:

- Vincent Shields <vince.shields913@gmail.com> \[copyright holder\]

Other contributors:

- Rich Geldreich (author of the bundled miniz) \[contributor, copyright
  holder\]

- Tenacious Software LLC (copyright holder of the bundled miniz)
  \[copyright holder\]

- RAD Game Tools and Valve Software (copyright holder of the bundled
  miniz) \[copyright holder\]

- Martin Raiber (contributor to and copyright holder of the miniz ZIP
  reader) \[contributor, copyright holder\]

- Alex Evans (author of the PNG writer in the bundled miniz)
  \[contributor\]

- Alistair Moffat (author of the minimum-redundancy code in the bundled
  miniz) \[contributor\]

- Jyrki Katajainen (author of the minimum-redundancy code in the bundled
  miniz) \[contributor\]

- Eric Biggers (author of the bundled libdeflate) \[contributor,
  copyright holder\]

- Google LLC (copyright holder of the bundled libdeflate) \[copyright
  holder\]
