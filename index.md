# rcxl

| CRAN checks | Linux | macOS | Windows |
|:---|:--:|:--:|:--:|
| [results](https://cran.r-project.org/web/checks/check_results_rcxl.html) | [![Linux](https://badges.cranchecks.info/flavor/linux/rcxl.svg)](https://cran.r-project.org/web/checks/check_results_rcxl.html) | [![macOS](https://badges.cranchecks.info/flavor/macos/rcxl.svg)](https://cran.r-project.org/web/checks/check_results_rcxl.html) | [![Windows](https://badges.cranchecks.info/flavor/windows/rcxl.svg)](https://cran.r-project.org/web/checks/check_results_rcxl.html) |

Read tabular data from `xlsx` files with a native C parser.

rcxl scans worksheet XML in a single pass, with no intermediate document
model: cells land in compact per-column buffers, and the R vectors are
built from those once the sheet’s extent and column types are known. The
`miniz` and `libdeflate` decompressors are bundled, so the package has
no R dependencies beyond base R.

## Installation

From CRAN:

``` r

install.packages("rcxl")
```

The development version from GitHub:

``` r

# install.packages("remotes")
remotes::install_github("vlshields/rcxl")
```

## Usage

[`read_xlsx()`](https://vlshields.github.io/rcxl/reference/read_xlsx.md)
returns a plain data.frame, taking the first row as the header:

``` r

library(rcxl)

path <- system.file("extdata", "flights.xlsx", package = "rcxl")
df <- read_xlsx(path)
dim(df)
#> [1] 100  10
```

Column types are guessed from the cells in the read area; dates come
back as `POSIXct` in UTC.

[`xlsx_sheets()`](https://vlshields.github.io/rcxl/reference/xlsx_sheets.md)
lists sheets, and `sheet` takes a name or an index:

``` r

multi <- system.file("extdata", "multisheet.xlsx", package = "rcxl")
xlsx_sheets(multi)
#> [1] "Alpha" "Beta"  "Gamma"

read_xlsx(multi, sheet = "Beta")
read_xlsx(multi, sheet = 2)
```

[`read_xlsx_all()`](https://vlshields.github.io/rcxl/reference/read_xlsx_all.md)
reads every sheet from a single workbook parse and returns a named list
of data.frames:

``` r

sheets <- read_xlsx_all(multi)
names(sheets)
#> [1] "Alpha" "Beta"  "Gamma"
```

Reads can be windowed with an A1 `range` (column-only and row-only forms
work too), or with `skip` / `n_max`:

``` r

read_xlsx(path, range = "B3:D10")
read_xlsx(path, range = "B:C")
read_xlsx(path, skip = 5, n_max = 10)
```

`col_names` accepts `TRUE`, `FALSE` (auto names, header row becomes
data), or a character vector sized to the read area. `col_types` takes
`"text"`, `"numeric"`, `"date"`, `"logical"`, `"guess"`, `"skip"`, or
`"list"`, recycled if length 1:

``` r

read_xlsx(path, col_names = FALSE)
read_xlsx(path, col_types = "text")                    # everything as character
read_xlsx(path, col_types = c("skip", rep("guess", 9)))  # drop the first column
```

Other arguments: `na` blanks matching cells, `trim_ws` trims surrounding
whitespace, and `name_repair` is one of `"unique"` (default),
`"minimal"`, or `"check_unique"`. See
[`?read_xlsx`](https://vlshields.github.io/rcxl/reference/read_xlsx.md)
for details.

By default, worksheets with less than 4 MB of uncompressed XML are read
serially and larger ones on several threads. Set the `RCXL_THREADS`
environment variable to an explicit worker count; it overrides both the
default count and the size threshold.

## Performance

Median wall-clock time to read each file, warm cache, measured with the
`bench` package. The timing runs do not compare outputs. Parity with
readxl is checked separately: `tools/parity_big.R` requires an exact
match, cell for cell, on the on-time file and the other large workbook,
and the test suite compares the fixtures with a tolerance of 1e-9 on
numeric values.

| File                         |   rcxl | rcxl, 1 thread | readxl |
|------------------------------|-------:|---------------:|-------:|
| BTS on-time reporting, 96 MB | 2.51 s |         3.99 s | 15.9 s |
| numeric fixture, 16 MB       | 124 ms |         215 ms | 954 ms |
| mixed fixture, 10 MB         | 136 ms |         204 ms | 796 ms |
| shared-strings fixture       |  31 ms |          49 ms | 376 ms |
| wide fixture                 |  24 ms |          45 ms | 186 ms |

rcxl 0.1.1, readxl 1.5.0, R 4.5.0, bench 1.1.4, on an AMD Ryzen 3 7320U
(8 cores) under Linux, 2026-10-15. Medians of 15 iterations (5 for the
on-time file). Across three runs the rcxl medians agreed within 3% and
the readxl medians within 8%, the widest spread being readxl on the
on-time file (15.6 s to 16.7 s). The raw results are in
`tools/bench_results`. The fixtures come from `tools/make_fixtures.R`;
the on-time file is the public Bureau of Transportation Statistics
on-time reporting table converted to xlsx. The script is
`tools/bench_mark.R`.

## Coming from readxl

rcxl has no `guess_max` or `progress` argument. The arguments `sheet`,
`skip`, `n_max`, `trim_ws`, `col_names`, and `col_types` take the same
values as in readxl, and the test suite checks `skip`, `n_max`, `range`,
`col_names`, and `col_types` (including forced conversions) against
readxl 1.5.0 on the shipped fixtures. Dates come back as `POSIXct` in
UTC. `na` and `range` each differ in one case, and `name_repair` offers
fewer options (see below).

|  | readxl | rcxl |
|----|----|----|
| Return type | tibble | plain data.frame |
| Listing sheets | [`excel_sheets()`](https://readxl.tidyverse.org/reference/excel_sheets.html) | [`xlsx_sheets()`](https://vlshields.github.io/rcxl/reference/xlsx_sheets.md) |
| Reading every sheet | loop over the sheets | [`read_xlsx_all()`](https://vlshields.github.io/rcxl/reference/read_xlsx_all.md) |
| Type guessing | first `guess_max` rows | every cell in the read area |
| Name repair | `.name_repair`, any tidyverse option | `name_repair`: `"unique"`, `"minimal"`, or `"check_unique"` |
| Formats | `xls` and `xlsx` | `xlsx` only |
| `range` entirely past the data | 0-by-0 result | the range’s rectangle, as all-`NA` columns |
| `na` against a numeric cell | matches the text stored in the file | matches the value, so `"-999.0"` blanks a cell holding -999 |

### Masking

Both packages export
[`read_xlsx()`](https://vlshields.github.io/rcxl/reference/read_xlsx.md).
If both are attached, the one attached last masks the other:

``` r

library(readxl)
library(rcxl)
#> Attaching package: 'rcxl'
#>
#> The following object is masked from 'package:readxl':
#>
#>     read_xlsx
```

To be explicit about which one runs, call it with the namespace:

``` r

rcxl::read_xlsx(path)
readxl::read_xlsx(path)
```

## License

MIT. Bundled `miniz` and `libdeflate` retain their own copyright and
license terms; see `inst/COPYRIGHTS`.
