# rcxl

| CRAN checks | Linux | macOS | Windows |
|:---|:--:|:--:|:--:|
| [results](https://cran.r-project.org/web/checks/check_results_rcxl.html) | [![Linux](https://badges.cranchecks.info/flavor/linux/rcxl.svg)](https://cran.r-project.org/web/checks/check_results_rcxl.html) | [![macOS](https://badges.cranchecks.info/flavor/macos/rcxl.svg)](https://cran.r-project.org/web/checks/check_results_rcxl.html) | [![Windows](https://badges.cranchecks.info/flavor/windows/rcxl.svg)](https://cran.r-project.org/web/checks/check_results_rcxl.html) |

Read tabular data from `xlsx` files with a native C parser.

rcxl decodes each worksheet directly into R vectors as it is scanned,
with no intermediate document model. This keeps memory use close to the
size of the data itself. The `miniz` and `libdeflate` decompressors are
bundled, so the package has no R dependencies beyond base R.

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

Set the `RCXL_THREADS` environment variable to override the worker
count. Sheets under 4MB are read serially regardless.

## Coming from readxl

rcxl has no `guess_max` or `progress` argument. The arguments `sheet`,
`skip`, `n_max`, `na`, `trim_ws`, `col_names`, and `col_types` behave
the way they do in readxl, and dates come back as `POSIXct` in UTC.
`range` behaves more or less the same, with one edge case (see below).

|  | readxl | rcxl |
|----|----|----|
| Return type | tibble | plain data.frame |
| Listing sheets | [`excel_sheets()`](https://readxl.tidyverse.org/reference/excel_sheets.html) | [`xlsx_sheets()`](https://vlshields.github.io/rcxl/reference/xlsx_sheets.md) |
| Reading every sheet | loop over the sheets | [`read_xlsx_all()`](https://vlshields.github.io/rcxl/reference/read_xlsx_all.md) |
| Type guessing | first `guess_max` rows | every cell in the read area |
| Name repair | `.name_repair`, any tidyverse option | `name_repair`: `"unique"`, `"minimal"`, or `"check_unique"` |
| Formats | `xls` and `xlsx` | `xlsx` only |
| `range` entirely past the data | 0-by-0 result | the range’s rectangle, as all-`NA` columns |

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
