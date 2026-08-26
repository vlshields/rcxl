# rcxl

[![R-CMD-check](https://github.com/vlshields/rcxl/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vlshields/rcxl/actions/workflows/R-CMD-check.yaml)

Read tabular data from `xlsx` files with a native C parser.

Worksheet XML is scanned in a single pass and decoded directly into R vectors,
with no intermediate document model. The `miniz` and `libdeflate`
decompressors are bundled, so the package has no R dependencies beyond base R.

## Installation

```r
# install.packages("remotes")
remotes::install_github("vlshields/rcxl")
```

## Usage

`read_xlsx()` returns a plain data.frame, taking the first row as the header:

```r
library(rcxl)

path <- system.file("extdata", "flights.xlsx", package = "rcxl")
df <- read_xlsx(path)
dim(df)
#> [1] 100  10
```

Column types are guessed from the cells in the read area; dates come back as
`POSIXct` in UTC.

`xlsx_sheets()` lists sheets, and `sheet` takes a name or an index:

```r
multi <- system.file("extdata", "multisheet.xlsx", package = "rcxl")
xlsx_sheets(multi)
#> [1] "Alpha" "Beta"  "Gamma"

read_xlsx(multi, sheet = "Beta")
read_xlsx(multi, sheet = 2)
```

`read_xlsx_all()` reads every sheet from a single workbook parse and returns a
named list of data.frames:

```r
sheets <- read_xlsx_all(multi)
names(sheets)
#> [1] "Alpha" "Beta"  "Gamma"
```

Reads can be windowed with an A1 `range` (column-only and row-only forms work
too), or with `skip` / `n_max`:

```r
read_xlsx(path, range = "B3:D10")
read_xlsx(path, range = "B:C")
read_xlsx(path, skip = 5, n_max = 10)
```

`col_names` accepts `TRUE`, `FALSE` (auto names, header row becomes data), or a
character vector sized to the read area. `col_types` takes `"text"`,
`"numeric"`, `"date"`, `"logical"`, `"guess"`, `"skip"`, or `"list"`, recycled
if length 1:

```r
read_xlsx(path, col_names = FALSE)
read_xlsx(path, col_types = "text")                    # everything as character
read_xlsx(path, col_types = c("skip", rep("guess", 9)))  # drop the first column
```

Other arguments: `na` blanks matching cells, `trim_ws` trims surrounding
whitespace, and `name_repair` is one of `"unique"` (default), `"minimal"`, or
`"check_unique"`. See `?read_xlsx` for details.

Set the `RCXL_THREADS` environment variable to override the worker count.
Sheets under 4MB are read serially regardless.

## License

MIT. Bundled `miniz` and `libdeflate` retain their own copyright and license
terms; see `inst/COPYRIGHTS`.
