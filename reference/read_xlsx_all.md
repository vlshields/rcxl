# Read several worksheets in one pass

Reads multiple sheets from an xlsx file through one shared workbook
pass, so the ZIP directory, shared strings and styles are parsed once.
All arguments other than `sheets` apply to every sheet read.

## Usage

``` r
read_xlsx_all(
  path,
  sheets = NULL,
  col_names = TRUE,
  trim_ws = TRUE,
  range = NULL,
  skip = 0L,
  n_max = Inf,
  col_types = NULL,
  na = "",
  name_repair = c("unique", "minimal", "check_unique")
)
```

## Arguments

- path:

  Path to an xlsx file. Tilde expansion is applied.

- sheets:

  Sheets to read, as a vector of 1-based indices or sheet names. `NULL`
  reads every sheet in workbook order.

- col_names:

  `TRUE` to use the first row of the read area as column names, `FALSE`
  to number them `V1`, `V2`, ..., or a character vector of names, one
  per sheet column including any skipped via `col_types` (the skipped
  names are dropped from the result).

- trim_ws:

  Trim leading and trailing whitespace from string cells and column
  names?

- range:

  As in
  [`read_xlsx()`](https://vlshields.github.io/rcxl/reference/read_xlsx.md),
  except a sheet-qualified range is an error here; select sheets with
  `sheets`.

- skip:

  Number of rows to skip before reading anything. Ignored when `range`
  is given.

- n_max:

  Maximum number of data rows to read. The column name row does not
  count. Ignored when `range` is given.

- col_types:

  `NULL` to guess every column, or a character vector of `"guess"`,
  `"skip"`, `"logical"`, `"numeric"`, `"date"`, `"text"` or `"list"`. A
  single value applies to every column; otherwise supply one entry per
  sheet column, counting `"skip"` columns. A `"list"` column keeps each
  cell's own type: length-one numeric, `POSIXct`, logical or character
  elements, with blank cells as logical `NA`.

- na:

  Character vector of strings to read as `NA`. Matched against
  whitespace-trimmed cell text; entries that parse as numbers also blank
  numeric and date cells holding that value.

- name_repair:

  As in
  [`read_xlsx()`](https://vlshields.github.io/rcxl/reference/read_xlsx.md).
  A `"check_unique"` error names the offending sheet.

## Value

A named list of `data.frame`s, one per sheet read, named by sheet name.

## Examples

``` r
multi <- system.file("extdata", "multisheet.xlsx", package = "rcxl")
sheets <- read_xlsx_all(multi)
names(sheets)
#> [1] "Alpha" "Beta"  "Gamma"
sheets[["Alpha"]]
#>   x
#> 1 1

# a subset of the sheets, still in one workbook pass
names(read_xlsx_all(multi, sheets = c("Alpha", "Gamma")))
#> [1] "Alpha" "Gamma"
```
