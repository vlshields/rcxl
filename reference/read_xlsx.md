# Read an xlsx worksheet

Reads one worksheet from an xlsx file into a data frame. Column types
are guessed from the cells unless overridden with `col_types`.

## Usage

``` r
read_xlsx(
  path,
  sheet = 1L,
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

- sheet:

  Worksheet to read, as a 1-based index or a sheet name. Overridden when
  `range` names a sheet.

- col_names:

  `TRUE` to use the first row of the read area as column names, `FALSE`
  to number them `V1`, `V2`, ..., or a character vector of names, one
  per sheet column including any skipped via `col_types` (the skipped
  names are dropped from the result).

- trim_ws:

  Trim leading and trailing whitespace from string cells and column
  names?

- range:

  A1-style cell range to read: `"B3:D87"`, a single cell `"B3"`, whole
  columns `"B:D"`, or whole rows `"3:87"`, optionally sheet-qualified as
  `"Sheet1!B3:D87"` or `"'My Sheet'!B3:D87"`. When given, `skip` and
  `n_max` are ignored. An axis the range bounds is read exactly, blank
  cells included; an open axis is trimmed to the sheet extent.

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

  How to resolve column names in the result. `"unique"` suffixes
  repeated names via
  [`make.unique()`](https://rdrr.io/r/base/make.unique.html) with
  separator `"_"`, `"minimal"` keeps names as read, `"check_unique"`
  errors on any repeat.

## Value

A `data.frame` with one column per worksheet column read.

## Details

A guessed column becomes character if it holds any string cell, logical
if it holds only booleans, `POSIXct` if it holds date cells and no plain
numbers, and numeric otherwise. Every cell in the read area informs the
guess. Columns with only blank cells become logical `NA`. Date cells are
returned as `POSIXct` in UTC; both the 1900 and 1904 date systems are
handled, including the nonexistent 29 Feb 1900 that the 1900 system
counts.

Cells that cannot be coerced to a type forced through `col_types` become
`NA`, with a warning giving the count and the first offending cell.

## See also

[`read_xlsx_all()`](https://vlshields.github.io/rcxl/reference/read_xlsx_all.md)
to read several sheets in one workbook pass;
[`xlsx_sheets()`](https://vlshields.github.io/rcxl/reference/xlsx_sheets.md)
for the sheet names.

## Examples

``` r
types <- system.file("extdata", "types.xlsx", package = "rcxl")
str(read_xlsx(types))
#> 'data.frame':    4 obs. of  6 variables:
#>  $ id    : chr  "007" "00042" "12345" NA
#>  $ amount: num  1.5 -2.0 0.0 1.0e+06
#>  $ flag  : logi  TRUE FALSE NA TRUE
#>  $ stamp : POSIXct, format: "2021-01-01 00:00:00" "2022-01-01 12:00:00" ...
#>  $ mix   : chr  "10" "twenty" "TRUE" "61"
#>  $ words : chr  "TRUE" "false" "3.5" "abc"

# read a rectangle rather than the whole sheet
read_xlsx(types, range = "A1:C3")
#>      id amount  flag
#> 1   007    1.5  TRUE
#> 2 00042   -2.0 FALSE

# force every column to character instead of guessing
str(read_xlsx(types, col_types = "text"))
#> 'data.frame':    4 obs. of  6 variables:
#>  $ id    : chr  "007" "00042" "12345" NA
#>  $ amount: chr  "1.5" "-2" "0" "1000000"
#>  $ flag  : chr  "TRUE" "FALSE" NA "TRUE"
#>  $ stamp : chr  "44197" "44562.5" "30" NA
#>  $ mix   : chr  "10" "twenty" "TRUE" "61"
#>  $ words : chr  "TRUE" "false" "3.5" "abc"

# a sheet other than the first, by name or by position
multi <- system.file("extdata", "multisheet.xlsx", package = "rcxl")
read_xlsx(multi, sheet = "Beta")
#>   x
#> 1 2
```
