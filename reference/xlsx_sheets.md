# List worksheet names

List worksheet names

## Usage

``` r
xlsx_sheets(path)
```

## Arguments

- path:

  Path to an xlsx file. Tilde expansion is applied.

## Value

A character vector of sheet names in workbook order.

## Examples

``` r
xlsx_sheets(system.file("extdata", "multisheet.xlsx", package = "rcxl"))
#> [1] "Alpha" "Beta"  "Gamma"
```
