#' Read an xlsx worksheet
#'
#' Reads one worksheet from an xlsx file into a data frame. Column types are
#' guessed from the cells unless overridden with `col_types`.
#'
#' A guessed column becomes character if it holds any string cell, logical if
#' it holds only booleans, `POSIXct` if it holds date cells and no plain
#' numbers, and numeric otherwise. Every cell in the read area informs the
#' guess. Columns with only blank cells become logical `NA`. Date cells are returned as `POSIXct` in UTC; both the 1900
#' and 1904 date systems are handled, including the nonexistent 29 Feb 1900
#' that the 1900 system counts.
#'
#' Cells that cannot be coerced to a type forced through `col_types` become
#' `NA`, with a warning giving the count and the first offending cell.
#'
#' @param path Path to an xlsx file. Tilde expansion is applied.
#' @param sheet Worksheet to read, as a 1-based index or a sheet name.
#'   Overridden when `range` names a sheet.
#' @param col_names `TRUE` to use the first row of the read area as column
#'   names, `FALSE` to number them `V1`, `V2`, ..., or a character vector of
#'   names, one per sheet column including any skipped via `col_types` (the
#'   skipped names are dropped from the result).
#' @param trim_ws Trim leading and trailing whitespace from string cells and
#'   column names?
#' @param range A1-style cell range to read: `"B3:D87"`, a single cell
#'   `"B3"`, whole columns `"B:D"`, or whole rows `"3:87"`, optionally
#'   sheet-qualified as `"Sheet1!B3:D87"` or `"'My Sheet'!B3:D87"`. When
#'   given, `skip` and `n_max` are ignored. An axis the range bounds is read
#'   exactly, blank cells included; an open axis is trimmed to the sheet
#'   extent.
#' @param skip Number of rows to skip before reading anything. Ignored when
#'   `range` is given.
#' @param n_max Maximum number of data rows to read. The column name row
#'   does not count. Ignored when `range` is given.
#' @param col_types `NULL` to guess every column, or a character vector of
#'   `"guess"`, `"skip"`, `"logical"`, `"numeric"`, `"date"`, `"text"` or
#'   `"list"`. A single value applies to every column; otherwise supply one
#'   entry per sheet column, counting `"skip"` columns. A `"list"` column
#'   keeps each cell's own type: length-one numeric, `POSIXct`, logical or
#'   character elements, with blank cells as logical `NA`.
#' @param na Character vector of strings to read as `NA`. Matched against
#'   whitespace-trimmed cell text; entries that parse as numbers also blank
#'   numeric and date cells holding that value.
#' @param name_repair How to resolve column names in the result. `"unique"`
#'   suffixes repeated names via [make.unique()] with separator `"_"`,
#'   `"minimal"` keeps names as read, `"check_unique"` errors on any repeat.
#' @return A `data.frame` with one column per worksheet column read.
#' @seealso [read_xlsx_all()] to read several sheets in one workbook pass;
#'   [xlsx_sheets()] for the sheet names.
#' @examples
#' types <- system.file("extdata", "types.xlsx", package = "rcxl")
#' str(read_xlsx(types))
#'
#' # read a rectangle rather than the whole sheet
#' read_xlsx(types, range = "A1:C3")
#'
#' # force every column to character instead of guessing
#' str(read_xlsx(types, col_types = "text"))
#'
#' # a sheet other than the first, by name or by position
#' multi <- system.file("extdata", "multisheet.xlsx", package = "rcxl")
#' read_xlsx(multi, sheet = "Beta")
#' @export
read_xlsx <- function(path, sheet = 1L, col_names = TRUE, trim_ws = TRUE,
                      range = NULL, skip = 0L, n_max = Inf, col_types = NULL,
                      na = "", name_repair = c("unique", "minimal",
                                               "check_unique")) {
    name_repair <- match.arg(name_repair)
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    if (!is.null(range)) {
        pr <- parse_range(range)
        if (!is.null(pr$sheet)) sheet <- pr$sheet
        win <- pr$win
    } else
        win <- skip_window(skip, n_max)
    if (!is.character(sheet)) sheet <- as.integer(sheet)
    out <- .Call(C_read_xlsx, path, sheet, parse_col_names(col_names),
                 isTRUE(trim_ws), win, parse_col_types(col_types),
                 parse_na(na))
    n <- if (length(out)) length(out[[1L]]) else 0L
    names(out) <- repair_names(names(out), name_repair)
    structure(out, class = "data.frame", row.names = c(NA_integer_, -n))
}

#' Read several worksheets in one pass
#'
#' Reads multiple sheets from an xlsx file through one shared workbook pass,
#' so the ZIP directory, shared strings and styles are parsed once. All
#' arguments other than `sheets` apply to every sheet read.
#'
#' @inheritParams read_xlsx
#' @param sheets Sheets to read, as a vector of 1-based indices or sheet
#'   names. `NULL` reads every sheet in workbook order.
#' @param range As in [read_xlsx()], except a sheet-qualified range is an
#'   error here; select sheets with `sheets`.
#' @param name_repair As in [read_xlsx()]. A `"check_unique"` error names
#'   the offending sheet.
#' @return A named list of `data.frame`s, one per sheet read, named by sheet
#'   name.
#' @examples
#' multi <- system.file("extdata", "multisheet.xlsx", package = "rcxl")
#' sheets <- read_xlsx_all(multi)
#' names(sheets)
#' sheets[["Alpha"]]
#'
#' # a subset of the sheets, still in one workbook pass
#' names(read_xlsx_all(multi, sheets = c("Alpha", "Gamma")))
#' @export
read_xlsx_all <- function(path, sheets = NULL, col_names = TRUE,
                          trim_ws = TRUE, range = NULL, skip = 0L,
                          n_max = Inf, col_types = NULL, na = "",
                          name_repair = c("unique", "minimal",
                                          "check_unique")) {
    name_repair <- match.arg(name_repair)
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    if (!is.null(sheets)) {
        if (!is.character(sheets)) sheets <- as.integer(sheets)
        if (anyNA(sheets)) stop("'sheets' must be sheet names or indices")
    }
    if (!is.null(range)) {
        pr <- parse_range(range)
        if (!is.null(pr$sheet))
            stop("use 'sheets' to select sheets in read_xlsx_all, not a ",
                 "sheet-qualified range")
        win <- pr$win
    } else
        win <- skip_window(skip, n_max)
    out <- .Call(C_read_xlsx_all, path, sheets, parse_col_names(col_names),
                 isTRUE(trim_ws), win, parse_col_types(col_types),
                 parse_na(na))
    Map(function(cols, sheet) {
        n <- if (length(cols)) length(cols[[1L]]) else 0L
        names(cols) <- repair_names(names(cols), name_repair, sheet)
        structure(cols, class = "data.frame", row.names = c(NA_integer_, -n))
    }, out, names(out))
}

#' List worksheet names
#'
#' @inheritParams read_xlsx
#' @return A character vector of sheet names in workbook order.
#' @examples
#' xlsx_sheets(system.file("extdata", "multisheet.xlsx", package = "rcxl"))
#' @export
xlsx_sheets <- function(path) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    .Call(C_sheet_names, path)
}


parse_col_names <- function(col_names) {
    if (!is.character(col_names)) return(isTRUE(col_names))
    if (anyNA(col_names)) stop("'col_names' must not contain NA")
    col_names
}

repair_names <- function(nms, name_repair, sheet = NULL) {
    if (name_repair == "minimal") return(nms)
    if (name_repair == "check_unique") {
        d <- unique(nms[duplicated(nms)])
        if (length(d))
            stop("duplicate column names",
                 if (!is.null(sheet)) paste0(" in sheet '", sheet, "'"),
                 ": ", paste0("'", d, "'", collapse = ", "))
        return(nms)
    }
    make.unique(nms, sep = "_")
}


parse_na <- function(na) {
    if (is.null(na)) return(character())
    if (!is.character(na)) na <- as.character(na)
    if (anyNA(na)) stop("'na' must not contain NA")
    na
}

# integer codes consumed by the C reader; order must match its COL_ enum
parse_col_types <- function(col_types) {
    if (is.null(col_types)) return(NULL)
    if (!is.character(col_types) || length(col_types) == 0L ||
        anyNA(col_types))
        stop("'col_types' must be a character vector of column types")
    types <- c("guess", "skip", "logical", "numeric", "date", "text", "list")
    i <- match(col_types, types)
    if (anyNA(i))
        stop("unknown col_types value(s): ",
             paste0("'", unique(col_types[is.na(i)]), "'", collapse = ", "),
             "; valid types are ",
             paste0("'", types, "'", collapse = ", "))
    as.integer(i - 1L)
}

# window vector consumed by the C reader:
# {row0, row1, col0, col1, fixed_rows, fixed_cols, n_max}
# 0-based half-open bounds, -1 for an open side or no n_max cap
open_window <- function() c(0L, -1L, 0L, -1L, 0L, 0L, -1L)

skip_window <- function(skip, n_max) {
    win <- open_window()
    skip <- as.integer(skip[1L])
    if (is.na(skip) || skip < 0L)
        stop("'skip' must be a single non-negative integer")
    win[1L] <- skip
    if (length(n_max) != 1L || !is.numeric(n_max) || is.na(n_max) || n_max < 0)
        stop("'n_max' must be a single non-negative number")
    if (n_max < .Machine$integer.max) win[7L] <- as.integer(n_max)
    win
}

col_index <- function(s) {
    v <- utf8ToInt(toupper(s)) - 64L
    Reduce(function(a, b) a * 26L + b, as.integer(v))
}


parse_range <- function(range) {
    if (!is.character(range) || length(range) != 1L || is.na(range))
        stop("'range' must be a single string like \"B3:D87\"")
    s <- range
    sheet <- NULL
    bang <- regexpr("!", s, fixed = TRUE)
    if (bang > 0L) {
        if (substr(s, 1L, 1L) == "'") {
            m <- regmatches(s, regexec("^'((?:[^']|'')+)'!(.+)$", s))[[1L]]
            if (length(m) != 3L) stop("malformed range: '", range, "'")
            sheet <- gsub("''", "'", m[2L], fixed = TRUE)
            s <- m[3L]
        } else {
            sheet <- substr(s, 1L, bang - 1L)
            s <- substr(s, bang + 1L, nchar(s))
        }
    }
    s <- gsub("$", "", s, fixed = TRUE)

    cell <- "([A-Za-z]{1,3})([0-9]+)"
    win <- open_window()
    set_rows <- function(a, b) {
        r <- sort(c(as.integer(a), as.integer(b)))
        if (r[1L] < 1L || r[2L] > 1048576L)
            stop("range rows must be within 1..1048576")
        win[c(1L, 2L, 5L)] <<- c(r[1L] - 1L, r[2L], 1L)
    }
    set_cols <- function(a, b) {
        v <- sort(c(col_index(a), col_index(b)))
        if (v[2L] > 16384L) stop("range columns must be within A..XFD")
        win[c(3L, 4L, 6L)] <<- c(v[1L] - 1L, v[2L], 1L)
    }

    m <- regmatches(s, regexec(paste0("^", cell, ":", cell, "$"), s))[[1L]]
    if (length(m)) {
        set_rows(m[3L], m[5L])
        set_cols(m[2L], m[4L])
        return(list(win = win, sheet = sheet))
    }
    m <- regmatches(s, regexec(paste0("^", cell, "$"), s))[[1L]]
    if (length(m)) {
        set_rows(m[3L], m[3L])
        set_cols(m[2L], m[2L])
        return(list(win = win, sheet = sheet))
    }
    m <- regmatches(s, regexec("^([A-Za-z]{1,3}):([A-Za-z]{1,3})$", s))[[1L]]
    if (length(m)) {
        set_cols(m[2L], m[3L])
        return(list(win = win, sheet = sheet))
    }
    m <- regmatches(s, regexec("^([0-9]+):([0-9]+)$", s))[[1L]]
    if (length(m)) {
        set_rows(m[2L], m[3L])
        return(list(win = win, sheet = sheet))
    }
    stop("malformed range: '", range, "'")
}
