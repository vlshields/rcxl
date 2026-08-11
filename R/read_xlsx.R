read_xlsx <- function(path, sheet = 1L, col_names = TRUE, trim_ws = TRUE,
                      range = NULL, skip = 0L, n_max = Inf, col_types = NULL) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    if (!is.null(range)) {
        pr <- parse_range(range)
        if (!is.null(pr$sheet)) sheet <- pr$sheet
        win <- pr$win
    } else
        win <- skip_window(skip, n_max)
    if (!is.character(sheet)) sheet <- as.integer(sheet)
    out <- .Call(C_read_xlsx, path, sheet, isTRUE(col_names), isTRUE(trim_ws),
                 win, parse_col_types(col_types))
    n <- if (length(out)) length(out[[1L]]) else 0L
    names(out) <- make.unique(names(out), sep = "_")
    structure(out, class = "data.frame", row.names = c(NA_integer_, -n))
}

read_xlsx_all <- function(path, sheets = NULL, col_names = TRUE,
                          trim_ws = TRUE, range = NULL, skip = 0L,
                          n_max = Inf, col_types = NULL) {
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
    out <- .Call(C_read_xlsx_all, path, sheets, isTRUE(col_names),
                 isTRUE(trim_ws), win, parse_col_types(col_types))
    lapply(out, function(cols) {
        n <- if (length(cols)) length(cols[[1L]]) else 0L
        names(cols) <- make.unique(names(cols), sep = "_")
        structure(cols, class = "data.frame", row.names = c(NA_integer_, -n))
    })
}

xlsx_sheets <- function(path) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    .Call(C_sheet_names, path)
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

# A1-style range: "B3:D87", "B3", "B:D", "3:87", optionally sheet-qualified
# ("Sheet1!B3:D87", "'My Sheet'!B3:D87").  A fixed axis is one the range
# bounds explicitly; the other keeps readxl's extent-trimming behavior.
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
