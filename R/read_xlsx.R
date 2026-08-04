read_xlsx <- function(path, sheet = 1L, col_names = TRUE, trim_ws = TRUE) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    if (!is.character(sheet)) sheet <- as.integer(sheet)
    out <- .Call(C_read_xlsx, path, sheet, isTRUE(col_names), isTRUE(trim_ws))
    n <- if (length(out)) length(out[[1L]]) else 0L
    names(out) <- make.unique(names(out), sep = "_")
    structure(out, class = "data.frame", row.names = c(NA_integer_, -n))
}

read_xlsx_all <- function(path, sheets = NULL, col_names = TRUE, trim_ws = TRUE) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    if (!is.null(sheets)) {
        if (!is.character(sheets)) sheets <- as.integer(sheets)
        if (anyNA(sheets)) stop("'sheets' must be sheet names or indices")
    }
    out <- .Call(C_read_xlsx_all, path, sheets, isTRUE(col_names), isTRUE(trim_ws))
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
