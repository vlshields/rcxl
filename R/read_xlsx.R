read_xlsx <- function(path, sheet = 1L, col_names = TRUE) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    if (!is.character(sheet)) sheet <- as.integer(sheet)
    out <- .Call(C_read_xlsx, path, sheet, isTRUE(col_names))
    n <- if (length(out)) length(out[[1L]]) else 0L
    names(out) <- make.unique(names(out), sep = "_")
    structure(out, class = "data.frame", row.names = c(NA_integer_, -n))
}

xlsx_sheets <- function(path) {
    path <- path.expand(path)
    if (!file.exists(path)) stop("file not found: ", path)
    .Call(C_sheet_names, path)
}
