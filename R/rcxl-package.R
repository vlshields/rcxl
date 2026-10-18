#' @details
#' By default, worksheets with at least 4 MB of uncompressed XML are parsed on
#' several threads and smaller ones are read serially. The worker count
#' defaults to the number of available cores, capped at 8. Set the
#' `RCXL_THREADS` environment variable to an explicit count to override both
#' the count and the size threshold. When `_R_CHECK_LIMIT_CORES_` is set, as it
#' is under `R CMD check`, no more than two workers are used whatever the
#' setting.
#'
#' @useDynLib rcxl, .registration = TRUE
#' @keywords internal
"_PACKAGE"
