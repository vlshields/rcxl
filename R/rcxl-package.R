#' @details
#' Worksheets of at least 4 MB are parsed on several threads; smaller ones are
#' read serially. The worker count defaults to the number of available cores,
#' capped at 8. Set the `RCXL_THREADS` environment variable to an explicit
#' count to override that. When `_R_CHECK_LIMIT_CORES_` is set, as it is under
#' `R CMD check`, no more than two workers are used whatever the setting.
#'
#' @useDynLib rcxl, .registration = TRUE
#' @keywords internal
"_PACKAGE"
