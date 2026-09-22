#' @keywords internal
#' @noRd
.layer_require <- function(i_data, spec, call) {
  req <- c(spec$required$set, spec$required$par, spec$required$dat)
  missing_headers <- setdiff(req, toupper(names(i_data)))
  if (length(missing_headers) > 0L) {
    .cli_action(data_err[[paste0(spec$flag, "_incomplete")]],
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}
