#' @keywords internal
#' @noRd
.check_int_headers <- function(coeff_extract,
                               call = call) {

  if (!.o_timestep_header() %in% coeff_extract$header) {
    timestep_header <- .o_timestep_header()
    header_descr <- "timestep header"
    arg_name <- "timestep_header"
    .cli_action(model_err$invalid_int_header,
      action = c("abort", "inform"),
      call = call
    )
  }

  if (!.o_n_timestep_header() %in% coeff_extract$header) {
    n_timestep_header <- .o_n_timestep_header()
    header_descr <- "number of timesteps header"
    arg_name <- "n_timestep_header"
    .cli_action(model_err$invalid_int_header,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}