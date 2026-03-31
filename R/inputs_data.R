#' @noRd
#' @keywords internal
.data_inputs <- function(a,
                         call) {
  if (!is.null(a$dat_input)) {
    a$dat_input <- .check_input(a$dat_input,
      valid_ext = "har",
      call = call
    )
  }

  if (!is.null(a$par_input)) {
    a$par_input <- .check_input(a$par_input,
      valid_ext = "har",
      call = call
    )
  }

  if (!is.null(a$set_input)) {
    a$set_input <- .check_input(a$set_input,
      valid_ext = "har",
      call = call
    )
  }

  if (!is.null(a$target_format)) {
    valid_formats <- c("GTAPv6", "GTAPv7")
    if (!a$target_format %in% valid_formats) {
      .cli_action(data_err$invalid_target,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  return(a)
}