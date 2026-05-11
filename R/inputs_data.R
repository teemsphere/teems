#' @noRd
#' @keywords internal
.data_inputs <- function(a,
                         call) {
  if (!is.null(a$dat_input) && !is.list(a$dat_input)) {
    a$dat_input <- .check_input(a$dat_input,
      valid_ext = "har",
      call = call
    )
  }

  if (!is.null(a$par_input) && !is.list(a$par_input)) {
    a$par_input <- .check_input(a$par_input,
      valid_ext = "har",
      call = call
    )
  }

  if (!is.null(a$set_input) && !is.list(a$set_input)) {
    a$set_input <- .check_input(a$set_input,
      valid_ext = "har",
      call = call
    )
  }

  return(a)
}