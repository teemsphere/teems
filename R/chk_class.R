#' @keywords internal
#' @noRd
.check_class <- function(arg,
                         arg_name,
                         check,
                         call) {
  if (!inherits(arg, check)) {
    .cli_action(
      gen_err$class,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}