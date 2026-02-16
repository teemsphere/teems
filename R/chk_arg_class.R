#' @importFrom purrr pwalk
#' @importFrom rlang cnd_signal
#' 
#' @keywords internal
#' @noRd
.check_arg_class <- function(args_list,
                             checklist,
                             call) {

  withCallingHandlers(
    purrr::pwalk(
      list(
        args_list,
        names(args_list),
        checklist,
        names(checklist)
      ),
      function(arg, arg_name, checks, check_name) {
        if (arg_name %!=% check_name) {
          .cli_action(
            msg = "Check missing from checklist",
            action = "abort",
            .internal = TRUE,
            call = call
          )
        }
        .check_class(
          arg = arg,
          arg_name = arg_name,
          check = checks,
          call = call
        )
      }
    ),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err[["parent"]])
    }
  )
}
