#' @importFrom rlang arg_match
#' 
#' @keywords internal
#' @noRd
.validate_convert_args <- function(a,
                                   call) {

  origin <- a$origin
  rlang::arg_match(
    origin,
    c("GTAPv6", "GTAPv7"),
    error_call = call
  )

  target <- a$target
  rlang::arg_match(
    target,
    c("GTAPv6", "GTAPv7"),
    error_call = call
  )

  checklist <- list(
    dat_input = "character",
    par_input = "character",
    set_input = "character",
    origin = "character",
    target = "character"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  a <- .data_inputs(a = a, call = call)
  return(a)
}