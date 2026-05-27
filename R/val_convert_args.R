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

  if (origin %=% target) {
    .cli_action(convert_err$same_format,
      action = "abort",
      call = call
    )
  }

  checklist <- list(
    dat_har = "character",
    par_har = "character",
    set_har = "character",
    origin = "character",
    target = "character"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  names(a) <- gsub("_har", "_input", names(a))
  a <- .data_inputs(a = a, call = call)
  return(a)
}