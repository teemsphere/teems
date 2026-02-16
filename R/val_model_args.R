#' @keywords internal
#' @noRd
.validate_model_args <- function(a,
                                 checklist,
                                 call) {
  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )
  
  a$var_omit <- tolower(a$var_omit)

  a$model_input <- .check_input(
    file = a$model_input,
    valid_ext = "tab",
    call = call
  )

  if (!is.null(a$closure_file)) {
    a$closure_file <- .check_input(
      file = a$closure_file,
      valid_ext = "cls",
      call = call
    )
  }

  a$closure <- .load_closure(
    closure_file = a$closure_file,
    model_input = a$model_input,
    call = call
  )

  return(a)
}