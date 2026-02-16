#' @keywords internal
#' @noRd
.validate_compose_args <- function(a,
                                   checklist,
                                   call) {
  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  a$sol_prefix <- file.path(dirname(a$cmf_path), "out", "variables", "bin", "sol.")

  return(a)
}