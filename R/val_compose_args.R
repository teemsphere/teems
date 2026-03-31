#' @importFrom rlang arg_match
#' 
#' @keywords internal
#' @noRd
.validate_compose_args <- function(a,
                                   call) {
  type <- a$type
  a$type <- rlang::arg_match(
    type,
    c("all", "variable", "coefficient"),
    error_call = call
  )

  if (a$minimal) {
    a$type <- "variable"
  }

  checklist <- list(
    cmf_path = "character",
    type = "character",
    name = c("NULL", "character"),
    minimal = "logical"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  a$sol_prefix <- file.path(dirname(a$cmf_path), "out", "variables", "bin", "sol.")

  return(a)
}