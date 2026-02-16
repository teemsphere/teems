#' @keywords internal
#' @noRd
.validate_aux_args <- function(a,
                               call) {
  checklist <- list(
    dat = c("NULL", "list", "character"),
    par = c("NULL", "list", "character"),
    set = c("NULL", "list", "character")
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  return(a)
}