#' @keywords internal
#' @noRd
.validate_swap_args <- function(a,
                                checklist,
                                call) {
  a[["..."]] <- NULL
  
  checklist <- list(
    var = "character",
    subset = "list"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )
  
  if (length(a$subset) %=% 0L) {
    a$subset <- NULL
  }
  
  return(a)
}