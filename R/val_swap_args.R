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
  
  a$subset <- .check_named_dots(a$subset)
  if (isFALSE(a$subset)) {
    .cli_action(shk_err$uni_named_lst,
                action = c("abort", "inform", "inform"),
                call = call
    )
  }

  if (a$subset %=% NA) {
    a$subset <- NULL
  }
  
  return(a)
}