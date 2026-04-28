#' @noRd
#' @keywords internal
.validate_data_args <- function(a,
                                call) {
  a[["..."]] <- NULL

  if (length(a$set_mappings) %=% 0L) {
    .cli_action(data_err$missing_set_mappings,
      action = "abort",
      call = call
    )
  }

  checklist <- list(
    dat_input = "character",
    par_input = "character",
    set_input = "character",
    time_steps = c("NULL", "numeric", "integer"),
    target_format = c("NULL", "character"),
    set_mappings = "list"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  checklist <- lapply(a$set_mappings, \(c) c("character", "data.frame"))

  .check_arg_class(
    args_list = a$set_mappings,
    checklist = checklist,
    call = call
  )

  a <- .data_inputs(a = a, call = call)

  if (!is.null(a$tab_file)) {
    a$tab_file <- .check_input(
      file = a$tab_file,
      valid_ext = "tab",
      call = call
    )
  }

  return(a)
}