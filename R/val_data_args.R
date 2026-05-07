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

  a$set_mappings <- .check_named_dots(a$set_mappings)
  if (isFALSE(a$set_mappings)) {
    .cli_action(data_err$no_name_mapping,
      action = "abort",
      call = call
    )
  }

  checklist <- list(
    dat_input = c("character", "list"),
    par_input = c("character", "list"),
    set_input = c("character", "list"),
    time_steps = c("NULL", "numeric", "integer"),
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
  return(a)
}