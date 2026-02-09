#' @importFrom purrr pluck
#' @importFrom rlang arg_match
#' 
#' @noRd
#' @keywords internal
.validate_data_args <- function(a,
                                call) {
  a$... <- NULL
  if (length(a$set_mappings) %=% 0L) {
    .cli_action(load_err$missing_set_mappings,
                action = "abort",
                call = call
    )
  }

  checklist <- list(
    dat_input = "character",
    par_input = "character",
    set_input = "character",
    time_steps = c("NULL", "numeric"),
    aux_input = c("NULL", "list"),
    target_format = c("NULL", "character"),
    set_mappings = "list"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  a$dat_input <- .check_input(a$dat_input,
    valid_ext = "har",
    call = call
  )

  a$par_input <- .check_input(a$par_input,
    valid_ext = "har",
    call = call
  )

  a$set_input <- .check_input(a$set_input,
    valid_ext = "har",
    call = call
  )
  
  if (!is.null(a$target_format)) {
    valid_formats <- c("GTAPv6", "GTAPv7")
    if (!a$target_format %in% valid_formats) {
      .cli_action(invalid_target,
                  action = c("abort", "inform"),
                  call = call)
    }
  }

  if (!is.null(a$tab_file)) {
    a$tab_file <- .check_input(
      file = a$tab_file,
      valid_ext = "tab",
      call = call
    )
  }

  return(a)
}