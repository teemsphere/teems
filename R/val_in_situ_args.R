#' @keywords internal
#' @noRd
.validate_in_situ_args <- function(a,
                                   call) {
  if (length(a$input_files) %=% 0L) {
    .cli_action(solve_err$no_insitu_inputs,
      action = "abort",
      call = call
    )
  }
  .check_class(
    arg = a$ignore_condense,
    arg_name = "ignore_condense",
    check = "logical",
    call = call
  )
  if (length(a$ignore_condense) != 1L || is.na(a$ignore_condense)) {
    bad_arg <- "ignore_condense"
    .cli_action(gen_err$logical_flag,
      action = "abort",
      call = call
    )
  }
  solver_args <- a$solver_args
  if (!is.null(solver_args)) {
    if (!is.list(solver_args) || length(solver_args) == 0L ||
      is.null(names(solver_args)) || !all(nzchar(names(solver_args)))) {
      .cli_action(solve_err$solver_args_list,
        action = c("abort", "inform"),
        call = call
      )
    }
    unknown_args <- setdiff(
      names(solver_args),
      c(names(.solver_extra_args()), names(.rk_step_controls()))
    )
    if (length(unknown_args)) {
      .cli_action(solve_err$solver_args_unknown,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  return(a)
}
