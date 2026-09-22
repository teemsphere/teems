#' @importFrom rlang is_integerish arg_match
#' @keywords internal
#' @noRd
.validate_solver_scalars <- function(a,
                                     call) {
  checklist <- .solver_arg_checklist()
  if (!is.null(a$complementarity) &&
    !inherits(a$complementarity, "teems_complementarity")) {
    .cli_action(solve_err$comp_spec_class,
      action = c("abort", "inform"),
      call = call
    )
  }
  if (!rlang::is_integerish(a$n_threads) || length(a$n_threads) != 1L ||
    a$n_threads < 1) {
    bad_arg <- "n_threads"
    requirement <- solve_err$requirement$positive_int
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!rlang::is_integerish(a$max_retries) || length(a$max_retries) != 1L ||
    a$max_retries < 1) {
    bad_arg <- "max_retries"
    requirement <- solve_err$requirement$positive_int
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.numeric(a$retry_adjust) || length(a$retry_adjust) != 1L ||
    is.na(a$retry_adjust) ||
    a$retry_adjust <= 0 || a$retry_adjust >= 1) {
    bad_arg <- "retry_adjust"
    requirement <- solve_err$requirement$open_unit
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  .validate_solver_extras(a = a, call = call)

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  if (!rlang::is_integerish(a$n_tasks)) {
    arg <- "n_tasks"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$n_tasks)) %!=% 1L) {
    arg <- "n_tasks"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  if (!rlang::is_integerish(a$n_subintervals)) {
    arg <- "n_subintervals"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$n_subintervals)) %!=% 1L) {
    arg <- "n_subintervals"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  {
    if (!rlang::is_integerish(a$verbosity)) {
      arg <- "verbosity"
      .cli_action(solve_err$x_integerish,
        action = "abort",
        call = call
      )
    }
    if (as.integer(length(a$verbosity)) %!=% 1L) {
      arg <- "verbosity"
      .cli_action(solve_err$invalid_length,
        action = "abort",
        call = call
      )
    }
    if (!a$verbosity %in% c(0, 1, 2)) {
      .cli_action(solve_err$verbosity_range,
        action = "abort",
        call = call
      )
    }
  }
  return(a)
}
