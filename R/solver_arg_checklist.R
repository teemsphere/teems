# the positional class checklist .check_arg_class() reads: the formals
# of ems_solve() in signature order, then the dot-passed Runge-Kutta
# controls, then the la* guesses and expert flags
#' @keywords internal
#' @noRd
.solver_arg_checklist <- function() {
  checklist <- c(
    list(
      cmf_path = "character",
      solution_method = "character",
      matrix_method = "character",
      n_subintervals = c("numeric", "integer"),
      steps = c("NULL", "numeric", "integer"),
      n_tasks = c("numeric", "integer"),
      n_threads = c("numeric", "integer"),
      precision = "character",
      verbosity = c("numeric", "integer"),
      suppress_outputs = "logical",
      terminal_run = "logical",
      assertions = "character",
      range_test_initial = "character",
      range_test_updated = "character",
      complementarity = c("NULL", "teems_complementarity"),
      # dot-passed Runge-Kutta controls sit after the formals in
      # args_list (ems_solve appends them; .check_arg_class is
      # positional)
      adaptive = "character",
      eps_tolerance = c("numeric", "integer"),
      max_retries = c("numeric", "integer"),
      retry_adjust = "numeric"
    ),
    # dot-passed la* initial guesses and expert flags follow the RK
    # controls, in .solver_extra_args() order
    .solver_extra_checklist()
  )
  return(checklist)
}
