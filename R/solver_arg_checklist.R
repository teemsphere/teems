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
      complementarity = c("NULL", "teems_complementarity"),
      adaptive = "character",
      eps_tolerance = c("numeric", "integer"),
      max_retries = c("numeric", "integer"),
      retry_adjust = "numeric"
    ),
    .solver_extra_checklist()
  )
  return(checklist)
}
