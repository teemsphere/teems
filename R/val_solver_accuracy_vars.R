#' @keywords internal
#' @noRd
.validate_solver_accuracy_vars <- function(a,
                                           call) {
  accuracy_vars <- a$accuracy_vars
  if (is.null(accuracy_vars)) {
    return(invisible(NULL))
  }
  if (!is.character(accuracy_vars) || length(accuracy_vars) == 0L || anyNA(accuracy_vars) ||
    !all(grepl("^[A-Za-z][A-Za-z0-9_@]*$", accuracy_vars)) || anyDuplicated(tolower(accuracy_vars)) > 0L) {
    .cli_action(solve_err$accuracy_vars_names,
      action = "abort",
      call = call
    )
  }
  if (!a$solution_method %in% c("Gragg", "Midpoint", "Euler") || length(a$steps) != 3L) {
    .cli_action(solve_err$accuracy_vars_method,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
