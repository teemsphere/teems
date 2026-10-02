#' @keywords internal
#' @noRd
.solver_method_codes <- function(a,
                                 call) {
  is_rk <- .solver_is_rk(a$solution_method)
  if (a$matrix_method %in% c("SBBD", "NDBBD") && !a$enable_time) {
    matrix_method <- a$matrix_method
    .cli_action(solve_err$invalid_method,
      action = "abort",
      call = call
    )
  }

  a$matsol <- switch(
    EXPR = a$matrix_method,
    "LU" = 0,
    "SBBD" = 1,
    "DBBD" = 2,
    "NDBBD" = 3
  )

  if (a$solution_method %in% c("Gragg", "Midpoint", "Euler") || is_rk) {
    a$solmed <- a$solution_method
  } else {
    a$solmed <- "Johansen"
    a$n_subintervals <- 1
  }
  return(a)
}
