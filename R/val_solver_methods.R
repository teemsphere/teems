#' @importFrom rlang arg_match
#' @keywords internal
#' @noRd
.validate_solver_methods <- function(a,
                                     call) {
  solution_method <- a$solution_method
  a$solution_method <- rlang::arg_match(
    arg = solution_method,
    values = c("Gragg", "Johansen", "Euler", "RK2", "Heun", "RK4", "BoSha32", "DoPri54"),
    error_call = call
  )
  is_rk <- a$solution_method %in% c("RK2", "Heun", "RK4", "BoSha32", "DoPri54")
  is_rk_embedded <- a$solution_method %in% c("BoSha32", "DoPri54")

  if (is.null(a$steps)) {
    a$steps <- if (is_rk) {
      4L
    } else {
      c(2L, 4L, 8L)
    }
  }

  adaptive <- a$adaptive
  a$adaptive <- rlang::arg_match(
    arg = adaptive,
    values = c("no", "yes", "accuracy-only"),
    error_call = call
  )
  
  matrix_method <- a$matrix_method
  a$matrix_method <- rlang::arg_match(
    arg = matrix_method,
    values = c("LU", "DBBD", "SBBD", "NDBBD"),
    error_call = call
  )

  precision <- a$precision
  a$precision <- rlang::arg_match(
    arg = precision,
    values = c("single", "double"),
    error_call = call
  )
  return(a)
}
