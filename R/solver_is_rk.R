#' @keywords internal
#' @noRd
.solver_is_rk <- function(method) {
  rk <- method %in% c("RK2", "Heun", "RK4", "BoSha32", "DoPri54")
  return(rk)
}

#' @keywords internal
#' @noRd
.solver_is_rk_embedded <- function(method) {
  embedded <- method %in% c("BoSha32", "DoPri54")
  return(embedded)
}
