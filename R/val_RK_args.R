#' @importFrom rlang arg_match
#' @keywords internal
#' @noRd
.validate_RK_args <- function(a,
                              call) {
  solution_method <- a$solution_method
  a$solution_method <- rlang::arg_match(
    arg = solution_method,
    values = c("DoPri54", "BoSha32", "RK4", "Heun", "RK2"),
    error_call = call
  )
  chart <- a$chart
  a$chart <- rlang::arg_match(
    arg = chart,
    values = c("log", "percent"),
    error_call = call
  )
  error_norm <- a$error_norm
  a$error_norm <- rlang::arg_match(
    arg = error_norm,
    values = c("max", "rms"),
    error_call = call
  )
  controller <- a$controller
  a$controller <- rlang::arg_match(
    arg = controller,
    values = c("std", "pi"),
    error_call = call
  )
  scope <- a$scope
  a$scope <- rlang::arg_match(
    arg = scope,
    values = c("pct", "all"),
    error_call = call
  )
  rk_dots <- intersect(names(a$solve_args), names(.rk_control_formals))
  if (length(rk_dots) > 0L) {
    rk_formals <- unname(.rk_control_formals[rk_dots])
    .cli_action(solve_err$rk_control_dots,
      action = "abort",
      call = call
    )
  }
  if (is.null(a$adaptive)) {
    a$adaptive <- if (a$solution_method %in% c("BoSha32", "DoPri54")) {
      "yes"
    } else {
      "no"
    }
  }
  return(a)
}

#' @keywords internal
#' @noRd
.rk_control_formals <- c(
  rk_chart = "chart",
  rk_norm = "error_norm",
  rk_controller = "controller",
  rk_scope = "scope",
  rk_h0 = "h_init",
  rk_guard = "guard"
)
