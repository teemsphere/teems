#' Scalar/value checks for the extras (classes are checked
#' positionally by `.check_arg_class`).
#'
#' @importFrom rlang is_integerish
#'
#' @keywords internal
#' @noRd
.validate_solver_extras <- function(a, call) {
  for (nme in c("laA", "laD", "laDi", "nsbbdblocks", "smllthreads")) {
    x <- a[[nme]]
    if (!is.null(x) &&
      (!rlang::is_integerish(x) || length(x) != 1L || is.na(x) || x < 1)) {
      bad_arg <- nme
      requirement <- "a positive integer-like numeric of length 1"
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  for (nme in c("postsim", "inmemory", "fastrefac", "gpzerodivide", "withmc66", "nowrites", "condest")) {
    x <- a[[nme]]
    if (!is.null(x) && (!is.logical(x) || length(x) != 1L || is.na(x))) {
      bad_arg <- nme
      requirement <- "a non-missing logical of length 1"
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  for (nme in c("cntl_3", "cntl_6")) {
    x <- a[[nme]]
    if (!is.null(x) && (!is.numeric(x) || length(x) != 1L || is.na(x))) {
      bad_arg <- nme
      requirement <- "a numeric of length 1"
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  # MA48/MP48 pivot threshold CNTL(2): absent = each library's default
  # (MA48 0.1, HSL_MP48 0.01); the solver validates the same range
  if (!is.null(a$ma48u) &&
    (!is.numeric(a$ma48u) || length(a$ma48u) != 1L || is.na(a$ma48u) ||
      a$ma48u <= 0 || a$ma48u > 1)) {
    bad_arg <- "ma48u"
    requirement <- "a numeric of length 1 in (0, 1]"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$tempdir) &&
    (!is.character(a$tempdir) || length(a$tempdir) != 1L || is.na(a$tempdir))) {
    bad_arg <- "tempdir"
    requirement <- "a character of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  # Runge-Kutta run controls (ems_RK() is the documented front end):
  # the state chart, the accept-test norm, the step controller, the
  # initial step and the log-chart level-ratio guard
  rk_choices <- list(
    rk_chart = c("log", "percent"),
    rk_norm = c("max", "rms"),
    rk_controller = c("std", "pi"),
    rk_scope = c("pct", "all")
  )
  for (nme in names(rk_choices)) {
    x <- a[[nme]]
    if (!is.null(x) &&
      (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% rk_choices[[nme]])) {
      bad_arg <- nme
      requirement <- sprintf(
        "one of %s",
        paste(sprintf('"%s"', rk_choices[[nme]]), collapse = ", ")
      )
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  if (!is.null(a$rk_h0) &&
    (!is.numeric(a$rk_h0) || length(a$rk_h0) != 1L || is.na(a$rk_h0) ||
      a$rk_h0 <= 0 || a$rk_h0 > 1)) {
    bad_arg <- "rk_h0"
    requirement <- "a numeric of length 1 in (0, 1]"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$rk_guard) &&
    (!is.numeric(a$rk_guard) || length(a$rk_guard) != 1L || is.na(a$rk_guard) ||
      a$rk_guard <= 1)) {
    bad_arg <- "rk_guard"
    requirement <- "a numeric of length 1 greater than 1 (a level ratio)"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}
