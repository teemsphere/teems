#' @importFrom rlang is_integerish
#' @keywords internal
#' @noRd
.validate_solver_extras <- function(a, call) {
  for (nme in c("laA", "laD", "laDi", "nsbbdblocks", "smllthreads")) {
    x <- a[[nme]]
    if (!is.null(x) &&
      (!rlang::is_integerish(x) || length(x) != 1L || is.na(x) || x < 1)) {
      bad_arg <- nme
      requirement <- solve_err$requirement$positive_int
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
      requirement <- solve_err$requirement$logical_flag
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
      requirement <- solve_err$requirement$numeric_scalar
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  if (!is.null(a$ma48u) &&
    (!is.numeric(a$ma48u) || length(a$ma48u) != 1L || is.na(a$ma48u) ||
      a$ma48u <= 0 || a$ma48u > 1)) {
    bad_arg <- "ma48u"
    requirement <- solve_err$requirement$half_open_unit
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$tempdir) &&
    (!is.character(a$tempdir) || length(a$tempdir) != 1L || is.na(a$tempdir))) {
    bad_arg <- "tempdir"
    requirement <- solve_err$requirement$character_scalar
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
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
        solve_err$requirement$one_of,
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
    requirement <- solve_err$requirement$half_open_unit
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$rk_guard) &&
    (!is.numeric(a$rk_guard) || length(a$rk_guard) != 1L || is.na(a$rk_guard) ||
      a$rk_guard <= 1)) {
    bad_arg <- "rk_guard"
    requirement <- solve_err$requirement$level_ratio
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}
