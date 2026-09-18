#' Named solver arguments accepted beyond the formals: the MA48
#' workspace initial guesses and the expert solver flags formerly
#' passed as raw `append_args` strings. `ems_solve()` and
#' `ems_probe()` take them through `...`; `solve_in_situ()` through
#' `solver_args` (its dots carry the input files). All are validated
#' against this table -- a typo is an R-side error, never a silently
#' ignored solver flag.
#'
#' @keywords internal
#' @noRd
.solver_extra_args <- function() {
  defaults <- list(
    laA = NULL,
    laD = NULL,
    laDi = NULL,
    postsim = NULL,
    inmemory = NULL,
    fastrefac = NULL,
    gpzerodivide = NULL,
    cntl_3 = NULL,
    cntl_6 = NULL,
    nsbbdblocks = NULL,
    withmc66 = NULL,
    smllthreads = NULL,
    tempdir = NULL,
    nowrites = NULL,
    condest = NULL,
    ma48u = NULL,
    rk_chart = NULL,
    rk_norm = NULL,
    rk_controller = NULL,
    rk_scope = NULL,
    rk_h0 = NULL,
    rk_guard = NULL
  )
  return(defaults)
}
