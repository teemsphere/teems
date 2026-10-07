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
    nsbbdblocks = NULL,
    withmc66 = NULL,
    smllthreads = NULL,
    tempdir = NULL,
    nowrites = NULL,
    condest = NULL,
    jacdump = NULL,
    ma48_cntl2 = NULL,
    ma48_cntl4 = NULL,
    rk_chart = NULL,
    rk_norm = NULL,
    rk_controller = NULL,
    rk_scope = NULL,
    rk_h0 = NULL,
    rk_guard = NULL
  )
  return(defaults)
}
