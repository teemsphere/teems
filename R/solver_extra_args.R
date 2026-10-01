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
    jacdump = NULL,
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
