#' @keywords internal
#' @noRd
.solver_extra_checklist <- function() {
  checklist <- list(
    laA = c("NULL", "numeric", "integer"),
    laD = c("NULL", "numeric", "integer"),
    laDi = c("NULL", "numeric", "integer"),
    postsim = c("NULL", "logical"),
    inmemory = c("NULL", "logical"),
    fastrefac = c("NULL", "logical"),
    nsbbdblocks = c("NULL", "numeric", "integer"),
    withmc66 = c("NULL", "logical"),
    smllthreads = c("NULL", "numeric", "integer"),
    tempdir = c("NULL", "character"),
    nowrites = c("NULL", "logical"),
    condest = c("NULL", "logical"),
    jacdump = c("NULL", "logical"),
    ma48_cntl2 = c("NULL", "numeric"),
    ma48_cntl4 = c("NULL", "numeric"),
    rk_chart = c("NULL", "character"),
    rk_norm = c("NULL", "character"),
    rk_controller = c("NULL", "character"),
    rk_scope = c("NULL", "character"),
    rk_h0 = c("NULL", "numeric", "integer"),
    rk_guard = c("NULL", "numeric", "integer")
  )
  return(checklist)
}
