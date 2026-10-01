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
    gpzerodivide = c("NULL", "logical"),
    cntl_3 = c("NULL", "numeric"),
    cntl_6 = c("NULL", "numeric"),
    nsbbdblocks = c("NULL", "numeric", "integer"),
    withmc66 = c("NULL", "logical"),
    smllthreads = c("NULL", "numeric", "integer"),
    tempdir = c("NULL", "character"),
    nowrites = c("NULL", "logical"),
    condest = c("NULL", "logical"),
    jacdump = c("NULL", "logical"),
    ma48u = c("NULL", "numeric"),
    rk_chart = c("NULL", "character"),
    rk_norm = c("NULL", "character"),
    rk_controller = c("NULL", "character"),
    rk_scope = c("NULL", "character"),
    rk_h0 = c("NULL", "numeric", "integer"),
    rk_guard = c("NULL", "numeric", "integer")
  )
  return(checklist)
}
