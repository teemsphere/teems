#' @keywords internal
#' @noRd
.as01 <- function(x) {
  flag <- as.integer(isTRUE(x))
  return(flag)
}

#' @keywords internal
#' @noRd
.extra_cli_flags <- function(a) {
  flags <- c(
    if (!is.null(a$postsim)) {
      paste("-postsim", .as01(a$postsim))
    },
    if (!is.null(a$inmemory)) {
      paste("-inmemory", .as01(a$inmemory))
    },
    if (!is.null(a$fastrefac)) {
      paste("-fastrefac", .as01(a$fastrefac))
    },
    if (!is.null(a$refine)) {
      paste("-refine", .as01(a$refine))
    },
    if (!is.null(a$nsbbdblocks)) {
      paste("-nsbbdblocks", as.integer(a$nsbbdblocks))
    },
    if (!is.null(a$withmc66)) {
      paste("-withmc66", .as01(a$withmc66))
    },
    if (!is.null(a$smllthreads)) {
      paste("-smllthreads", as.integer(a$smllthreads))
    },
    if (!is.null(a$tempdir)) {
      paste("-tempdir", a$tempdir)
    },
    if (!is.null(a$nowrites)) {
      paste("-nowrites", .as01(a$nowrites))
    },
    if (!is.null(a$condest)) {
      paste("-condest", .as01(a$condest))
    },
    if (!is.null(a$jacdump)) {
      paste("-jacdump", .as01(a$jacdump))
    },
    if (!is.null(a$ma48_cntl2)) {
      paste("-ma48_cntl2", format(a$ma48_cntl2, digits = 15))
    },
    if (!is.null(a$ma48_cntl4)) {
      paste("-ma48_cntl4", format(a$ma48_cntl4, digits = 15))
    },
    if (!is.null(a$rk_chart)) {
      paste("-rkchart", a$rk_chart)
    },
    if (!is.null(a$rk_norm)) {
      paste("-rknorm", a$rk_norm)
    },
    if (!is.null(a$rk_controller)) {
      paste("-rkctrl", a$rk_controller)
    },
    if (!is.null(a$rk_scope)) {
      paste("-rkscope", a$rk_scope)
    },
    if (!is.null(a$rk_h0)) {
      paste("-rk_h0", format(a$rk_h0, digits = 15))
    },
    if (!is.null(a$rk_guard)) {
      paste("-rkguard", format(a$rk_guard, digits = 15))
    }
  )
  if (is.null(flags)) {
    return(NULL)
  }
  flags <- paste(flags, collapse = " ")
  return(flags)
}
