#' Render the expert flags onto the solver command line. Absent
#' (NULL) values emit nothing: the solver applies its own defaults
#' and records the effective values in sol.stats.json.
#'
#' @keywords internal
#' @noRd
.extra_cli_flags <- function(a) {
  as01 <- function(x) {
    flag <- as.integer(isTRUE(x))
    return(flag)
  }
  flags <- c(
    if (!is.null(a$postsim)) {
      paste("-postsim", as01(a$postsim))
    },
    if (!is.null(a$inmemory)) {
      paste("-inmemory", as01(a$inmemory))
    },
    if (!is.null(a$fastrefac)) {
      paste("-fastrefac", as01(a$fastrefac))
    },
    if (!is.null(a$gpzerodivide)) {
      paste("-gpzerodivide", as01(a$gpzerodivide))
    },
    if (!is.null(a$cntl_3)) {
      paste("-cntl_3", a$cntl_3)
    },
    if (!is.null(a$cntl_6)) {
      paste("-cntl_6", a$cntl_6)
    },
    if (!is.null(a$nsbbdblocks)) {
      paste("-nsbbdblocks", as.integer(a$nsbbdblocks))
    },
    if (!is.null(a$withmc66)) {
      paste("-withmc66", as01(a$withmc66))
    },
    if (!is.null(a$smllthreads)) {
      paste("-smllthreads", as.integer(a$smllthreads))
    },
    if (!is.null(a$tempdir)) {
      paste("-tempdir", a$tempdir)
    },
    if (!is.null(a$nowrites)) {
      paste("-nowrites", as01(a$nowrites))
    },
    if (!is.null(a$condest)) {
      paste("-condest", as01(a$condest))
    },
    if (!is.null(a$ma48u)) {
      paste("-ma48u", format(a$ma48u, digits = 15))
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
