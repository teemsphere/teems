#' @keywords internal
#' @noRd
.solver_min_version <- "1.1.0"

#' @keywords internal
#' @noRd
.solver_version_cache <- new.env(parent = emptyenv())

#' @importFrom utils packageVersion
#' @keywords internal
#' @noRd
.check_solver_version <- function(image,
                                  call = NULL) {
  mode <- .o_version_check()
  if (mode %=% "off") {
    return(invisible(NULL))
  }
  cached <- .solver_version_cache[[image]]
  if (!is.null(cached)) {
    return(invisible(cached))
  }
  action <- c(if (mode %=% "abort") {
    "abort"
  } else {
    "warn"
  }, "inform")
  pkg_version <- as.character(utils::packageVersion("teems"))
  pkg_core <- .solver_version_core(pkg_version)
  pkg_major <- as.integer(pkg_core[[1, 1]])
  min_version <- .solver_min_version

  solver_version <- .solver_version_query(image)
  if (is.na(solver_version)) {
    .cli_action(solve_err$solver_version_none,
      action = action,
      call = call
    )
  } else {
    solver_core <- .solver_version_core(solver_version)
    if (as.integer(solver_core[[1, 1]]) != pkg_major) {
      .cli_action(solve_err$solver_version_major,
        action = action,
        call = call
      )
    } else if (solver_core < numeric_version(min_version)) {
      .cli_action(solve_err$solver_version_floor,
        action = action,
        call = call
      )
    }
  }
  assign(image, solver_version, envir = .solver_version_cache)
  return(invisible(solver_version))
}
