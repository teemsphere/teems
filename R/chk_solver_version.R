#' Solver version handshake (release plan 1.1.0, platform-gate finding 2)
#'
#' The solver surfaces one version constant through `teems-solver
#' -version` (exactly `teems-solver <version>`, exit 0, answered before
#' MPI starts). The package asks the image once per session and holds
#' the image to a contract: the MAJOR is the interface version (a
#' 1.x package drives a 1.x solver), and a package release may require
#' a minimum solver (`.solver_min_version`: the versioned interface
#' itself and the coefficient dump arrived with 1.1.0). A pre-1.1 image
#' does not answer at all -- that silence IS the old-image detection.
#' Minor/patch skew inside the contract is not reported (the image
#' label is a hint, the binary's answer is ground truth, and pinned old
#' images are a stated reproducibility value: `ems_option_set(
#' version_check = "warn"/"off")` is the escape hatch).
#'
#' @keywords internal
#' @noRd
.solver_min_version <- "1.1.0"

#' @keywords internal
#' @noRd
.solver_version_cache <- new.env(parent = emptyenv())

#' Ask an image for its solver version. `NA_character_` when the binary
#' gives no `teems-solver <version>` line (an image that predates the
#' flag runs the solver proper instead: PETSc's own `-version` banner,
#' then `Error: cannot open ./reg.cmf`).
#'
#' @keywords internal
#' @noRd
.solver_version_query <- function(image) {
  out <- tryCatch(
    suppressWarnings(system2("docker",
      c(
        "run", "--rm", "--entrypoint", "/opt/teems-solver/solver/teems-solver",
        image, "-version"
      ),
      stdout = TRUE, stderr = TRUE
    )),
    error = function(e) character(0)
  )
  hit <- regmatches(out, regexpr("^teems-solver [0-9]+\\.[0-9]+\\.[0-9]+\\S*", out))
  if (!length(hit)) {
    return(NA_character_)
  }
  return(sub("^teems-solver ", "", hit[1]))
}

#' `MAJOR.MINOR.PATCH` of a version string, pre-release suffix
#' (`-dev.4`) and R's development component (`.9000`) dropped: a dev
#' build of 1.1.0 carries the 1.1.0 interface.
#'
#' @keywords internal
#' @noRd
.solver_version_core <- function(x) {
  core <- regmatches(x, regexpr("^[0-9]+\\.[0-9]+\\.[0-9]+", x))
  if (!length(core)) {
    return(NA)
  }
  return(numeric_version(core))
}

#' @keywords internal
#' @noRd
.check_solver_version <- function(image,
                                  call = NULL) {
  mode <- ems_options$get_version_check()
  if (mode %=% "off") {
    return(invisible(NULL))
  }
  cached <- .solver_version_cache[[image]]
  if (!is.null(cached)) {
    return(invisible(cached))
  }
  action <- c(if (mode %=% "abort") "abort" else "warn", "inform")
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
  # one query per image per session; a warned-through image is not
  # asked (or warned about) again
  assign(image, solver_version, envir = .solver_version_cache)
  return(invisible(solver_version))
}
