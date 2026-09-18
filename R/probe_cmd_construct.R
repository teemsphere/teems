#' @description The probe run is always sequential (`-n 1`): the
#'   MC79 diagnosis is serial solver-side and skips itself on more
#'   ranks. The matrix method is irrelevant to the diagnosis, and the
#'   solver's probe measures the system structure (chain dimension,
#'   partition candidates) regardless of the `-matsol` passed.
#' @keywords internal
#' @noRd
.construct_probe_cmd <- function(paths,
                                 timeID,
                                 fine,
                                 extra = NULL) {
  docker_preamble <- paste(
    "docker run --rm --mount",
    # quoted for the platform's shell: an unquoted --mount value split on
    # the first space in the user's path, and the failure surfaced as a
    # bare connection error naming neither the path nor docker
    .shell_quote(paste("type=bind", paste0("src=", paths$run), "dst=/opt/teems", sep = ",")),
    paste0("teems", ":", .resolve_docker_tag()),
    "/bin/bash -c"
  )
  exec_preamble <- paste(
    docker_preamble,
    '"/opt/teems-solver/lib/mpi/bin/mpiexec',
    "-n", 1L,
    "/opt/teems-solver/solver/teems-solver",
    "-cmdfile", paths$docker_cmf
  )
  docker_diagnostic_out <- file.path(
    paths$docker_run, "out",
    paste0("solver_out", "_", timeID, ".txt")
  )
  solver_param <- paste(
    "-matsol", 0L,
    "-nsubints", 1L,
    "-solmed", "probe",
    "-probefine", as.integer(fine),
    "-maxthreads", 1L,
    "-nox"
  )
  if (!is.null(extra)) {
    # the la* guesses ride explicitly (the probe factorizes the same
    # condensed system); the remaining flags render as in ems_solve
    la_flags <- paste(c(
      if (!is.null(extra$laA)) {
        paste("-laA", as.integer(extra$laA))
      },
      if (!is.null(extra$laD)) {
        paste("-laD", as.integer(extra$laD))
      },
      if (!is.null(extra$laDi)) {
        paste("-laDi", as.integer(extra$laDi))
      }
    ), collapse = " ")
    extra_flags <- .extra_cli_flags(extra)
    for (flags in c(la_flags, extra_flags)) {
      if (!is.null(flags) && nzchar(flags)) {
        solver_param <- paste(solver_param, flags)
      }
    }
  }
  solver_out <- paste("2>&1 | tee", paste0(docker_diagnostic_out, "\""))
  cmd <- paste(exec_preamble, solver_param, solver_out)
  return(cmd)
}
