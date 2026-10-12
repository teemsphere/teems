#' @keywords internal
#' @noRd
.construct_probe_cmd <- function(paths,
                                 timeID,
                                 fine,
                                 extra = NULL) {
  docker_preamble <- .docker_run_preamble(timeID = timeID, run_dir = paths$run)
  exec_preamble <- paste(
    docker_preamble,
    '"set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec',
    "-n", 1L,
    "/opt/teems-solver/solver/teems-solver",
    "-cmdfile", paths$docker_cmf
  )
  docker_diagnostic_out <- paths$docker_diag_out %|||% file.path(
    paths$docker_run, "out",
    paste0("solver_out", "_", timeID, ".txt")
  )
  solver_param <- paste(
    "-matsol", 0L,
    "-nsubints", 1L,
    "-solmed", "probe",
    "-probefine", as.integer(fine),
    "-maxthreads", 1L,
    "-nowrites", 1L,
    "-cofdump", 0L,
    "-nox",
    if (.solver_uses_random(paths)) {
      paste("-random_seed", .o_random_seed())
    }
  )
  if (!is.null(extra)) {
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
