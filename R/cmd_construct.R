#' Quote one argument for the shell `system()` hands the command to:
#' cmd.exe on Windows, the POSIX shell elsewhere. Used for the docker
#' `--mount` value, whose `src=` carries the user's own path.
#'
#' @keywords internal
#' @noRd
.shell_quote <- function(x, os = .Platform$OS.type) {
  quoted <- shQuote(x, type = if (os %=% "windows") {
    "cmd"
  } else {
    "sh"
  })
  return(quoted)
}

#' @importFrom cli cli_verbatim cli_ol
#'
#' @keywords internal
#' @noRd
.construct_cmd <- function(paths,
                           terminal_run,
                           timeID,
                           n_tasks,
                           n_subintervals,
                           solmed,
                           matsol,
                           steps,
                           adaptive = "no",
                           eps_tolerance = 0.01,
                           max_retries = 3L,
                           retry_adjust = 0.5,
                           n_threads = 1L,
                           precision = "single",
                           verbosity = 1L,
                           assertions = NULL,
                           range_test_initial = NULL,
                           range_test_updated = NULL,
                           complementarity = NULL,
                           laA = NULL,
                           laD = NULL,
                           laDi = NULL,
                           extra_flags = NULL) {
  docker_preamble <- paste(
    "docker run --rm --mount",
    # quoted for the platform's shell: an unquoted --mount value split on
    # the first space in the user's path, and the failure surfaced as a
    # bare connection error naming neither the path nor docker
    .shell_quote(paste("type=bind", paste0("src=", paths$run), "dst=/opt/teems", sep = ",")),
    paste0("teems", ":", .resolve_docker_tag()),
    "/bin/bash -c"
  )
  # precision = "double" selects the f64 coefficient-storage binary
  # shipped alongside the default in the same image
  solver_bin <- if (precision %=% "double") {
    "/opt/teems-solver/solver/teems-solver-f64"
  } else {
    "/opt/teems-solver/solver/teems-solver"
  }
  # pipefail: the pipeline's status is the solver's, not tee's
  exec_preamble <- paste(
    docker_preamble,
    '"set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec',
    "-n", n_tasks,
    solver_bin,
    "-cmdfile", paths$docker_cmf
  )

  docker_diagnostic_out <- file.path(paths$docker_run, "out", paste0("solver_out", "_", timeID, ".txt"))
  # la* initial guesses: user-passed > previous run's recorded la_used
  # > package cold defaults; the solver grows the workspace itself on
  # a too-small guess, so these only set the starting size
  la <- .resolve_la_args(
    laA = laA,
    laD = laD,
    laDi = laDi,
    cmf_path = paths$cmf
  )
  solver_param <- paste(
    "-matsol", matsol,
    if (solmed %in% c("Gragg", "Euler")) {
      paste("-step1", steps[1], "-step2", steps[2], "-step3", steps[3])
    },
    if (solmed %in% c("RK2", "Heun", "RK4", "BoSha32", "DoPri54")) {
      paste("-step1", steps[1])
    },
    if (solmed %in% c("BoSha32", "DoPri54") && adaptive != "no") {
      paste("-adaptive", adaptive, "-epstol", eps_tolerance)
    },
    "-nsubints", n_subintervals,
    "-solmed", solmed,
    "-laA", la$laA,
    "-laDi", la$laDi,
    "-laD", la$laD,
    paste("-verbosity", as.integer(verbosity)),
    paste("-maxthreads", as.integer(n_threads)),
    "-nox"
  )

  # run-mode switches, always rendered so the command records the run
  # (the R defaults are the solver's; effective values also land in
  # sol.stats.json and model_diagnostics.txt)
  mode_flag <- function(flag, x) {
    flag <- paste(flag, c(off = 0L, warn = 1L, fatal = 2L)[[x]])
    return(flag)
  }
  solver_param <- paste(c(
    solver_param,
    if (solmed %in% c("BoSha32", "DoPri54") && adaptive != "no") {
      paste("-maxretries", as.integer(max_retries), "-retryadj", retry_adjust)
    },
    mode_flag("-assertions", assertions),
    mode_flag("-range_test_initial", range_test_initial),
    mode_flag("-range_test_updated", range_test_updated)
  ), collapse = " ")

  # ch. 51 complementarity run controls (ems_complementarity();
  # solver -comp_* flags, defaults applied solver-side when absent)
  comp_flags <- .comp_cli_flags(complementarity)
  if (!is.null(comp_flags) && nzchar(comp_flags)) {
    solver_param <- paste(solver_param, comp_flags)
  }

  if (!is.null(extra_flags) && nzchar(extra_flags)) {
    solver_param <- paste(solver_param, extra_flags)
  }

  solver_out <- paste("2>&1 | tee", paste0(docker_diagnostic_out, "\""))
  solver_param <- paste(solver_param, solver_out)
  solve_cmd <- paste(exec_preamble, solver_param)

  if (terminal_run) {
    m_exec <- normalizePath(file.path(paths$run, "model_exec.txt"), "/", FALSE)
    cat(solve_cmd, file = m_exec)
    hsl <- "teems-solver"
    diag_out <- normalizePath(paths$diag_out, "/", FALSE)
    cmf_path <- paste0("\"", normalizePath(paths$cmf, "/"), "\"")
    .cli_action(solve_info$terminal_run,
      action = "inform",
      call = call
    )
    cli::cli_verbatim(solve_cmd, "\n")
    cli::cli_ol(solve_info$terminal_run_steps)
    return(FALSE)
  }

  cmd <- list(
    solve = solve_cmd
  )

  return(cmd)
}