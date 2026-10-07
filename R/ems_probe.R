#' @importFrom rlang is_integerish is_logical
#' @title Structural probe of a deployed model
#' @export
#' @description Runs the solver's structural probe (`-solmed probe`) on
#'   a deployed model: the full pre-solve pipeline (data, formulas,
#'   closure, ordering) followed by an HSL_MC79 maximum-matching /
#'   Dulmage-Mendelsohn diagnosis of the condensed Jacobian — without
#'   solving. The probe validates the closure structurally, catches the
#'   zero-flow singularity class (structurally present but zero-valued
#'   at base data), names any defective variable and equation elements,
#'   and returns the statement-level equation-system structure.
#' @param cmf_path Character length 1, path to the CMF file generated
#'   by [`ems_deploy()`].
#' @param fine Logical length 1 (default `TRUE`). Also run the fine
#'   Dulmage-Mendelsohn decomposition: the strongly-connected-component
#'   view of the system — its irreducible simultaneous cores versus the
#'   recursively solvable remainder — including the composition of the
#'   largest cores by equation and variable.
#' @param cores Integer length 1 or `NULL` (default): the core count
#'   the recommendation is made for. `NULL` reads it from the
#'   container the solver runs in (Docker Desktop's Resources setting
#'   on a laptop); pass a number to get the recommendation for
#'   another machine.
#' @param memory Numeric length 1 or `NULL` (default): the memory in
#'   GB the recommendation is made for, read from the container when
#'   `NULL`, as for `cores`.
#' @param ... Additional named solver arguments, the same set
#'   [`ems_solve()`] accepts through `...`: the MA48 workspace initial
#'   guesses (`laA`, `laD`, `laDi`), the expert solver flags
#'   (`postsim`, `inmemory`, `fastrefac`, `cntl_3`,
#'   `cntl_6`, `nsbbdblocks`, `withmc66`, `smllthreads`, `tempdir`,
#'   `nowrites`, `condest`, `jacdump`, `ma48u`) and the Runge-Kutta
#'   run controls, which a probe ignores. Anything else is an error,
#'   never a silently ignored flag.
#' @details The probe runs on a single MPI rank; its cost is the
#'   pre-solve pipeline plus the matching (milliseconds at 10^4
#'   equations, ~a minute at 10^6). A structurally singular result does
#'   not error here — the object reports it (see
#'   [`plot.teems_probe()`] and the `defects` tibble).
#'
#'   The probe also settles the condensation question, which the
#'   deploy-time advice in [`ems_solve()`] can only guess at: the
#'   measured block partition decides whether backsolving helps.
#'   Substitution densifies the blocks the bordered methods exploit
#'   (measured on GTAP-RE: the standard condensation made `"SBBD"` runs
#'   69 to 393 percent slower, and condensed `"DBBD"` stops gaining from
#'   extra tasks), so a system with a usable partition and a time chain,
#'   or one above about 120 thousand condensed equations, is told to
#'   redeploy without `backsolve` (`"hurts"`); below that, threaded
#'   `"LU"` on the condensed system is still the fastest measured
#'   (`"lu_fine"`); a condensed system without a partition is
#'   `"LU"`-bound, where condensation pays (`"helps"`); and an
#'   uncondensed `"LU"`-bound system of a million equations or more is
#'   named as a candidate for `backsolve` in [`ems_model()`]. The
#'   verdict is carried in `condense$verdict`.
#'
#'   The probe also recommends how to solve the deployment: the matrix
#'   method and the tasks, threads and scratch directory to run it
#'   with on this machine (or on the `cores` and `memory` given),
#'   from the measured structure, the deploy metadata and a peak-memory
#'   model per method (see [`ems_solve()`] Details for the rules and
#'   their provenance). The recommendation prints with the object as a
#'   ready-to-paste [`ems_solve()`] call and is carried in
#'   `recommendation`; `ems_solve()` itself chooses nothing. No
#'   recommendation is made when one cannot be sound: a structurally
#'   singular system (fix the closure first), a container whose cores
#'   could not be read (pass `cores` and `memory`), or a system whose
#'   smallest estimate does not fit the memory. The evidence is
#'   assessed at the task count the recommendation names, and a
#'   system below the smallest measured size (300 thousand equations)
#'   is sent to `"LU"`, since every method solves it quickly.
#' @seealso [`ems_deploy()`] for generating `"cmf_path"`;
#'   [`plot.teems_probe()`] for the incidence, Dulmage-Mendelsohn and
#'   core visualizations; [`ems_solve()`].
#' @return A `teems_probe` object: validity verdict and rank per
#'   pattern, named defect tibble, statement-level incidence tibbles
#'   (`statements`, `incidence`), core structure (`cores`), ordering
#'   evidence (`structure`: the chain dimension and the block-partition
#'   candidate table the solver measured -- the evidence
#'   the recommendation is made from), the
#'   condensation verdict (`condense`), the recommended method and
#'   resources (`recommendation`, whose `status` is `"ok"`,
#'   `"singular"`, `"no_host"` or `"wont_fit"`), and report paths.
#' @examples
#' \dontrun{
#' # The following examples require the teems solver to be built.
#' # See https://teemsphere.github.io/ to get started.
#'
#' probe <- ems_probe(cmf_path)
#' probe
#' plot(probe, type = "incidence")
#' plot(probe, type = "cores")
#' }
ems_probe <- function(cmf_path,
                      fine = TRUE,
                      cores = NULL,
                      memory = NULL,
                      ...) {
  if (missing(cmf_path)) {
    .cli_missing(cmf_path)
  }
  call <- match.call()
  xtr_args <- .solver_extra_args()
  dots <- list(...)
  unknown_args <- setdiff(names(dots), names(xtr_args))
  if (length(dots) &&
    (is.null(names(dots)) || !all(nzchar(names(dots))) || length(unknown_args))) {
    if (!length(unknown_args)) {
      unknown_args <- "<unnamed>"
    }
    .cli_action(probe_err$probe_dots,
      action = c("abort", "inform"),
      call = call
    )
  }
  for (nm in names(dots)) {
    xtr_args[nm] <- dots[nm]
  }
  .validate_solver_extras(a = xtr_args, call = call)
  if (!rlang::is_logical(fine, n = 1) || is.na(fine)) {
    arg <- "fine"
    .cli_action(probe_err$x_logical,
      action = "abort",
      call = call
    )
  }
  if (!is.null(cores) &&
    (!rlang::is_integerish(cores) || length(cores) != 1L || is.na(cores) || cores < 1)) {
    arg <- "cores"
    .cli_action(probe_err$x_positive,
      action = "abort",
      call = call
    )
  }
  if (!is.null(memory) &&
    (!is.numeric(memory) || length(memory) != 1L || is.na(memory) || memory <= 0)) {
    arg <- "memory"
    .cli_action(probe_err$x_positive,
      action = "abort",
      call = call
    )
  }
  .check_docker(
    image_name = "teems",
    call = call
  )
  timeID <- .run_id()
  paths <- .get_solver_paths(
    cmf_path = cmf_path,
    timeID = paste0(timeID, "_probe"),
    call = call
  )
  paths <- .probe_paths(paths = paths)
  probe_cmd <- .construct_probe_cmd(
    paths = paths,
    timeID = paste0(timeID, "_probe"),
    fine = fine,
    extra = xtr_args
  )
  verbose <- .o_verbose()
  if (verbose) {
    cmf_file <- basename(paths$cmf)
    log_rel <- file.path("out", "probe", basename(paths$diag_out))
    .cli_action(probe_info$run$start,
      action = "inform",
      call = call
    )
  }
  elapsed <- system.time(status <- .run_solver_cmd(probe_cmd))
  if (!identical(as.integer(status), 0L) && file.exists(paths$diag_out)) {
    .check_solver_log(
      elapsed_time = NULL,
      solve_cmd = probe_cmd,
      paths = paths,
      call = call,
      status = status
    )
  }
  probe <- .collect_probe(
    paths = paths,
    call = call
  )
  if (verbose) {
    elapsed_txt <- .probe_elapsed_txt(elapsed[["elapsed"]])
    .cli_action(probe_info$run$done,
      action = "inform",
      call = call
    )
    .probe_inform_warnings(paths = paths, call = call)
  }
  if (!probe$valid && verbose) {
    .cli_action(probe_info$probe_defective,
      action = "inform",
      call = call
    )
  }
  # the recommendation: for this container unless a machine is given
  inspected <- if (is.null(cores) || is.null(memory)) {
    .cntnr_resources(image = paste0("teems:", .resolve_docker_tag(quiet = TRUE)))
  } else {
    NULL
  }
  host <- list(
    cores = as.integer(cores %|||% inspected$cores %|||% NA_integer_),
    mem_gb = as.numeric(memory %|||% inspected$mem_gb %|||% NA_real_),
    source = if (!is.null(cores) && !is.null(memory)) {
      "given"
    } else if (!is.null(cores)) {
      "cores_given"
    } else if (!is.null(memory)) {
      "memory_given"
    } else {
      "container"
    }
  )
  probe$recommendation <- .probe_recommend(
    probe = probe,
    metadata = .deploy_metadata(cmf_path = cmf_path),
    host = host
  )
  return(probe)
}
