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
#'   (`postsim`, `inmemory`, `fastrefac`, `nsbbdblocks`, `withmc66`,
#'   `smllthreads`, `nowrites`, `condest`, `jacdump`,
#'   `ma48_cntl2`, `ma48_cntl4`) and the Runge-Kutta
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
                      ...
) {
if (missing(cmf_path)) {
  .cli_missing(cmf_path)
}
args_list <- mget(setdiff(names(formals()), "..."))
args_list$solver_extras <- list(...)
call <- match.call()
probe <- .implement_probe(
  args_list = args_list,
  call = call
)
probe
}