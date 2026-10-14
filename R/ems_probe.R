#' @title Structural probe of a deployed model
#' @export
#' @description Runs the solver up to the point where the
#'   equation system is assembled, without solving, and checks
#'   that system. It reports whether the closure is valid, and
#'   names the elements at fault when it is not, as well as
#'   attempts to judge whether backsolving is advantageous. It
#'   also recommends the matrix method, task count and thread
#'   count.
#' @param cmf_path Character length 1, path to the CMF file
#'   written by [`ems_deploy()`].
#' @param components Logical length 1 (default `TRUE`). Also find
#'   the simultaneous components of the system, the groups of
#'   equations that must be solved together, and the make-up of
#'   the largest; needed by `plot(x, type = "components")`.
#' @param cores Integer length 1 or `NULL` (default). The number
#'   of cores the recommendation is made for. `NULL` reads it
#'   from the container the solver runs in (on Docker Desktop,
#'   the Resources setting); give a number for another machine.
#' @param memory Numeric length 1 or `NULL` (default). The memory
#'   in GB the recommendation is made for, read from the
#'   container when `NULL`.
#' @param ... Named solver arguments, the same ones `...` of
#'   [`ems_solve()`] accepts: the MA48 workspace guesses (`laA`,
#'   `laD`, `laDi`) and the expert solver flags. The Runge-Kutta
#'   controls are accepted and ignored.
#' @details A probe runs on one task. It takes seconds on a
#'   condensed GTAP model and minutes at a few million
#'   uncondensed equations. A singular system is reported, not
#'   raised as an error: the printed object names the
#'   undetermined variables and the over-constrained equations,
#'   `plot(x, type = "dm")` locates them and `x$defects` lists
#'   every element.
#' @section Condensation:
#'   Backsolving shrinks the system but densifies the blocks the parallel methods split along; on
#'   GTAP-RE the standard condensation makes `"SBBD"` runs much
#'   slower. The probe's verdict on the deployment is in
#'   `x$condense$verdict`:
#'   * `"slower"`: condensed, with a block partition and either a
#'   time chain or more than about 120 thousand condensed
#'   equations; try redeploying without `backsolve`.
#'   * `"lu_fine"`: condensed, with a partition, below that size;
#'   threaded `"LU"` on the condensed system was the fastest
#'   measured.
#'   * `"faster"`: condensed, without a partition; the system is
#'   `"LU"`-bound and condensation tends to speed it.
#'   * `"candidate"`: uncondensed, `"LU"`-bound and a million
#'   equations or more; `backsolve` in [`ems_model()`] may help.
#'   * `"none"`: no verdict.
#' @section Recommendation:
#'   From the measured structure, the deploy metadata and a memory model per method, the probe suggests a
#'   matrix method with its tasks and threads for the machine
#'   (the container's, or the `cores` and `memory` given). It
#'   prints as a ready-to-paste [`ems_solve()`] call and is kept
#'   in `x$recommendation`. In three cases the probe makes no
#'   recommendation and `recommendation$status` says why:
#'   `"singular"`, the closure must be fixed first; `"no_host"`,
#'   the container's cores could not be read, so pass `cores` and
#'   `memory`; `"wont_fit"`, no method fits in the memory. A
#'   system below 300 thousand equations, the smallest
#'   size measured, is sent to `"LU"`. The rules and the
#'   measurements behind them are in the
#'   \href{https://teemsphere.github.io/solve.html}{solving} and
#'   \href{https://teemsphere.github.io/benchmarks.html}{benchmarks}
#'   chapters of the manual. `ems_probe()` is experimental: its rules
#'   come from a limited
#'   set of models and machines, so a different method, task
#'   count or thread count may be faster for yours. Try
#'   alternatives with [`ems_solve()`], and report a suggestion
#'   that is off at
#'   \url{https://github.com/teemsphere/teems/issues}.
#' @return A `teems_probe` object: `valid`; the `structural` and
#'   `realized` (base-data) rank patterns; the `defects`,
#'   `statements` and `incidence` tibbles; `components`; `structure`
#'   (the time chain and block partition measured); `condense`;
#'   `recommendation`, with its `status`; and `paths`. `print()`
#'   gives the verdict and the recommendation, `summary()` the
#'   full report and [`plot.teems_probe()`] the pictures.
#' @seealso [`ems_deploy()`] for writing the CMF file;
#'   [`ems_solve()`] for the solve; [`plot.teems_probe()`].
#' @examples
#' \dontrun{
#' # The following examples require the teems solver to be built.
#' # See https://teemsphere.github.io/ to get started.
#'
#' probe <- ems_probe(cmf_path)
#' probe
#' plot(probe, type = "incidence")
#' plot(probe, type = "components")
#' }
ems_probe <- function(cmf_path,
                      components = TRUE,
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
