#' @title Configure and solve model in-situ
#' @export
#' @description Calls the
#'   \href{https://github.com/teemsphere/teems-solver}{teems-solver}
#'   Docker image directly with user-supplied input files,
#'   bypassing the [`ems_data()`] / [`ems_model()`] /
#'   [`ems_deploy()`] pipeline. All input files must be provided
#'   in their final form.
#' @return A tibble containing model output variables and
#'   coefficients. Alternatively, if `"suppress_outputs"` is
#'   `TRUE`, file path to a CMF file that may be used with
#'   [`ems_compose()`].
#' @inheritParams ems_model
#' @inheritParams ems_solve
#' @inheritParams ems_deploy
#' @param ... Named arguments corresponding to input files
#'   necessary for an in-situ model run. Names must correspond to
#'   "File" statements within the model Tablo file. Values
#'   correspond to file paths where these files are found. No
#'   checks or modifications are conducted on input files used in
#'   this manner. All model declared input files as well as
#'   `"model_file"`, `"closure_file"`, and `"shock_file"` are
#'   required for in-situ model runs.
#' @param model_dir Character of length 1, base directory where
#'   input files will be copied (if not all already present) and
#'   model outputs will be written.
#' @param shock_file A character vector length 1, path to a file
#'   with .shf extension representing a fully prepared shock
#'   file. No checks or modifications are carried out on this
#'   file.
#' @param ignore_condense Logical length 1 (default is `FALSE`).
#'   If `TRUE`, `Substitute` and `Backsolve` statements found in
#'   the model file are ignored, as for [ems_model()] (in-TAB
#'   `Omit` statements are always ignored). Set this to match the
#'   `ems_model()` call that produced the closure and shock
#'   files.
#' @param solver_args Named list (default `NULL`). The named
#'   solver arguments [`ems_solve()`] accepts through `...`,
#'   passed as a list because the in-situ `...` carries the
#'   input files: the MA48 workspace initial guesses (`laA`,
#'   `laD`, `laDi`), the expert solver flags (`postsim`,
#'   `inmemory`, `fastrefac`, `nsbbdblocks`, `withmc66`,
#'   `smllthreads`, `nowrites`, `condest`, `jacdump`,
#'   `ma48_cntl2`, `ma48_cntl4`) and the Runge-Kutta run controls
#'   (`rk_chart`, `rk_norm`, `rk_controller`, `rk_scope`,
#'   `rk_h0`, `rk_guard`) and the Runge-Kutta step controls
#'   (`adaptive`, `eps_tolerance`, `max_retries`, `retry_adjust`;
#'   see [`ems_RK()`]). See the [`ems_solve()`] `...`
#'   documentation for each argument's meaning.
#' @seealso [`ems_solve()`] for the standard package-supported
#'   solver.
#' @examples
#' \dontrun{
#' # The following examples require the teems solver to be built.
#' # See https://teemsphere.github.io/ to get started.
#' 
#' solve_in_situ(
#'  GTAPDATA = "path/to/dat_file.txt",
#'  GTAPPARM = "path/to/par_file.txt",
#'  GTAPSETS = "path/to/sets_file.txt",
#'  model_dir = "existing/dir/for/model_run",
#'  model_file = "path/to/model_file.tab",
#'  closure_file = "path/to/closure_file.cls",
#'  shock_file = "path/to/shock_file.shf"
#'  )
#' }
solve_in_situ <- function(...,
                          model_dir,
                          model_file,
                          closure_file,
                          shock_file,
                          ignore_condense = FALSE,
                          solution_method = c("Gragg", "Johansen", "Euler", "Midpoint", "RK2", "Heun", "RK4", "BoSha32", "DoPri54"),
                          matrix_method = c("LU", "DBBD", "SBBD", "NDBBD"),
                          n_subintervals = 1L,
                          steps = NULL,
                          n_tasks = 1L,
                          n_threads = 1L,
                          precision = c("single", "double"),
                          verbosity = 1L,
                          suppress_outputs = FALSE,
                          terminal_run = FALSE,
                          accuracy_vars = NULL,
                          solver_args = NULL
) {
if (missing(model_dir)) {
  .cli_missing(model_dir)
}
if (missing(model_file)) {
  .cli_missing(model_file)
}
if (missing(closure_file)) {
  .cli_missing(closure_file)
}
if (missing(shock_file)) {
  .cli_missing(shock_file)
}
args_list <- mget(setdiff(names(formals()), "..."))
args_list$input_files <- list(...)
call <- match.call()
sol <- .implement_solve_in_situ(
  args_list = args_list,
  call = call
)
if (is.null(sol) || is.character(sol)) {
  invisible(sol)
} else {
  sol
}
}
