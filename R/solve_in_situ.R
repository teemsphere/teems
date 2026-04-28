#' Configure and solve model in-situ
#' 
#' @description `solve_in_situ()` is a wrapper for the
#'   \href{https://github.com/teemsphere/teems-solver}{teems-solver}
#'   which conducts a minimal number of checks prior to attempting
#'   to solve the constrained optimization problem according to a
#'   range of runtime configuration options. In order to solve the
#'   model, a `teems` Docker image
#'   (\href{https://github.com/teemsphere/teems-solver}{teems-solver})
#'   must be built. Singularity, accuracy, and error checks are
#'   carried out following a successful run.
#' 
#'   In contrast to [`teems::ems_solve()`], all model input files
#'   must be provided by the user in their final form.
#' 
#' @inheritParams ems_model
#' @inheritParams ems_solve
#' @inheritParams ems_deploy
#' 
#' @param ... Named arguments corresponding to input files
#'   necessary for an in-situ model run. Names must correspond to
#'   "File" statements within the model Tablo file. Values
#'   correspond to file paths where these files are found. No
#'   checks or modifications are conducted on input files used in
#'   this manner. All model declared input files as well as
#'   `"model_file"`, `"closure_file"`, and `"shock_file"` are
#'   required for in-situ model runs.
#' @param model_dir Character of length 1, base directory where
#'   input files will be copied and the model will be run. 
#' @param writeout Logical length 1, default `FALSE`. Whether to
#'   attempt to parse the Tablo file and append "Write"
#'   declarations for out all model output coefficients and sets.
#'   Note that if `TRUE` a successful writeout may require
#'   modifications to the `"model_file"` provided. If `FALSE`,
#'   outputs will consist of model variables.
#' 
#' @seealso [`ems_solve()`] for the standard package-supported
#'   solver.
#' 
#' @return A list of data.tables (if `"writeout"` == `FALSE`). A
#'   tibble containing model output variables and coefficients (if
#'   `"writeout"` == `TRUE`). Alternatively, if
#'   `"suppress_outputs"` is `TRUE`, file path to a CMF file that
#'   may be used with [`ems_compose()`].
#' 
#' @export
solve_in_situ <- function(...,
                          model_file,
                          model_dir,
                          closure_file,
                          shock_file,
                          n_tasks = 1L,
                          n_subintervals = 1L,
                          matrix_method = c("LU", "DBBD", "SBBD", "NDBBD"),
                          solution_method = c("Johansen", "mod_midpoint"),
                          steps = c(2L, 4L, 8L),
                          laA = 300L,
                          laD = 200L,
                          laDi = 500L,
                          terminal_run = FALSE,
                          suppress_outputs = FALSE,
                          writeout = FALSE
) {
call <- match.call()
if (missing(model_file)) {
  .cli_missing(model_file)
}
if (missing(model_dir)) {
  .cli_missing(model_dir)
}
if (missing(closure_file)) {
  .cli_missing(closure_file)
}
if (missing(shock_file)) {
  .cli_missing(shock_file)
}
if (missing(...)) {
  .cli_action(solve_err$no_insitu_inputs,
    action = "abort",
    call = call
  )
}
input_files <- list(...)
output <- .implement_solve_in_situ(
  model_file = model_file,
  model_dir = model_dir,
  closure_file = closure_file,
  input_files = input_files,
  shock_file = shock_file,
  writeout = writeout,
  n_tasks = n_tasks,
  n_subintervals = n_subintervals,
  matrix_method = matrix_method,
  solution_method = solution_method,
  steps = steps,
  laA = laA,
  laD = laD,
  laDi = laDi,
  terminal_run = terminal_run,
  suppress_outputs = suppress_outputs,
  call = call
)
output
}