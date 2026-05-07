#' Configure and solve model in-situ
#'
#' Calls the
#' \href{https://github.com/teemsphere/teems-solver}{teems-solver}
#' Docker image directly with user-supplied input files, bypassing
#' the [`ems_data()`] / [`ems_model()`] / [`ems_deploy()`]
#' pipeline. All input files must be provided in their final form.
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
#'
#' @seealso [`ems_solve()`] for the standard package-supported
#'   solver.
#'
#' @return A tibble containing model output variables and
#'   coefficients. Alternatively, if `"suppress_outputs"` is
#'   `TRUE`, file path to a CMF file that may be used with
#'   [`ems_compose()`].
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
                          suppress_outputs = FALSE,
                          terminal_run = FALSE,
                          append_args = NULL
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
return(.implement_solve_in_situ(
  model_file = model_file,
  model_dir = model_dir,
  closure_file = closure_file,
  input_files = input_files,
  shock_file = shock_file,
  n_tasks = n_tasks,
  n_subintervals = n_subintervals,
  matrix_method = matrix_method,
  solution_method = solution_method,
  steps = steps,
  laA = laA,
  laD = laD,
  laDi = laDi,
  suppress_outputs = suppress_outputs,
  terminal_run = terminal_run,
  append_args = append_args,
  call = call
))
}