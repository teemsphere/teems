#' @keywords internal
#' @noRd
.implement_calculate <- function(model_file,
                                 input_files,
                                 model_dir,
                                 call) {
  cmf_path <- .in_situ_cmf(
    input_files = input_files,
    model_file = model_file,
    model_dir = model_dir,
    call = call
  )
  args_list <- list(
    cmf_path = cmf_path,
    solution_method = "Johansen",
    matrix_method = "LU",
    n_subintervals = 1L,
    steps = NULL,
    n_tasks = 1L,
    n_threads = 1L,
    precision = "single",
    verbosity = 1L,
    suppress_outputs = FALSE,
    terminal_run = FALSE,
    complementarity = NULL,
    adaptive = "no",
    eps_tolerance = 0.01,
    max_retries = 3L,
    retry_adjust = 0.5
  )
  args_list <- c(args_list, .solver_extra_args())
  output <- .implement_solve(
    args_list = args_list,
    call = call,
    solmed = "nosim"
  )
  output <- output[output$type != "postsim", ]
  return(output)
}
