build_solve_err <- function() {
  list(
    no_insitu_inputs = c("No input files have been loaded.",
                              "All input files must be passed as named arguments using {.arg ...} under the {.emph in-situ solve} method."),
    missing_insitu_inputs = "The Tablo file provided indicates that the following files are required: {.val {req_inputs}} however one or more appear to not have been provided: {.val {missing_files}}.",
    insitu_no_file = "One or more input files provided does not exist: {.val {nonexist_files}}.",
    x_integerish = "{.arg {arg}} must be integer-like.",
    invalid_length = "{.arg {arg}} must be an integer-like numeric of length 1.",
    subint_form = "{.arg n_subintervals} must contain either all even numbers or all odd numbers.",
    step_length = "{.arg steps} must be a numeric vector of length 3.",
    invalid_method = "{.arg matrix_method} {.val {matrix_method}} only applicable to intertemporal model runs.",
    solution_err = "Errors detected during solution. See {.path {paths$diag_out}}.",
    solution_sing = "Singularity detected during solution. See {.path {paths$diag_out}}.",
    NDBBD_time = "The {.arg n_timesteps} argument is required when using the NDBBD matrix method.",
    docker_installed = "Docker is required to call the solver and is not installed on this system.",
    docker_sudo = "Docker is installed but cannot be called without sudo.",
    docker_x_image = "The {.val {image_name}} Docker image is not present."
  )
}

build_solve_wrn <- function() {
  list(
    accuracy = c(
      "Only {.emph {accuracy}} of variables accurate at at least 4 digit precision, less than {a_threshold}.",
      "See {.arg accuracy_threshold} within {.fun teems::ems_option_set} to adjust the warning threshold."
    )
  )
}

build_solve_info <- function() {
  list(
    in_situ = "\"solve-in-situ\" mode activated.",
    terminal_run = "{.arg terminal_run} mode has been selected enabling model runs outside of your R IDE or R script. The following steps are necessary to solve the model and compose outputs.",
    t_run_append = substitute(cli::cli_ol(
      c(
        "Run the following command at your OS terminal: {.val {solve_cmd}}",
        "If errors are present in the terminal output during an ongoing run, it is possible to stop the relevant {.val {hsl}} process early according to your OS-specific system activity monitor.",
        "Error and singularity indicators will be present in the model diagnostic output: {.val {diag_out}}.",
        "If no errors or singularities are detected, run the following command to convert binary outputs: {.val {sol_parse_cmd}}.",
        "The {.arg cmf_path} path to use as a value within {.fun teems::ems_compose} is {.path {cmf_path}}."
      )
    )),
    accuracy = "{.emph {accuracy}} of variables accurate at at least 4 digit precision.",
    elapsed_time = "Elapsed time: {elapsed_time}"
  )
}
