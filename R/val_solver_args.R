#' @importFrom rlang arg_match is_integerish
#'
#' @keywords internal
#' @noRd
.validate_solver_args <- function(a,
                                  paths,
                                  call,
                                  timeID = NULL,
                                  explicit = character()) {
  
  solution_method <- a$solution_method
  a$solution_method <- rlang::arg_match(
    arg = solution_method,
    values = c("Johansen", "Gragg", "Euler", "RK2", "Heun", "RK4", "BoSha32", "DoPri54"),
    error_call = call
  )
  is_rk <- a$solution_method %in% c("RK2", "Heun", "RK4", "BoSha32", "DoPri54")
  is_rk_embedded <- a$solution_method %in% c("BoSha32", "DoPri54")

  # the step count the method expects, when the user named none: the
  # extrapolating methods take the triple, the Runge-Kutta methods one
  # count (as ems_RK defaults it). A single shared default could not
  # serve both, and the triple reached the RK branch as a user error.
  if (is.null(a$steps)) {
    a$steps <- if (is_rk) 4L else c(2L, 4L, 8L)
  }

  adaptive <- a$adaptive
  a$adaptive <- rlang::arg_match(
    arg = adaptive,
    values = c("no", "yes", "accuracy-only"),
    error_call = call
  )
  
  matrix_method <- a$matrix_method
  a$matrix_method <- rlang::arg_match(
    arg = matrix_method,
    values = c("LU", "DBBD", "SBBD", "NDBBD", "auto"),
    error_call = call
  )

  precision <- a$precision
  a$precision <- rlang::arg_match(
    arg = precision,
    values = c("single", "double"),
    error_call = call
  )
  resources <- a$resources
  a$resources <- rlang::arg_match(
    arg = resources,
    values = c("manual", "auto"),
    error_call = call
  )
  
  checklist <- c(
    list(
      cmf_path = "character",
      solution_method = "character",
      matrix_method = "character",
      n_subintervals = c("numeric", "integer"),
      steps = c("NULL", "numeric", "integer"),
      n_tasks = c("numeric", "integer"),
      n_threads = c("numeric", "integer"),
      precision = "character",
      inmemory = c("NULL", "logical"),
      resources = "character",
      verbosity = c("NULL", "numeric", "integer"),
      suppress_outputs = "logical",
      terminal_run = "logical",
      assertions = c("NULL", "character"),
      range_test_initial = c("NULL", "character"),
      range_test_updated = c("NULL", "character"),
      postsim = c("NULL", "logical"),
      complementarity = c("NULL", "teems_complementarity"),
      pre_probe = "logical",
      # dot-passed Runge-Kutta controls sit after the formals in
      # args_list (ems_solve appends them; .check_arg_class is
      # positional)
      adaptive = "character",
      eps_tolerance = c("numeric", "integer"),
      max_retries = c("NULL", "numeric", "integer"),
      retry_adjust = c("NULL", "numeric")
    ),
    # dot-passed la* initial guesses and expert flags follow the RK
    # controls, in .solver_extra_args() order
    .solver_extra_checklist()
  )
  if (!is.null(a$complementarity) &&
    !inherits(a$complementarity, "teems_complementarity")) {
    .cli_action(solve_err$comp_spec_class,
      action = c("abort", "inform"),
      call = call
    )
  }
  if (!rlang::is_integerish(a$n_threads) || length(a$n_threads) != 1L ||
    a$n_threads < 1) {
    bad_arg <- "n_threads"
    requirement <- "a positive integer-like numeric of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$max_retries) &&
    (!rlang::is_integerish(a$max_retries) || length(a$max_retries) != 1L ||
      a$max_retries < 1)) {
    bad_arg <- "max_retries"
    requirement <- "a positive integer-like numeric of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$retry_adjust) &&
    (!is.numeric(a$retry_adjust) || length(a$retry_adjust) != 1L ||
      is.na(a$retry_adjust) ||
      a$retry_adjust <= 0 || a$retry_adjust >= 1)) {
    bad_arg <- "retry_adjust"
    requirement <- "a numeric of length 1 in (0, 1)"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  for (nme in c("assertions", "range_test_initial", "range_test_updated")) {
    x <- a[[nme]]
    if (!is.null(x) &&
      (!is.character(x) || length(x) != 1L || !x %in% c("fatal", "warn", "off"))) {
      bad_arg <- nme
      .cli_action(solve_err$switch_mode,
        action = "abort",
        call = call
      )
    }
  }
  if (!is.null(a$postsim) &&
    (!is.logical(a$postsim) || length(a$postsim) != 1L || is.na(a$postsim))) {
    bad_arg <- "postsim"
    requirement <- "a non-missing logical of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  .validate_solver_extras(a = a, call = call)

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  if (!rlang::is_integerish(a$n_tasks)) {
    arg <- "n_tasks"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$n_tasks)) %!=% 1L) {
    arg <- "n_tasks"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  if (!rlang::is_integerish(a$n_subintervals)) {
    arg <- "n_subintervals"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$n_subintervals)) %!=% 1L) {
    arg <- "n_subintervals"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  if (!is.null(a$inmemory) && (as.integer(length(a$inmemory)) %!=% 1L || is.na(a$inmemory))) {
    arg <- "inmemory"
    .cli_action(solve_err$logical_scalar,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$pre_probe)) %!=% 1L || is.na(a$pre_probe)) {
    arg <- "pre_probe"
    .cli_action(probe_err$x_logical,
      action = "abort",
      call = call
    )
  }

  if (!is.null(a$verbosity)) {
    if (!rlang::is_integerish(a$verbosity)) {
      arg <- "verbosity"
      .cli_action(solve_err$x_integerish,
        action = "abort",
        call = call
      )
    }
    if (as.integer(length(a$verbosity)) %!=% 1L) {
      arg <- "verbosity"
      .cli_action(solve_err$invalid_length,
        action = "abort",
        call = call
      )
    }
    if (!a$verbosity %in% c(0, 1, 2)) {
      .cli_action(solve_err$verbosity_range,
        action = "abort",
        call = call
      )
    }
  }

  if (is_rk) {
    if (!all(
      is.numeric(a$steps), length(a$steps) == 1,
      rlang::is_integerish(a$steps), a$steps >= 1
    )) {
      solution_method <- a$solution_method
      .cli_action(solve_err$step_single_rk,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$adaptive %!=% "no" && !is_rk_embedded) {
      adaptive <- a$adaptive
      .cli_action(solve_err$adaptive_method,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$n_subintervals != 1) {
      solution_method <- a$solution_method
      .cli_action(solve_err$rk_subintervals,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (!all(is.numeric(a$eps_tolerance), length(a$eps_tolerance) == 1, a$eps_tolerance > 0)) {
      .cli_action(solve_err$epstol_range,
        action = "abort",
        call = call
      )
    }
  } else {
    if (!all(is.numeric(a$steps), length(a$steps) == 3)) {
      .cli_action(solve_err$step_length,
        action = "abort",
        call = call
      )
    }
    if (a$solution_method %=% "Gragg" && !all(a$steps %% 2 == 0)) {
      .cli_action(solve_err$step_parity,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$solution_method %in% c("Gragg", "Euler") && !all(diff(a$steps) > 0)) {
      solution_method <- a$solution_method
      .cli_action(solve_err$step_increasing,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  if ("tab_path" %in% names(attributes(paths$cmf))) {
    tab <- readLines(attr(paths$cmf, "tab_path"))
  } else {
    tab <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = paths$cmf
    )
    tab <- readLines(tab)
  }

  if (any(grepl(pattern = "(intertemporal)", tab))) {
    a$enable_time <- TRUE
  } else {
    a$enable_time <- FALSE
  }

  # `auto` may run the structural probe; the probe object is kept so
  # the pre_probe verdict reuses it (one probe run per solve) and the
  # decision record is written to model_diagnostics.txt after the run
  th <- .auto_thresholds()
  # the container the solver runs in: cores and memory, read once per
  # image and session; NULL when the inspection fails, which leaves the
  # memory checks inert and resources = "auto" on one task
  host <- .container_resources(
    image = paste0("teems:", .resolve_docker_tag(quiet = TRUE))
  )
  metadata <- .deploy_metadata(cmf_path = paths$cmf)
  multistep <- !(a$solution_method %=% "Johansen")
  resources_auto <- a$resources %=% "auto"
  if (resources_auto && !"n_tasks" %in% explicit) {
    # provisional rank count for the method decision (the knee for a
    # chain, two for a static partition); resolved for the chosen
    # method below
    cores <- host$cores %|||% 1L
    a$n_tasks <- if (a$enable_time) {
      min(th$ranks_sbbd_max, cores)
    } else {
      min(2L, cores)
    }
  }
  a$auto_decision <- NULL
  a$probe <- NULL
  if (a$matrix_method %=% "auto") {
    auto <- .resolve_auto_method(
      enable_time = a$enable_time,
      n_tasks = a$n_tasks,
      cmf_path = paths$cmf,
      pre_probe = isTRUE(a$pre_probe),
      timeID = timeID,
      call = call,
      multistep = multistep,
      host = host
    )
    a$matrix_method <- auto$method
    a$auto_decision <- auto$decision
    a$probe <- auto$probe
  }

  # plain-equivalent size, condensation and block count for the
  # resource rules and the memory model: the probe's record when it
  # ran, else the deploy metadata
  d <- a$auto_decision
  condensed <- d$condensed %|||% isTRUE((metadata$condense$n_backsolve %|||% 0L) > 0L)
  plain_size <- d$plain_size %|||% (
    if (is.null(metadata$system_size)) {
      NA_real_
    } else {
      metadata$system_size + (metadata$condense$n_backsolve_ele %|||% 0)
    }
  )
  n_blocks <- if (a$enable_time) {
    d$n_time %|||% (if (isTRUE((metadata$n_time %|||% 0L) > 0L)) metadata$n_time else NULL)
  } else {
    d$partition$n_blocks %|||% metadata$n_reg
  }
  resources_record <- list(
    mode = "manual",
    method = a$matrix_method,
    n_tasks = as.integer(a$n_tasks),
    n_threads = as.integer(a$n_threads),
    inmemory = a$inmemory,
    cores = host$cores,
    mem_gb = host$mem_gb
  )
  if (resources_auto) {
    r <- .resolve_resources(
      method = a$matrix_method,
      host = host,
      n_blocks = n_blocks,
      plain_size = plain_size,
      condensed = condensed,
      requested = a[c("n_tasks", "n_threads", "inmemory")],
      explicit = explicit,
      th = th
    )
    a$n_tasks <- r$n_tasks
    a$n_threads <- r$n_threads
    a$inmemory <- r$inmemory
    resources_record <- r
    n_tasks <- r$n_tasks
    n_threads <- r$n_threads
    method <- a$matrix_method
    cores <- r$cores
    mem_gb <- if (is.na(r$mem_gb)) "unknown" else format(round(r$mem_gb, 1), nsmall = 1)
    rationale <- r$rationale
    .cli_action(solve_info$auto_resources,
      action = "inform",
      call = call
    )
  }
  # the pre-solve memory check (both modes): refused by name past the
  # model's error band, warned inside it, recorded otherwise
  resources_record$fit <- .memory_fit_check(
    method = a$matrix_method,
    n_tasks = a$n_tasks,
    plain_size = plain_size,
    condensed = condensed,
    host = host,
    th = th,
    call = call
  )
  # scratch inside the container filesystem for the scratch-backed
  # runs: docker's /dev/shm is 64 MB by default (Docker Desktop and
  # plain docker alike) and the solver puts NDBBD's and inmemory 0's
  # scratch there unless told where else
  if (is.null(a$tempdir) && (a$matrix_method %=% "NDBBD" || isFALSE(a$inmemory))) {
    a$tempdir <- "/tmp"
  }
  resources_record$tempdir <- a$tempdir
  a$resources_record <- resources_record
  if (!is.null(metadata)) {
    .advise_condense(
      metadata = metadata,
      matrix_method = a$matrix_method,
      enable_time = a$enable_time,
      call = call
    )
  }

  if (a$matrix_method %in% c("SBBD", "NDBBD") && !a$enable_time) {
    matrix_method <- a$matrix_method
    .cli_action(solve_err$invalid_method,
      action = "abort",
      call = call
    )
  }

  a$matsol <- switch(
    EXPR = a$matrix_method,
    "LU" = 0,
    "SBBD" = 1,
    "DBBD" = 2,
    "NDBBD" = 3
  )

  if (a$solution_method %in% c("Gragg", "Euler") || is_rk) {
    a$solmed <- a$solution_method
  } else {
    a$solmed <- "Johansen"
    a$n_subintervals <- 1
  }

  return(a)
}