#' @keywords internal
#' @noRd
.implement_probe <- function(args_list,
                             call) {
  v <- .validate_probe_args(
    a = args_list,
    call = call
  )
  .check_docker(
    image_name = "teems",
    call = call
  )
  timeID <- .run_id()
  paths <- .get_solver_paths(
    cmf_path = v$cmf_path,
    timeID = paste0(timeID, "_probe"),
    call = call
  )
  paths <- .probe_paths(paths = paths)
  probe_cmd <- .construct_probe_cmd(
    paths = paths,
    timeID = paste0(timeID, "_probe"),
    fine = v$fine,
    extra = v$solver_extras
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
  completed <- FALSE
  on.exit(if (!completed) .container_remove(paste0(timeID, "_probe")), add = TRUE, after = FALSE)
  elapsed <- system.time(status <- .run_solver_cmd(probe_cmd))
  completed <- TRUE
  log_error <- file.exists(paths$diag_out) &&
    any(grepl("Error:", readLines(paths$diag_out, warn = FALSE), fixed = TRUE))
  if (log_error || (!identical(as.integer(status), 0L) && file.exists(paths$diag_out))) {
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
  cores <- v$cores
  memory <- v$memory
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
    metadata = .deploy_metadata(cmf_path = v$cmf_path),
    host = host
  )
  return(probe)
}
