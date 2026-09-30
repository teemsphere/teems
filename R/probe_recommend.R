#' @keywords internal
#' @noRd
.probe_recommend <- function(probe,
                             metadata = NULL,
                             host = NULL) {
  structure <- probe$structure
  chain <- isTRUE(identical(structure$chain_source, "structural"))
  system_size <- probe$vecsize %|||% metadata$system_size
  condensed <- isTRUE((metadata$condense$n_backsolve %|||% 0L) > 0L)
  n_backsolve_ele <- metadata$condense$n_backsolve_ele %|||% 0
  cores <- as.integer(host$cores %|||% 1L)
  provisional <- if (chain) {
    min(auto_thresholds$ranks_sbbd_max, cores)
  } else {
    min(2L, cores)
  }
  d <- .auto_decide(
    enable_time = chain,
    n_tasks = provisional,
    system_size = system_size,
    n_reg = metadata$n_reg,
    structure = structure,
    condensed = condensed,
    n_backsolve_ele = n_backsolve_ele,
    multistep = TRUE,
    mem_limit_gb = host$mem_gb
  )
  d_johansen <- .auto_decide(
    enable_time = chain,
    n_tasks = provisional,
    system_size = system_size,
    n_reg = metadata$n_reg,
    structure = structure,
    condensed = condensed,
    n_backsolve_ele = n_backsolve_ele,
    multistep = FALSE,
    mem_limit_gb = host$mem_gb
  )
  n_blocks <- if (chain) {
    d$n_time %|||% (if (isTRUE((metadata$n_time %|||% 0L) > 0L)) {
      metadata$n_time
    } else {
      NULL
    })
  } else {
    d$partition$n_blocks %|||% metadata$n_reg
  }
  r <- .resolve_resources(
    method = d$method,
    host = host,
    n_blocks = n_blocks,
    plain_size = d$plain_size,
    condensed = d$condensed
  )
  fit <- .memory_fit_check(
    method = d$method,
    n_tasks = r$n_tasks,
    plain_size = d$plain_size,
    condensed = d$condensed,
    host = host,
    report_only = TRUE
  )
  refine <- .refine_decide(
    method = d$method,
    n_tasks = r$n_tasks,
    plain_size = d$plain_size,
    condensed = d$condensed
  )
  tempdir <- if (identical(d$method, "NDBBD")) {
    "/tmp"
  } else {
    NULL
  }
  call <- paste0(
    "ems_solve(cmf_path, matrix_method = \"", d$method, "\"",
    if (r$n_tasks > 1L) {
      paste0(", n_tasks = ", r$n_tasks)
    } else {
      ""
    },
    if (r$n_threads > 1L) {
      paste0(", n_threads = ", r$n_threads)
    } else {
      ""
    },
    ")"
  )
  recommendation <- list(
    matrix_method = d$method,
    n_tasks = r$n_tasks,
    n_threads = r$n_threads,
    inmemory = NULL,
    tempdir = tempdir,
    model_type = d$model_type,
    method_johansen = d_johansen$method,
    evidence = .auto_evidence(d),
    rationale = r$rationale,
    decision = d,
    host = list(
      cores = host$cores,
      mem_gb = host$mem_gb,
      source = host$source %|||% "container"
    ),
    fit = fit,
    refine = refine,
    call = call
  )
  return(recommendation)
}
