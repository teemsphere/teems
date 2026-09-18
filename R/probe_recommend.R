#' @description The `ems_probe()` recommendation: method, tasks,
#'   threads, in-memory switch and scratch directory for the probed
#'   deployment on `host` (cores, mem_gb), with the evidence, the memory
#'   estimates and the fits verdict. Pure over the probe object, the
#'   deploy metadata and the host (unit tested on fixtures and synthetic
#'   hosts). The recommendation targets the multi-step solution methods;
#'   where the Johansen crossover differs the alternative is named.
#' @keywords internal
#' @noRd
.probe_recommend <- function(probe,
                             metadata = NULL,
                             host = NULL,
                             th = .auto_thresholds()) {
  structure <- probe$structure
  chain <- isTRUE(identical(structure$chain_source, "structural"))
  system_size <- probe$vecsize %|||% metadata$system_size
  condensed <- isTRUE((metadata$condense$n_backsolve %|||% 0L) > 0L)
  n_backsolve_ele <- metadata$condense$n_backsolve_ele %|||% 0
  cores <- as.integer(host$cores %|||% 1L)
  # the rank count the method decision is made at: the knee for a
  # chain, two for a static partition; resolved for the method below
  provisional <- if (chain) {
    min(th$ranks_sbbd_max, cores)
  } else {
    min(2L, cores)
  }
  decide <- function(multistep) {
    decision <- .auto_decide(
      enable_time = chain,
      n_tasks = provisional,
      system_size = system_size,
      n_reg = metadata$n_reg,
      structure = structure,
      th = th,
      condensed = condensed,
      n_backsolve_ele = n_backsolve_ele,
      multistep = multistep,
      mem_limit_gb = host$mem_gb
    )
    return(decision)
  }
  d <- decide(TRUE)
  d_johansen <- decide(FALSE)
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
    condensed = d$condensed,
    th = th
  )
  fit <- .memory_fit_check(
    method = d$method,
    n_tasks = r$n_tasks,
    plain_size = d$plain_size,
    condensed = d$condensed,
    host = host,
    th = th,
    report_only = TRUE
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
    call = call
  )
  return(recommendation)
}
