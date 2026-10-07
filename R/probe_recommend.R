#' @keywords internal
#' @noRd
.probe_no_recommendation <- function(status,
                                     host) {
  recommendation <- list(
    status = status,
    matrix_method = NA_character_,
    n_tasks = NA_integer_,
    n_threads = NA_integer_,
    host = host,
    call = NA_character_
  )
  return(recommendation)
}

#' @keywords internal
#' @noRd
.dbbd_rank_rule <- function(cores,
                            th) {
  n_tasks <- if (cores <= th$cores_laptop_max) {
    th$ranks_dbbd_laptop
  } else {
    min(th$ranks_dbbd_max, max(2L, cores %/% 4L))
  }
  n_tasks <- as.integer(min(n_tasks, cores))
  return(n_tasks)
}

#' @importFrom cli format_inline
#' @keywords internal
#' @noRd
.probe_recommend <- function(probe,
                             metadata = NULL,
                             host = NULL,
                             th = .auto_thresholds()) {
  cores <- as.integer(host$cores %|||% NA_integer_)
  mem_gb <- as.numeric(host$mem_gb %|||% NA_real_)
  host <- list(
    cores = cores,
    mem_gb = mem_gb,
    source = host$source %|||% "container"
  )
  if (!isTRUE(probe$valid)) {
    recommendation <- .probe_no_recommendation("singular", host)
    return(recommendation)
  }
  if (is.na(cores) || cores < 1L) {
    recommendation <- .probe_no_recommendation("no_host", host)
    return(recommendation)
  }
  structure <- probe$structure
  chain <- identical(structure$chain_source, "structural")
  system_size <- probe$vecsize %|||% metadata$system_size
  condensed <- isTRUE((metadata$condense$n_backsolve %|||% 0L) > 0L)
  n_backsolve_ele <- metadata$condense$n_backsolve_ele %|||% 0
  decide <- function(n_tasks, multistep = TRUE) {
    d <- .auto_decide(
      enable_time = chain,
      n_tasks = n_tasks,
      system_size = system_size,
      n_reg = metadata$n_reg,
      structure = structure,
      th = th,
      condensed = condensed,
      n_backsolve_ele = n_backsolve_ele,
      multistep = multistep,
      mem_limit_gb = mem_gb
    )
    d$small <- !is.na(d$plain_size) && d$plain_size < th$dbbd_hint_min
    if (d$small) {
      d$method <- "LU"
    }
    return(d)
  }
  provisional <- if (chain) {
    as.integer(min(th$ranks_sbbd_max, cores))
  } else {
    .dbbd_rank_rule(cores, th)
  }
  d <- decide(provisional)
  if (!chain && !d$small && !identical(d$method, "DBBD") && provisional > 2L) {
    d2 <- decide(2L)
    if (identical(d2$method, "DBBD")) {
      d <- d2
    }
  }
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
  if ((d$method %in% c("DBBD", "SBBD") || d$small) && r$n_tasks != d$n_tasks) {
    d <- decide(r$n_tasks)
  }
  d_johansen <- decide(d$n_tasks, multistep = FALSE)
  if (identical(d$method, "LU") && !d$condensed) {
    r$n_threads <- 1L
    r$rationale <- cli::format_inline(probe_info$recommend$lu_plain)
  }
  if (d$small) {
    r$rationale <- sprintf(probe_info$recommend$small, .fmt(th$dbbd_hint_min))
  }
  fit <- .memory_fit_check(
    method = d$method,
    n_tasks = r$n_tasks,
    plain_size = d$plain_size,
    condensed = d$condensed,
    host = host,
    th = th,
    report_only = TRUE
  )
  refine <- .refine_decide(
    method = d$method,
    n_tasks = r$n_tasks,
    plain_size = d$plain_size,
    condensed = d$condensed,
    th = th
  )
  dbbd_plain_gb <- NA_real_
  if (isTRUE(d$dbbd_memory_blocked) && d$condensed) {
    plain_gb <- .auto_memory_gb("DBBD", d$n_tasks, d$plain_size, condensed = FALSE, th = th)
    if (!is.na(plain_gb) && !is.na(mem_gb) && plain_gb <= mem_gb * th$mem_fit_share) {
      dbbd_plain_gb <- plain_gb
    }
  }
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
  status <- if (identical(fit$verdict, "exceeds")) {
    "wont_fit"
  } else {
    "ok"
  }
  recommendation <- list(
    status = status,
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
    host = host,
    fit = fit,
    refine = refine,
    dbbd_plain_gb = dbbd_plain_gb,
    call = call
  )
  return(recommendation)
}
