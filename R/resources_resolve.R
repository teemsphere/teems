#' @description Resolve `resources = "auto"`: tasks, threads, the
#'   in-memory switch and the scratch directory for `method` on the
#'   inspected container. Pure (no I/O), unit tested on synthetic
#'   hosts. `n_blocks` caps the rank count (the probe's partition or the
#'   region count); `explicit` names the arguments the caller passed,
#'   which keep their values. Returns the resolved values with the
#'   rationale and the memory estimate for the record.
#' @keywords internal
#' @noRd
.resolve_resources <- function(method,
                               host,
                               n_blocks = NULL,
                               plain_size = NA_real_,
                               condensed = FALSE,
                               requested = list(),
                               explicit = character(),
                               th = .auto_thresholds()) {
  cores <- as.integer(host$cores %|||% 1L)
  limit <- host$mem_gb %|||% NA_real_
  cap_blocks <- function(n) {
    if (is.null(n_blocks) || is.na(n_blocks) || n_blocks < 1L) {
      return(n)
    }
    capped <- min(n, as.integer(n_blocks))
    return(capped)
  }
  fits <- function(ranks) {
    e <- .auto_memory_gb(method, ranks, plain_size, condensed, th)
    return(is.na(e) || is.na(limit) || e <= limit * th$mem_fit_share)
  }
  rationale <- switch(method,
    SBBD = {
      n_tasks <- cap_blocks(min(th$ranks_sbbd_max, cores))
      "ranks to the knee, threads take the remaining cores"
    },
    DBBD = {
      n_tasks <- if (cores <= th$cores_laptop_max) {
        th$ranks_dbbd_laptop
      } else {
        min(th$ranks_dbbd_max, max(2L, cores %/% 4L))
      }
      n_tasks <- cap_blocks(min(n_tasks, cores))
      while (n_tasks > 2L && !fits(n_tasks)) n_tasks <- n_tasks %/% 2L
      "two ranks on a laptop, more only where cores and memory allow; threads take the rest"
    },
    NDBBD = {
      n_tasks <- 1L
      "one rank holds the tables once; the solver budgets its own threads"
    },
    {
      n_tasks <- 1L
      "one rank; threads for the condensed factorization"
    }
  )
  n_tasks <- max(1L, as.integer(n_tasks))
  if ("n_tasks" %in% explicit) {
    n_tasks <- max(1L, as.integer(requested$n_tasks))
  }
  n_threads <- if (identical(method, "NDBBD")) {
    max(1L, cores)
  } else {
    max(1L, min(th$threads_max, cores %/% n_tasks))
  }
  if ("n_threads" %in% explicit) {
    n_threads <- as.integer(requested$n_threads)
  }
  inmemory <- if ("inmemory" %in% explicit) {
    requested$inmemory
  } else {
    NULL
  }
  resources <- list(
    method = method,
    n_tasks = n_tasks,
    n_threads = n_threads,
    inmemory = inmemory,
    cores = cores,
    mem_gb = limit,
    rationale = rationale,
    explicit = explicit
  )
  return(resources)
}
