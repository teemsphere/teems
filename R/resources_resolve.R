#' @keywords internal
#' @noRd
.cap_blocks <- function(n, n_blocks) {
  if (is.null(n_blocks) || is.na(n_blocks) || n_blocks < 1L) {
    return(n)
  }
  capped <- min(n, as.integer(n_blocks))
  return(capped)
}

#' @keywords internal
#' @noRd
.resources_fits <- function(ranks, method, plain_size, condensed, th, limit) {
  e <- .auto_memory_gb(method, ranks, plain_size, condensed, th)
  return(is.na(e) || is.na(limit) || e <= limit * th$mem_fit_share)
}

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
  rationale <- switch(method,
    SBBD = {
      n_tasks <- .cap_blocks(min(th$ranks_sbbd_max, cores), n_blocks)
      solve_info$rationale$SBBD
    },
    DBBD = {
      n_tasks <- if (cores <= th$cores_laptop_max) {
        th$ranks_dbbd_laptop
      } else {
        min(th$ranks_dbbd_max, max(2L, cores %/% 4L))
      }
      n_tasks <- .cap_blocks(min(n_tasks, cores), n_blocks)
      while (n_tasks > 2L && !.resources_fits(n_tasks, method, plain_size, condensed, th, limit)) n_tasks <- n_tasks %/% 2L
      solve_info$rationale$DBBD
    },
    NDBBD = {
      n_tasks <- 1L
      solve_info$rationale$NDBBD
    },
    {
      n_tasks <- 1L
      solve_info$rationale$LU
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
