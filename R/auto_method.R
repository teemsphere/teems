#' @keywords internal
#' @noRd
.auto_est <- function(method, ranks, mem, plain_size, condensed, th) {
  e <- .auto_memory_gb(method, ranks, plain_size, condensed, th)
  mem$estimates[[method]] <- e
  return(e)
}

#' @keywords internal
#' @noRd
.auto_fits <- function(method, ranks, mem, plain_size, condensed, th, limit) {
  e <- .auto_est(method, ranks, mem, plain_size, condensed, th)
  if (is.na(e) || is.na(limit)) {
    return(NA)
  }
  return(e <= limit * th$mem_fit_share)
}

#' @keywords internal
#' @noRd
.auto_finish <- function(d, mem) {
  d$memory$estimates <- mem$estimates
  d$memory$chosen_gb <- mem$estimates[[d$method]] %|||% NA_real_
  return(d)
}

#' @keywords internal
#' @noRd
.auto_decide <- function(enable_time,
                         n_tasks,
                         system_size,
                         n_reg = NULL,
                         structure = NULL,
                         th = .auto_thresholds(),
                         condensed = FALSE,
                         n_backsolve_ele = 0,
                         multistep = TRUE,
                         mem_limit_gb = NULL) {
  probed <- !is.null(structure)
  n_tasks <- as.integer(n_tasks)
  part <- .auto_partition(structure, n_tasks = n_tasks)
  chain <- if (probed) {
    identical(structure$chain_source, "structural")
  } else {
    NA
  }
  size <- system_size %|||% NA_real_
  if (probed && !is.null(structure$nbacksolve)) {
    condensed <- isTRUE(structure$nbacksolve > 0)
    n_backsolve_ele <- structure$nbselems %|||% n_backsolve_ele
  }
  condensed <- isTRUE(condensed)
  plain_size <- if (is.na(size)) {
    NA_real_
  } else {
    size + (n_backsolve_ele %|||% 0)
  }
  limit <- mem_limit_gb %|||% NA_real_
  mem <- new.env(parent = emptyenv())
  mem$estimates <- list()
  d <- list(
    model_type = if (enable_time) {
      "intertemporal"
    } else {
      "static"
    },
    method = NA_character_,
    source = if (probed) {
      "probe"
    } else if (!is.na(size)) {
      "metadata"
    } else {
      "none"
    },
    n_tasks = n_tasks,
    system_size = size,
    plain_size = plain_size,
    condensed = condensed,
    multistep = isTRUE(multistep),
    chain = chain,
    chain_set = structure$chain_set,
    n_time = structure$ntime,
    chain_border = if (isTRUE(chain)) {
      structure$netcut
    } else {
      NULL
    },
    partition = part,
    n_reg = n_reg,
    thresholds = th,
    probed = probed,
    dbbd_hint = FALSE,
    no_chain = FALSE,
    lu_ceiling = NULL,
    lu_excluded = FALSE,
    lu_unavoidable = FALSE,
    memory = list(limit_gb = limit, estimates = list()),
    memory_arm = FALSE,
    dbbd_memory_blocked = FALSE
  )
  if (enable_time && !isFALSE(chain)) {
    d$method <- "SBBD"
    if (isFALSE(.auto_fits("SBBD", n_tasks, mem, plain_size, condensed, th, limit)) &&
      isTRUE(.auto_fits("NDBBD", n_tasks, mem, plain_size, condensed, th, limit))) {
      d$method <- "NDBBD"
      d$memory_arm <- TRUE
    }
    decision <- .auto_finish(d, mem)
    return(decision)
  }
  if (enable_time && isFALSE(chain)) {
    d$no_chain <- TRUE
  }

  d$method <- "LU"
  viable <- if (probed) {
    !is.null(part) && (is.na(part$border_share) || part$border_share <= th$border_share_max)
  } else {
    isTRUE((n_reg %|||% 0L) >= max(n_tasks, 2L))
  }
  favorable <- if (!condensed) {
    TRUE
  } else if (!is.na(size)) {
    size >= (if (isTRUE(multistep)) {
      th$dbbd_condensed_size
    } else {
      th$dbbd_condensed_size_johansen
    })
  } else {
    FALSE
  }
  if (probed) {
    d$lu_ceiling <- .auto_lu_ceiling(
      nnz = structure$nnz,
      condensed = condensed,
      th = th
    )
    d$lu_excluded <- isTRUE(d$lu_ceiling$exceeded)
    if (d$lu_excluded) {
      if (!is.null(part) && part$n_blocks >= max(n_tasks, 1L)) {
        d$method <- "DBBD"
      } else {
        d$lu_unavoidable <- TRUE
      }
      decision <- .auto_finish(d, mem)
      return(decision)
    }
  }
  if (n_tasks >= 2L && viable && favorable) {
    if (isFALSE(.auto_fits("DBBD", n_tasks, mem, plain_size, condensed, th, limit))) {
      d$dbbd_memory_blocked <- TRUE
    } else {
      d$method <- "DBBD"
    }
  } else if (n_tasks < 2L && favorable && !is.na(size) &&
    (condensed || size >= th$dbbd_hint_min)) {
    d$dbbd_hint <- TRUE
  }
  if (identical(d$method, "LU")) {
    .auto_est("LU", 1L, mem, plain_size, condensed, th)
  }
  decision <- .auto_finish(d, mem)
  return(decision)
}
