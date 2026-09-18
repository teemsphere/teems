# The `ems_probe()` recommendation (ROADMAP 6.10) and the pre-solve
# memory check
#
# `ems_solve()` runs exactly what it is given. The recommendation of a
# matrix method and of the tasks, threads and scratch to run it with
# comes from `ems_probe()`, which reads the deployed system's MEASURED
# structure (the solver's structural probe: the chain dimension the
# equations couple through lead/lag offsets, the diagonal-block
# partition candidates and the border of the chosen partition), the
# deploy metadata (system size, region count, condensation record) and
# the container's cores and memory (or the ones given, for a machine
# other than this one). A memory model per method turns the memory
# limit into a third input, so a recommendation never names a method
# that would not fit, and `ems_solve()` refuses by name a run the
# memory limit would kill.
#
# Every constant is a named entry of `.auto_thresholds()`, printed with
# the recommendation, with its provenance beside it. Calibration (2026-09): the laptop ladder on the
# HPC box (`teems-dev/hpc/results.csv`, labels `laptop4_*`/`laptop8_*`:
# a 16 GB laptop emulated as `--memory 12g` with 4 or 8 cores; static
# plain 346k-7.69M and condensed 20k-230k equations, intertemporal
# 1.30M-8.85M; tables in `teems-dev/docs/ladder_tables.md`) and the
# uncapped top-end cells (I-long-big 21.9M, I-230 230M, S-full-big
# 40.5M, Q34 234M; hpc_auto_plan.md section 3). Ratios transfer between
# machines (kernel-invariant memory, method crossovers, rank/thread
# splits); absolute walls do not and none is encoded.

#' @description Pure decision rule over the evidence (no I/O), unit
#'   tested on synthetic `structure` lists. `structure` is
#'   `probe$structure` (`.probe_stats()` output) or `NULL` when no
#'   probe ran; then the metadata-only rule applies. `condensed` and
#'   `n_backsolve_ele` come from the deploy metadata (the probe's own
#'   record wins when it ran); `mem_limit_gb` is the container's memory
#'   (`NULL` = unknown: the memory arm and the DBBD memory guard stay
#'   inert and the won't-fit check is not applied).
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
  # the memory estimates are filled in as the rules ask for them; an
  # explicit environment carries them instead of a super-assignment
  mem <- new.env(parent = emptyenv())
  mem$estimates <- list()
  est <- function(method, ranks) {
    e <- .auto_memory_gb(method, ranks, plain_size, condensed, th)
    mem$estimates[[method]] <- e
    return(e)
  }
  fits <- function(method, ranks) {
    e <- est(method, ranks)
    if (is.na(e) || is.na(limit)) {
      return(NA)
    }
    return(e <= limit * th$mem_fit_share)
  }

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
  finish <- function(d) {
    d$memory$estimates <- mem$estimates
    d$memory$chosen_gb <- mem$estimates[[d$method]] %|||% NA_real_
    return(d)
  }

  if (enable_time && !isFALSE(chain)) {
    # SBBD is the intertemporal method at every measured size (2.4M-230M
    # eq; SBBD@2 is 104x LU@1 on I-long); NDBBD only when SBBD's host
    # copy does not fit the container -- never a time choice on GTAP
    # geometry (remainder_plan.md section 8)
    d$method <- "SBBD"
    if (isFALSE(fits("SBBD", n_tasks)) && isTRUE(fits("NDBBD", n_tasks))) {
      d$method <- "NDBBD"
      d$memory_arm <- TRUE
    }
    decision <- finish(d)
    return(decision)
  }
  if (enable_time && isFALSE(chain)) {
    # declared intertemporal, but no equation couples set elements
    # through lead/lag offsets: the chain methods would abort in the
    # solver; the static family applies
    d$no_chain <- TRUE
  }

  d$method <- "LU"
  # a bordered partition with at least n_tasks blocks: measured by the
  # probe when it ran, else the region count from the deploy metadata
  # (the solver's static partition is the regional set on GTAP models)
  viable <- if (probed) {
    !is.null(part) && (is.na(part$border_share) || part$border_share <= th$border_share_max)
  } else {
    isTRUE((n_reg %|||% 0L) >= max(n_tasks, 2L))
  }
  # the crossover: plain systems favor DBBD at every measured size;
  # condensed ones only from the size where threaded LU stops winning
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
    # the 32-bit workspace ceiling is a hard exclusion, evaluated
    # before the performance gates: past it LU cannot factorize this
    # system at any -laA, so a bordered method is the only option that
    # runs at all, whatever the crossover would have said
    d$lu_ceiling <- .auto_lu_ceiling(
      nnz = structure$nnz,
      condensed = condensed,
      th = th
    )
    d$lu_excluded <- isTRUE(d$lu_ceiling$exceeded)
    if (d$lu_excluded) {
      if (!is.null(part) && part$n_blocks >= max(n_tasks, 1L)) {
        # correctness outranks the crossover, the border-share guard
        # and the memory model
        d$method <- "DBBD"
      } else {
        d$lu_unavoidable <- TRUE
      }
      decision <- finish(d)
      return(decision)
    }
  }
  if (n_tasks >= 2L && viable && favorable) {
    if (isFALSE(fits("DBBD", n_tasks))) {
      d$dbbd_memory_blocked <- TRUE
    } else {
      d$method <- "DBBD"
    }
  } else if (n_tasks < 2L && favorable && !is.na(size) &&
    (condensed || size >= th$dbbd_hint_min)) {
    d$dbbd_hint <- TRUE
  }
  if (identical(d$method, "LU")) {
    est("LU", 1L)
  }
  decision <- finish(d)
  return(decision)
}
