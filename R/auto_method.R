#' Structure-informed `matrix_method = "auto"` (ROADMAP 6.10) and the
#' `resources = "auto"` resolution
#'
#' The method decision reads the deployed system's MEASURED structure --
#' the solver's structural probe (`-solmed probe`) -- where the probe is
#' cheap, and the deploy metadata (system size, region count,
#' condensation record) where it is not: the chain dimension the
#' equations couple through lead/lag offsets, the diagonal-block
#' partition candidates (`partition_auto`), and the border sizes of the
#' chosen partition. A memory model per method turns the container's
#' memory limit into a third input, so a choice that would not fit is
#' never made and a run that cannot fit is refused by name before it
#' starts.
#'
#' Every constant is a named entry of `.auto_thresholds()`, reported in
#' the auto message and written to model_diagnostics.txt, with its
#' provenance beside it. Calibration (2026-09): the laptop ladder on the
#' HPC box (`teems-dev/hpc/results.csv`, labels `laptop4_*`/`laptop8_*`:
#' a 16 GB laptop emulated as `--memory 12g` with 4 or 8 cores; static
#' plain 346k-7.69M and condensed 20k-230k equations, intertemporal
#' 1.30M-8.85M; tables in `teems-dev/docs/ladder_tables.md`) and the
#' uncapped top-end cells (I-long-big 21.9M, I-230 230M, S-full-big
#' 40.5M, Q34 234M; hpc_auto_plan.md section 3). Ratios transfer between
#' machines (kernel-invariant memory, method crossovers, rank/thread
#' splits); absolute walls do not and none is encoded.
#'
#' @keywords internal
#' @noRd
.auto_thresholds <- function() {
  list(
    # ---- probe policy -------------------------------------------------
    # plain static: the probe costs 2.3-2.9x a Johansen solve at
    # 346k-534k eq (9-11 s) but 10.7-15x at 1.41M and 27-50x from 3.46M
    # (more than the whole Gragg solve it would inform), super-linear
    # in size; above this it is skipped and the metadata rule decides
    probe_plain_max = 1e6,
    # condensed static: always cheap (3.7-44 s to 230k condensed eq,
    # 0.3-1.1x a Johansen), so it always runs when a bordered method
    # is a candidate; intertemporal: never (SBBD is fixed by structure)
    # ---- static crossovers --------------------------------------------
    # plain static: DBBD beats LU at every rung 346k-7.69M under Gragg
    # (DBBD/LU 0.62 -> 0.30) and ties or wins under Johansen (ties below
    # ~600k, whole solves under 10 s) -- no size gate
    dbbd_hint_min = 3e5,
    # condensed static, multi-step: threaded LU wins to 110k condensed eq
    # (DBBD/LU 2.0 -> 1.12), ties at 140k (0.99/0.87)
    dbbd_condensed_size = 1.2e5,
    # condensed static, Johansen: DBBD wins from 70k condensed eq
    # (0.61 -> 0.03 at 230k)
    dbbd_condensed_size_johansen = 7e4,
    # ceiling on max(border variables, border equations) / system size
    # for a bordered method chosen from probe evidence; never binds on
    # GTAP geometry (2.8e-4 S-full, 3.9e-5 I-200) -- a guard
    border_share_max = 0.10,
    # ---- ranks and threads ----------------------------------------------
    # SBBD: 1->2 ranks -26..-33 %, 2->4 -3..-21 %, 4->8 -11..+1 % on the
    # ladder; ranks beat threads at every fixed core count (1x8 is +55..
    # +93 % against 8x1); on the box 8x4 beats 32x1 (I-long) -- knee 4,
    # cap 8, the remaining cores go to threads
    ranks_sbbd_max = 8L,
    # DBBD plain: flat from 2 ranks at 8 cores (S14P 171/170/170 s at
    # 2x4/4x2/8x1) while each rank costs +0.27 kB/eq; condensed DBBD gets
    # slower with ranks (S14C 153 -> 198 -> 282 s) and OOMs at 8 from
    # 110k condensed eq -- 2 ranks on a laptop, up to 8 where the core
    # count and the memory model allow (S-full 4->8 ranks 1.56x)
    ranks_dbbd_laptop = 2L,
    ranks_dbbd_max = 8L,
    cores_laptop_max = 8L,
    # t8 no better than t4 at the knee (I-long SBBD8: 496 vs 506 s)
    threads_max = 8L,
    # ---- LU workspace ceiling (unchanged; MA48_LA_MAX, 32-bit HSL) ------
    lu_la_ceiling = 2147483647,
    # measured LA / nnz (2026-08): I-long 6.0, S-full 12.0, S-full-cond 40
    lu_fill = 12,
    lu_fill_condensed = 40,
    lu_ceiling_warn_share = 0.75,
    # ---- memory model: peak GB = kB/eq x plain-equivalent equations ------
    # plain-equivalent = solved system + backsolved elements: condensation
    # cuts equations ~20x but not peak memory (the value tables follow
    # the data); measured whole-container peaks, Gragg, ladder 6.3
    # LU@1: 0.58-0.70 kB/eq to 3.77M, 0.84-0.90 at 7.69M
    mem_lu = 0.90,
    # condensed LU = 0.90-1.21x the plain rig's LU
    mem_lu_condensed = 1.2,
    # DBBD: eq x (0.85 + 0.27 x ranks) kB, anchored on the binding cell
    # S90P DBBD2 (10.7 predicted vs 10.8-11.2 GB), 7-25 % over on smaller
    # rungs -- conservative in the right direction
    mem_dbbd_base = 0.85,
    mem_dbbd_rank = 0.27,
    # condensed DBBD = 1.30-1.55x the plain rig's DBBD
    mem_dbbd_condensed = 1.55,
    # SBBD: (0.36 + 0.02 x ranks) kB/eq at >= 4.5M; extrapolates to
    # I-long-big 21.9M within 15 %
    mem_sbbd_base = 0.36,
    mem_sbbd_rank = 0.02,
    # NDBBD: replicated tables 0.148 GB per M eq per rank (34.7 GB at
    # Q34's 234M, one rank) plus the local matrix share; the per-thread
    # working sets are capped by the solver itself (dev8 thread budget)
    mem_ndbbd_rank = 0.15,
    mem_ndbbd_local = 0.05,
    # a choice must fit inside this share of the container's memory
    mem_fit_share = 0.90,
    # the won't-fit abort fires only past the model's error band: an
    # estimate above this multiple of the limit cannot be a false alarm
    mem_abort_ratio = 1.2
  )
}

#' @description Peak-memory estimate in decimal GB for `method` at
#'   `n_tasks` on a system of `plain_size` plain-equivalent equations
#'   (`NA` when the size is unknown).
#' @keywords internal
#' @noRd
.auto_memory_gb <- function(method,
                            n_tasks,
                            plain_size,
                            condensed = FALSE,
                            th = .auto_thresholds()) {
  if (is.null(plain_size) || is.na(plain_size) || plain_size <= 0) {
    return(NA_real_)
  }
  n_tasks <- max(1L, as.integer(n_tasks))
  kb <- switch(method,
    LU = th$mem_lu * (if (isTRUE(condensed)) th$mem_lu_condensed else 1),
    DBBD = (th$mem_dbbd_base + th$mem_dbbd_rank * n_tasks) *
      (if (isTRUE(condensed)) th$mem_dbbd_condensed else 1),
    SBBD = th$mem_sbbd_base + th$mem_sbbd_rank * n_tasks,
    NDBBD = th$mem_ndbbd_rank * n_tasks + th$mem_ndbbd_local,
    NA_real_
  )
  kb * plain_size / 1e6
}

#' @description Resolve `matrix_method = "auto"`. Returns a list:
#'   `method` (the chosen method), `decision` (the evidence record
#'   rendered in the message and model_diagnostics.txt) and `probe`
#'   (the `teems_probe` object when a probe ran, else `NULL`; the
#'   caller reuses it for the `pre_probe` verdict so one probe run
#'   serves both). `pre_probe = TRUE` forces the probe, so its
#'   structure feeds the decision at any size. `host` is the
#'   container's cores/memory (`.container_resources()`) or `NULL`;
#'   `multistep` says whether the solution method factorizes more than
#'   once (the condensed crossover differs for Johansen).
#' @keywords internal
#' @noRd
.resolve_auto_method <- function(enable_time,
                                 n_tasks,
                                 cmf_path,
                                 pre_probe = FALSE,
                                 timeID = NULL,
                                 call = NULL,
                                 multistep = TRUE,
                                 host = NULL) {
  th <- .auto_thresholds()
  metadata <- .deploy_metadata(cmf_path = cmf_path)
  system_size <- metadata$system_size
  n_tasks <- as.integer(n_tasks)
  condensed <- isTRUE((metadata$condense$n_backsolve %|||% 0L) > 0L)
  n_backsolve_ele <- metadata$condense$n_backsolve_ele %|||% 0

  size_known <- !is.null(system_size)
  probe_reason <- NULL
  probe_skip <- NULL
  if (isTRUE(pre_probe)) {
    probe_reason <- "pre_probe"
  } else if (enable_time) {
    probe_skip <- "intertemporal: the method is fixed by structure"
  } else if (n_tasks < 2L) {
    probe_skip <- "single task"
  } else if (condensed) {
    probe_reason <- "condensed static candidate"
  } else if (!size_known || system_size < th$probe_plain_max) {
    probe_reason <- "static candidate"
  } else {
    probe_skip <- paste0(
      "plain static above ",
      format(th$probe_plain_max, big.mark = ",", scientific = FALSE),
      " equations (the probe costs more than the solve)"
    )
  }

  probe <- NULL
  structure <- NULL
  if (!is.null(probe_reason)) {
    .cli_action(solve_info$auto_probe,
      action = "inform",
      call = call
    )
    probe <- .run_probe(
      cmf_path = cmf_path,
      timeID = timeID %|||% .run_id(),
      call = call
    )
    structure <- probe$structure
    if (!size_known && !is.null(probe$vecsize)) {
      system_size <- probe$vecsize
    }
  }

  d <- .auto_decide(
    enable_time = enable_time,
    n_tasks = n_tasks,
    system_size = system_size,
    n_reg = metadata$n_reg,
    structure = structure,
    th = th,
    condensed = condensed,
    n_backsolve_ele = n_backsolve_ele,
    multistep = multistep,
    mem_limit_gb = host$mem_gb
  )
  d$probe_reason <- probe_reason
  d$probe_skip <- probe_skip

  chosen <- d$method
  model_type <- d$model_type
  .cli_action(solve_info$auto_method,
    action = "inform",
    call = call
  )
  if (!is.null(probe)) {
    evidence <- .auto_evidence(d)
    .cli_action(solve_info$auto_evidence,
      action = "inform",
      call = call
    )
  }
  fmt_gb <- function(x) format(round(x, 1), nsmall = 1, trim = TRUE)
  if (isTRUE(d$memory_arm)) {
    sbbd_gb <- fmt_gb(d$memory$estimates$SBBD)
    ndbbd_gb <- fmt_gb(d$memory$estimates$NDBBD)
    mem_gb <- fmt_gb(d$memory$limit_gb)
    .cli_action(solve_info$auto_memory_arm,
      action = "inform",
      call = call
    )
  }
  if (isTRUE(d$dbbd_memory_blocked)) {
    dbbd_gb <- fmt_gb(d$memory$estimates$DBBD)
    mem_gb <- fmt_gb(d$memory$limit_gb)
    .cli_action(solve_info$auto_dbbd_memory,
      action = "inform",
      call = call
    )
  }
  if (isTRUE(d$dbbd_hint)) {
    .cli_action(solve_info$auto_dbbd_hint,
      action = "inform",
      call = call
    )
  }
  if (isTRUE(d$no_chain)) {
    .cli_action(solve_info$auto_no_chain,
      action = "inform",
      call = call
    )
  }
  if (isTRUE(d$lu_excluded)) {
    la_ceiling <- format(d$lu_ceiling$ceiling, big.mark = ",", scientific = FALSE, trim = TRUE)
    if (isTRUE(d$lu_unavoidable)) {
      projected <- format(round(d$lu_ceiling$projected),
        big.mark = ",", scientific = FALSE, trim = TRUE
      )
      .cli_action(solve_wrn$auto_lu_ceiling,
        action = c("warn", "inform"),
        call = call
      )
    } else {
      .cli_action(solve_info$auto_lu_excluded,
        action = c("inform", "inform"),
        call = call
      )
    }
  } else if (identical(chosen, "LU") && isTRUE(d$lu_ceiling$near)) {
    la_ceiling <- format(d$lu_ceiling$ceiling, big.mark = ",", scientific = FALSE, trim = TRUE)
    share <- paste0(round(100 * d$lu_ceiling$share), "%")
    .cli_action(solve_info$auto_lu_near_ceiling,
      action = c("inform", "inform"),
      call = call
    )
  }
  list(method = chosen, decision = d, probe = probe)
}

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
  plain_size <- if (is.na(size)) NA_real_ else size + (n_backsolve_ele %|||% 0)
  limit <- mem_limit_gb %|||% NA_real_
  estimates <- list()
  est <- function(method, ranks) {
    e <- .auto_memory_gb(method, ranks, plain_size, condensed, th)
    estimates[[method]] <<- e
    e
  }
  fits <- function(method, ranks) {
    e <- est(method, ranks)
    if (is.na(e) || is.na(limit)) NA else e <= limit * th$mem_fit_share
  }

  d <- list(
    model_type = if (enable_time) "intertemporal" else "static",
    method = NA_character_,
    source = if (probed) "probe" else if (!is.na(size)) "metadata" else "none",
    n_tasks = n_tasks,
    system_size = size,
    plain_size = plain_size,
    condensed = condensed,
    multistep = isTRUE(multistep),
    chain = chain,
    chain_set = structure$chain_set,
    n_time = structure$ntime,
    chain_border = if (isTRUE(chain)) structure$netcut else NULL,
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
    d$memory$estimates <- estimates
    d$memory$chosen_gb <- estimates[[d$method]] %|||% NA_real_
    d
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
    return(finish(d))
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
    size >= (if (isTRUE(multistep)) th$dbbd_condensed_size else th$dbbd_condensed_size_johansen)
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
      return(finish(d))
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
  if (identical(d$method, "LU")) est("LU", 1L)
  finish(d)
}

#' @description Projected MA48 workspace for a single sequential LU
#'   factorization, and whether it clears the 32-bit ceiling. Returns
#'   `NULL` when the probe supplied no nonzero count (the exclusion
#'   cannot be applied without one). `condensed` selects the fill
#'   anchor: condensation cuts nnz but raises fill by about as much.
#' @keywords internal
#' @noRd
.auto_lu_ceiling <- function(nnz,
                             condensed = FALSE,
                             th = .auto_thresholds()) {
  if (is.null(nnz) || is.na(nnz) || nnz <= 0) {
    return(NULL)
  }
  fill <- if (isTRUE(condensed)) th$lu_fill_condensed else th$lu_fill
  projected <- nnz * fill
  share <- projected / th$lu_la_ceiling
  list(
    nnz = nnz,
    fill = fill,
    projected = projected,
    ceiling = th$lu_la_ceiling,
    share = share,
    exceeded = projected >= th$lu_la_ceiling,
    near = share >= th$lu_ceiling_warn_share
  )
}

#' @description The partition the solver would apply at `n_tasks`,
#'   replayed from the probe's candidate table with the solver's own
#'   rule (viable + at least `n_tasks` blocks; smallest border; near
#'   ties within 2% broken by block balance). `border_share` is the
#'   larger of border variables (netcut) and border equations
#'   (`border_neq`, known for the probe's chosen set only) over the
#'   system size. `NULL` when no candidate qualifies.
#' @keywords internal
#' @noRd
.auto_partition <- function(structure,
                            n_tasks) {
  cand <- structure$partition_auto
  if (is.null(cand) || !NROW(cand)) {
    return(NULL)
  }
  cand <- as.data.frame(cand)
  ok <- cand[cand$viable %in% TRUE & cand$nblocks >= n_tasks, , drop = FALSE]
  if (!NROW(ok)) {
    return(NULL)
  }
  best_cut <- min(ok$netcut)
  ok <- ok[50 * ok$netcut <= 51 * best_cut, , drop = FALSE]
  balance <- ok$block_min / pmax(ok$block_max, 1)
  pick <- ok[which.max(balance), , drop = FALSE]
  if (NROW(pick) > 1L) pick <- pick[1L, , drop = FALSE]
  vecsize <- structure$vecsize %|||% NA_real_
  border_neq <- if (identical(pick$set, structure$partition_set)) {
    structure$border_neq %|||% NA_real_
  } else {
    NA_real_
  }
  border <- max(pick$netcut, border_neq, na.rm = TRUE)
  list(
    set = pick$set,
    n_blocks = as.integer(pick$nblocks),
    netcut = as.integer(pick$netcut),
    border_neq = if (is.na(border_neq)) NA_integer_ else as.integer(border_neq),
    block_min = as.integer(pick$block_min),
    block_max = as.integer(pick$block_max),
    border_share = if (is.na(vecsize) || vecsize <= 0) NA_real_ else border / vecsize
  )
}

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
    if (is.null(n_blocks) || is.na(n_blocks) || n_blocks < 1L) n else min(n, as.integer(n_blocks))
  }
  fits <- function(ranks) {
    e <- .auto_memory_gb(method, ranks, plain_size, condensed, th)
    is.na(e) || is.na(limit) || e <= limit * th$mem_fit_share
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
  if ("n_tasks" %in% explicit) n_tasks <- max(1L, as.integer(requested$n_tasks))
  n_threads <- if (identical(method, "NDBBD")) {
    max(1L, cores)
  } else {
    max(1L, min(th$threads_max, cores %/% n_tasks))
  }
  if ("n_threads" %in% explicit) n_threads <- as.integer(requested$n_threads)
  inmemory <- if ("inmemory" %in% explicit) requested$inmemory else NULL
  list(
    mode = "auto",
    method = method,
    n_tasks = n_tasks,
    n_threads = n_threads,
    inmemory = inmemory,
    cores = cores,
    mem_gb = limit,
    rationale = rationale,
    explicit = explicit
  )
}

#' @description The pre-solve memory check (all modes): the chosen
#'   method's estimate against the container's memory. Aborts by name
#'   past the model's error band, warns inside it, records otherwise.
#'   Returns the record (`NULL` verdict fields when the size or the
#'   limit is unknown).
#' @keywords internal
#' @noRd
.memory_fit_check <- function(method,
                              n_tasks,
                              plain_size,
                              condensed = FALSE,
                              host = NULL,
                              th = .auto_thresholds(),
                              call = NULL) {
  est_gb <- .auto_memory_gb(method, n_tasks, plain_size, condensed, th)
  limit <- host$mem_gb %|||% NA_real_
  rec <- list(
    method = method,
    n_tasks = as.integer(n_tasks),
    plain_size = plain_size,
    condensed = isTRUE(condensed),
    est_gb = est_gb,
    limit_gb = limit,
    share = NA_real_,
    verdict = "unknown"
  )
  if (is.na(est_gb) || is.na(limit) || limit <= 0) {
    return(rec)
  }
  rec$share <- est_gb / limit
  fmt_gb <- function(x) format(round(x, 1), nsmall = 1, trim = TRUE)
  if (rec$share > th$mem_abort_ratio) {
    rec$verdict <- "exceeds"
    est_gb <- fmt_gb(est_gb)
    mem_gb <- fmt_gb(limit)
    kb_per_eq <- format(round(1e6 * rec$est_gb / plain_size, 2), nsmall = 2, trim = TRUE)
    plain_size <- format(round(plain_size), big.mark = ",", scientific = FALSE, trim = TRUE)
    .cli_action(solve_err$wont_fit,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }
  if (rec$share > th$mem_fit_share) {
    rec$verdict <- "tight"
    est_gb <- fmt_gb(est_gb)
    mem_gb <- fmt_gb(limit)
    share <- paste0(round(100 * rec$share), "%")
    .cli_action(solve_wrn$memory_tight,
      action = c("warn", "inform"),
      call = call
    )
    return(rec)
  }
  rec$verdict <- "fits"
  rec
}

#' @description One-line evidence string for the auto message.
#' @keywords internal
#' @noRd
.auto_evidence <- function(d) {
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  pct <- function(x) paste0(format(round(100 * x, 1), nsmall = 1, trim = TRUE), "%")
  size <- if (is.na(d$system_size)) "unknown size" else paste(fmt(d$system_size), "equations")
  if (isTRUE(d$condensed)) size <- paste(size, "(condensed)")
  if (!isTRUE(d$probed)) {
    return(paste0(
      size, ", n_tasks ", d$n_tasks,
      "; structural probe skipped (",
      d$probe_skip %|||% "not a candidate",
      ")"
    ))
  }
  chain <- if (isTRUE(d$chain)) {
    paste0("chain ", d$chain_set, " (", d$n_time, " blocks)")
  } else {
    "no chain"
  }
  part <- if (is.null(d$partition)) {
    paste0("no partition viable for ", d$n_tasks, " task(s)")
  } else {
    p <- d$partition
    paste0(
      "partition ", p$set, " (", p$n_blocks, " blocks, border ",
      if (is.na(p$border_share)) "n/a" else pct(p$border_share), ")"
    )
  }
  ceil <- if (is.null(d$lu_ceiling)) {
    ""
  } else if (isTRUE(d$lu_excluded)) {
    paste0(
      ", LU excluded (projected MA48 workspace ", fmt(round(d$lu_ceiling$projected)),
      " > 32-bit ceiling ", fmt(d$lu_ceiling$ceiling), ")"
    )
  } else {
    paste0(", LU workspace ", pct(d$lu_ceiling$share), " of the 32-bit ceiling")
  }
  paste0(size, ", ", chain, ", ", part, ", n_tasks ", d$n_tasks, ceil)
}

#' @description Lines for the model_diagnostics.txt solve record.
#' @keywords internal
#' @noRd
.auto_record_lines <- function(d) {
  if (is.null(d)) {
    return(NULL)
  }
  th <- d$thresholds
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  fmt_gb <- function(x) if (is.null(x) || is.na(x)) "n/a" else paste0(format(round(x, 2), nsmall = 2, trim = TRUE), " GB")
  mem_line <- if (is.na(d$memory$limit_gb)) {
    "  memory: container limit unknown (memory arm and won't-fit check not applied)"
  } else {
    est <- d$memory$estimates
    sprintf(
      "  memory: container %s; estimates %s%s%s",
      fmt_gb(d$memory$limit_gb),
      paste(
        vapply(names(est), function(m) paste0(m, " ", fmt_gb(est[[m]])), character(1)),
        collapse = ", "
      ),
      if (isTRUE(d$memory_arm)) " -- SBBD does not fit, NDBBD chosen" else "",
      if (isTRUE(d$dbbd_memory_blocked)) " -- DBBD does not fit, LU kept" else ""
    )
  }
  c(
    sprintf(
      "Matrix method auto: %s (%s: %s)", d$method,
      if (isTRUE(d$probed)) "structural probe" else "deploy metadata",
      .auto_evidence(d)
    ),
    sprintf(
      "  thresholds: probe_plain_max %s, dbbd_condensed_size %s (Johansen %s), dbbd_hint_min %s, border_share_max %s, mem_fit_share %s, mem_abort_ratio %s",
      format(th$probe_plain_max, scientific = FALSE),
      format(th$dbbd_condensed_size, scientific = FALSE),
      format(th$dbbd_condensed_size_johansen, scientific = FALSE),
      format(th$dbbd_hint_min, scientific = FALSE),
      th$border_share_max, th$mem_fit_share, th$mem_abort_ratio
    ),
    mem_line,
    sprintf(
      "  LU workspace ceiling: %s elements, fill %s%s, advisory share %s%s",
      format(th$lu_la_ceiling, scientific = FALSE),
      if (is.null(d$lu_ceiling)) th$lu_fill else d$lu_ceiling$fill,
      if (is.null(d$lu_ceiling)) " (nnz unknown: exclusion not applied)" else "",
      th$lu_ceiling_warn_share,
      if (is.null(d$lu_ceiling)) {
        ""
      } else {
        sprintf(
          "; nnz %s -> projected %s (%s of ceiling)%s",
          fmt(d$lu_ceiling$nnz),
          fmt(round(d$lu_ceiling$projected)),
          paste0(round(100 * d$lu_ceiling$share, 1), "%"),
          if (isTRUE(d$lu_excluded)) " -- LU EXCLUDED" else ""
        )
      }
    )
  )
}

#' @description Lines for the model_diagnostics.txt solve record:
#'   the resolved resources (auto or manual), the container inspected
#'   and the pre-solve memory check.
#' @keywords internal
#' @noRd
.resources_record_lines <- function(r) {
  if (is.null(r)) {
    return(NULL)
  }
  fmt_gb <- function(x) if (is.null(x) || is.na(x)) "unknown" else paste0(format(round(x, 2), nsmall = 2, trim = TRUE), " GB")
  host <- if (is.null(r$cores) || is.na(r$cores)) {
    "container not inspected"
  } else {
    sprintf("container %s core(s), %s", r$cores, fmt_gb(r$mem_gb))
  }
  fit <- r$fit
  fit_line <- if (is.null(fit) || is.na(fit$est_gb)) {
    "  memory check: not applied (system size unknown)"
  } else if (identical(fit$verdict, "unknown")) {
    sprintf("  memory check: estimate %s, container limit unknown", fmt_gb(fit$est_gb))
  } else {
    sprintf(
      "  memory check: %s at %s task(s) estimated %s = %s kB/eq x %s plain-equivalent equations%s -> %s of %s (%s)",
      fit$method, fit$n_tasks, fmt_gb(fit$est_gb),
      format(round(1e6 * fit$est_gb / fit$plain_size, 2), nsmall = 2, trim = TRUE),
      format(round(fit$plain_size), big.mark = ",", scientific = FALSE, trim = TRUE),
      if (isTRUE(fit$condensed)) " (condensed)" else "",
      paste0(round(100 * fit$share), "%"), fmt_gb(fit$limit_gb), fit$verdict
    )
  }
  c(
    sprintf(
      "Resources %s: n_tasks %s, n_threads %s, inmemory %s, tempdir %s (%s)%s",
      r$mode, r$n_tasks, r$n_threads,
      if (is.null(r$inmemory)) "solver default" else tolower(as.character(r$inmemory)),
      r$tempdir %|||% "solver default",
      host,
      if (identical(r$mode, "auto")) paste0("; ", r$rationale) else ""
    ),
    fit_line
  )
}
