# matrix_method = "auto" decision rule (ROADMAP 6.10) and the
# resources = "auto" resolution: pure rules over probe evidence, deploy
# metadata and the container's memory, exercised on -solmed probe
# stats.json fixtures from the GTAPv7 (static) and GTAP-RE
# (intertemporal) goldens plus synthetic variants. The constants are
# the 2026-09 laptop-ladder measurements (see .auto_thresholds).
fx <- test_path("fixtures", "probe")
static_stats <- .probe_stats(jsonlite::fromJSON(file.path(fx, "structural_static.stats.json")))
inter_stats <- .probe_stats(jsonlite::fromJSON(file.path(fx, "structural_inter.stats.json")))
th <- .auto_thresholds()

test_that("probe stats keep the partition candidate table and the chosen set", {
  expect_s3_class(static_stats$partition_auto, "tbl_df")
  expect_identical(nrow(static_stats$partition_auto), 12L)
  expect_identical(static_stats$partition_chosen, "reg")
  expect_identical(static_stats$partition_source, "structural")
  expect_identical(static_stats$chain_source, "none")
  expect_identical(inter_stats$chain_source, "structural")
  expect_identical(inter_stats$chain_set, "alltime")
  expect_identical(inter_stats$ntime, 3L)
})

test_that("partition replay follows the solver's selection at the solve's rank count", {
  # the solver's own choice at 1 rank
  p <- .auto_partition(static_stats, n_tasks = 1L)
  expect_identical(p$set, "reg")
  expect_identical(p$n_blocks, 3L)
  expect_identical(p$netcut, 74L)
  expect_identical(p$border_neq, 224L)
  expect_equal(p$border_share, 224 / 3485)
  # 3 blocks cannot serve 4 tasks: the next viable candidate takes over
  p4 <- .auto_partition(static_stats, n_tasks = 4L)
  expect_identical(p4$set, "demd")
  expect_identical(p4$n_blocks, 9L)
  # border_neq is only known for the solver's chosen set
  expect_true(is.na(p4$border_neq))
  expect_equal(p4$border_share, 535 / 3485)
  # nothing viable at 40 tasks
  expect_null(.auto_partition(static_stats, n_tasks = 40L))
  # nested geometry on the intertemporal fixture
  pi <- .auto_partition(inter_stats, n_tasks = 4L)
  expect_identical(pi$set, "reg")
  expect_identical(pi$n_blocks, 12L)
  expect_identical(pi$netcut, 294L)
  # no structure at all
  expect_null(.auto_partition(NULL, n_tasks = 2L))
})

test_that("plain static auto takes DBBD wherever a partition serves the tasks", {
  # DBBD beat LU at every measured plain rung (346k-7.69M): no size gate
  d <- .auto_decide(FALSE, 2L, 3485, structure = static_stats)
  expect_identical(d$method, "DBBD")
  expect_identical(d$source, "probe")
  expect_true(d$probed)
  expect_false(d$condensed)
  expect_identical(.auto_decide(FALSE, 2L, 2.5e6, structure = static_stats)$method, "DBBD")
  # not enough blocks for the tasks -> next candidate's border too wide
  expect_identical(.auto_decide(FALSE, 4L, 2.5e6, structure = static_stats)$method, "LU")
  # a border ceiling that admits demd's 15% border flips it
  wide <- th
  wide$border_share_max <- 0.2
  expect_identical(.auto_decide(FALSE, 4L, 2.5e6, structure = static_stats, th = wide)$method, "DBBD")
  # single task: LU, with the hint from the smallest measured rung up
  d1 <- .auto_decide(FALSE, 1L, 2.5e6, structure = static_stats)
  expect_identical(d1$method, "LU")
  expect_true(d1$dbbd_hint)
  expect_false(.auto_decide(FALSE, 1L, 3485, structure = static_stats)$dbbd_hint)
  # size unknown: the partition evidence still decides
  expect_identical(.auto_decide(FALSE, 2L, NULL, structure = static_stats)$method, "DBBD")
})

test_that("condensed static auto keeps threaded LU below the crossover", {
  cond <- static_stats
  cond$nbacksolve <- 60L
  cond$nbselems <- 60000
  # multi-step: threaded LU wins to 110k condensed eq, ties at 140k
  expect_identical(.auto_decide(FALSE, 2L, 1e5, structure = cond)$method, "LU")
  expect_identical(.auto_decide(FALSE, 2L, 1.3e5, structure = cond)$method, "DBBD")
  # Johansen crosses at 70k
  expect_identical(.auto_decide(FALSE, 2L, 1e5, structure = cond, multistep = FALSE)$method, "DBBD")
  expect_identical(.auto_decide(FALSE, 2L, 6e4, structure = cond, multistep = FALSE)$method, "LU")
  # the record carries the condensation and the plain-equivalent size
  d <- .auto_decide(FALSE, 2L, 1.3e5, structure = cond)
  expect_true(d$condensed)
  expect_equal(d$plain_size, 1.3e5 + 60000)
  # metadata-only: the deploy record supplies the same facts
  m <- .auto_decide(FALSE, 2L, 1.3e5, n_reg = 3L, condensed = TRUE, n_backsolve_ele = 60000)
  expect_identical(m$method, "DBBD")
  expect_identical(m$source, "metadata")
  expect_equal(m$plain_size, 1.9e5)
  expect_identical(.auto_decide(FALSE, 2L, 1e5, n_reg = 3L, condensed = TRUE)$method, "LU")
  # single task: the hint applies above the crossover
  expect_true(.auto_decide(FALSE, 1L, 1.3e5, n_reg = 3L, condensed = TRUE)$dbbd_hint)
  expect_false(.auto_decide(FALSE, 1L, 1e5, n_reg = 3L, condensed = TRUE)$dbbd_hint)
})

test_that("static auto without a probe decides from the region count", {
  # plain static above the probe cap: DBBD when the regions serve the tasks
  expect_identical(.auto_decide(FALSE, 2L, 2.5e6, n_reg = 3L)$method, "DBBD")
  expect_identical(.auto_decide(FALSE, 4L, 2.5e6, n_reg = 3L)$method, "LU")
  d <- .auto_decide(FALSE, 2L, 2.5e6)
  expect_identical(d$method, "LU")
  expect_identical(d$source, "metadata")
  expect_false(d$probed)
  expect_identical(.auto_decide(FALSE, 2L, NULL)$source, "none")
  expect_true(.auto_decide(FALSE, 1L, 2.5e6, n_reg = 3L)$dbbd_hint)
  expect_false(.auto_decide(FALSE, 1L, 2e5, n_reg = 3L)$dbbd_hint)
})

test_that("the 32-bit LU workspace ceiling is a hard exclusion", {
  # measured anchors (HPC matrix 2026-08): every rig that actually
  # factorized must stay allowed -- S-full ran at LA 1.669e9 and
  # S-full-cond at 1.764e9, both under the 2147483647 ceiling
  expect_false(.auto_lu_ceiling(138885939, FALSE, th)$exceeded)
  expect_false(.auto_lu_ceiling(44095569, TRUE, th)$exceeded)
  expect_false(.auto_lu_ceiling(85498365, FALSE, th)$exceeded)
  # ... but the two that ran near the wall are flagged
  expect_true(.auto_lu_ceiling(138885939, FALSE, th)$near)
  expect_true(.auto_lu_ceiling(44095569, TRUE, th)$near)
  expect_false(.auto_lu_ceiling(85498365, FALSE, th)$near)
  # double either rig and LU is off the table
  expect_true(.auto_lu_ceiling(277771878, FALSE, th)$exceeded)
  expect_true(.auto_lu_ceiling(88191138, TRUE, th)$exceeded)
  # condensation trades nnz for fill: the same nnz is nearer the
  # ceiling when the deployment is condensed
  expect_gt(
    .auto_lu_ceiling(44095569, TRUE, th)$share,
    .auto_lu_ceiling(44095569, FALSE, th)$share
  )
  # no nonzero count -> the exclusion cannot be applied
  expect_null(.auto_lu_ceiling(NULL, FALSE, th))
  expect_null(.auto_lu_ceiling(NA_real_, FALSE, th))
})

test_that("an excluded LU falls through to the bordered method", {
  # a condensed small system every performance gate leaves on LU
  cond_small <- static_stats
  cond_small$nbacksolve <- 60L
  expect_identical(.auto_decide(FALSE, 2L, 3485, structure = cond_small)$method, "LU")
  big <- cond_small
  big$nnz <- 500e6
  # the ceiling overrides the crossover and the border-share guard
  d <- .auto_decide(FALSE, 2L, 3485, structure = big)
  expect_identical(d$method, "DBBD")
  expect_true(d$lu_excluded)
  expect_false(d$lu_unavoidable)
  # and it applies at a single task, where the crossover never would
  d1 <- .auto_decide(FALSE, 1L, 3485, structure = big)
  expect_identical(d1$method, "DBBD")
  expect_true(d1$lu_excluded)
  # no viable partition: LU stands, flagged as expected to abort
  nopart <- big
  nopart$partition_auto <- NULL
  d2 <- .auto_decide(FALSE, 2L, 3485, structure = nopart)
  expect_identical(d2$method, "LU")
  expect_true(d2$lu_excluded)
  expect_true(d2$lu_unavoidable)
  # under the ceiling nothing changes
  small <- cond_small
  small$nnz <- 1e6
  expect_identical(.auto_decide(FALSE, 2L, 3485, structure = small)$method, "LU")
  expect_false(.auto_decide(FALSE, 2L, 3485, structure = small)$lu_excluded)
  # a probe with no nnz leaves the exclusion unapplied
  expect_false(.auto_decide(FALSE, 2L, 2.5e6, structure = static_stats)$lu_excluded)
})

test_that("the memory model reproduces the ladder's binding cells", {
  # S90P DBBD at 2 ranks (7.69M plain): 10.8-11.2 GB measured
  expect_equal(.auto_memory_gb("DBBD", 2L, 7.69e6), 7.69e6 * (0.85 + 2 * 0.27) / 1e6)
  expect_lt(abs(.auto_memory_gb("DBBD", 2L, 7.69e6) - 10.7), 0.1)
  # I-long-big SBBD at 8 ranks (21.9M): 9.98 GB measured, within 15 %
  expect_lt(abs(.auto_memory_gb("SBBD", 8L, 21.9e6) / 9.98 - 1), 0.15)
  # condensed DBBD = 1.55x the plain rig's DBBD at the same plain size
  expect_equal(
    .auto_memory_gb("DBBD", 2L, 3.77e6, condensed = TRUE) / .auto_memory_gb("DBBD", 2L, 3.77e6),
    1.55
  )
  # NDBBD on one rank at Q34 (234M): tables 34.7 GB measured, estimate above it
  expect_equal(.auto_memory_gb("NDBBD", 1L, 234e6), 234 * 0.20)
  expect_gt(.auto_memory_gb("NDBBD", 1L, 234e6), 34.7)
  # unknown size or method
  expect_true(is.na(.auto_memory_gb("LU", 1L, NA_real_)))
  expect_true(is.na(.auto_memory_gb("LU", 1L, NULL)))
  expect_true(is.na(.auto_memory_gb("other", 1L, 1e6)))
})

test_that("intertemporal auto is SBBD, NDBBD only through the memory arm", {
  expect_identical(.auto_decide(TRUE, 4L, 10524)$method, "SBBD")
  expect_identical(.auto_decide(TRUE, 4L, 10524, structure = inter_stats)$method, "SBBD")
  expect_identical(.auto_decide(TRUE, 32L, 4.4e6, structure = inter_stats)$method, "SBBD")
  d <- .auto_decide(TRUE, 4L, 10524, structure = inter_stats)
  expect_identical(d$chain_set, "alltime")
  expect_identical(d$chain_border, 18L)
  # Q34-sized system (234M) on a 60 GB container at one task: SBBD's
  # 89 GB does not fit, NDBBD's 47 GB does
  d <- .auto_decide(TRUE, 1L, 234e6, mem_limit_gb = 60)
  expect_identical(d$method, "NDBBD")
  expect_true(d$memory_arm)
  expect_equal(d$memory$estimates$SBBD, 234 * 0.38)
  expect_equal(d$memory$estimates$NDBBD, 234 * 0.20)
  expect_equal(d$memory$chosen_gb, 234 * 0.20)
  # at 8 tasks on 125 GB neither fits: SBBD stands and the fit check speaks
  d <- .auto_decide(TRUE, 8L, 234e6, mem_limit_gb = 125)
  expect_identical(d$method, "SBBD")
  expect_false(d$memory_arm)
  # unknown limit: the arm is inert
  expect_identical(.auto_decide(TRUE, 1L, 234e6)$method, "SBBD")
  expect_true(is.na(.auto_decide(TRUE, 1L, 234e6)$memory$limit_gb))
})

test_that("DBBD is not chosen where its estimate does not fit the container", {
  # S90C-like: 230k condensed of 7.69M plain on a 12 GB laptop -> the
  # condensed DBBD estimate (16.6 GB) exceeds it, LU stays
  d <- .auto_decide(FALSE, 2L, 2.3e5,
    n_reg = 30L, condensed = TRUE, n_backsolve_ele = 7.46e6, mem_limit_gb = 12
  )
  expect_identical(d$method, "LU")
  expect_true(d$dbbd_memory_blocked)
  # S56C-like: 140k condensed of 3.77M plain -> 8.1 GB fits
  d <- .auto_decide(FALSE, 2L, 1.4e5,
    n_reg = 30L, condensed = TRUE, n_backsolve_ele = 3.63e6, mem_limit_gb = 12
  )
  expect_identical(d$method, "DBBD")
  expect_false(d$dbbd_memory_blocked)
})

test_that("a declared intertemporal model without a chain falls to the static family", {
  d <- .auto_decide(TRUE, 2L, 2.5e6, structure = static_stats)
  expect_true(d$no_chain)
  expect_identical(d$method, "DBBD")
  expect_identical(.auto_decide(TRUE, 1L, 10524, structure = static_stats)$method, "LU")
})

test_that("resources auto follows the measured rank and thread rules", {
  laptop4 <- list(cores = 4L, mem_gb = 12)
  laptop8 <- list(cores = 8L, mem_gb = 12)
  box <- list(cores = 32L, mem_gb = 125)
  split <- function(r) c(r$n_tasks, r$n_threads)
  # SBBD: ranks to the knee (cap 8), threads take the rest
  expect_identical(split(.resolve_resources("SBBD", laptop8)), c(8L, 1L))
  expect_identical(split(.resolve_resources("SBBD", laptop4)), c(4L, 1L))
  expect_identical(split(.resolve_resources("SBBD", box)), c(8L, 4L))
  expect_identical(split(.resolve_resources("SBBD", box, n_blocks = 3L)), c(3L, 8L))
  # DBBD: two ranks on a laptop, up to eight on a box
  expect_identical(split(.resolve_resources("DBBD", laptop8)), c(2L, 4L))
  expect_identical(split(.resolve_resources("DBBD", laptop4)), c(2L, 2L))
  expect_identical(split(.resolve_resources("DBBD", box)), c(8L, 4L))
  # memory pulls the DBBD rank count back (S-full 40.5M: 8 ranks 122 GB, 4 ranks 78 GB)
  expect_identical(split(.resolve_resources("DBBD", box, plain_size = 40.5e6)), c(4L, 8L))
  # LU: one rank, threads for the condensed factorization
  expect_identical(split(.resolve_resources("LU", laptop8)), c(1L, 8L))
  expect_identical(split(.resolve_resources("LU", box)), c(1L, 8L))
  # NDBBD: one rank, every core (the solver budgets its regions)
  expect_identical(split(.resolve_resources("NDBBD", box)), c(1L, 32L))
  # explicit values stay, the rest is resolved around them
  r <- .resolve_resources("SBBD", box,
    requested = list(n_tasks = 2L, n_threads = 3L, inmemory = FALSE),
    explicit = c("n_tasks", "inmemory")
  )
  expect_identical(split(r), c(2L, 8L))
  expect_false(r$inmemory)
  expect_null(.resolve_resources("SBBD", box)$inmemory)
  # no host: one task, one thread
  expect_identical(split(.resolve_resources("SBBD", NULL)), c(1L, 1L))
})

test_that("the memory fit check refuses a run past the error band and warns inside it", {
  laptop <- list(cores = 8L, mem_gb = 12)
  expect_identical(.memory_fit_check("SBBD", 4L, 4.5e6, host = laptop)$verdict, "fits")
  # 95 % of the container: inside the band, warned
  expect_warning(
    rec <- .memory_fit_check("DBBD", 2L, 8.2e6, host = laptop),
    "may not fit"
  )
  expect_identical(rec$verdict, "tight")
  expect_equal(rec$share, 8.2 * 1.39 / 12)
  # past the band: refused by name
  expect_snapshot_error(.memory_fit_check("DBBD", 4L, 7.69e6, host = laptop))
  # unknown size or container: not applied
  expect_identical(.memory_fit_check("DBBD", 4L, NA_real_, host = laptop)$verdict, "unknown")
  expect_identical(.memory_fit_check("DBBD", 4L, 7.69e6)$verdict, "unknown")
})

test_that("evidence and record lines render every input", {
  d <- .auto_decide(FALSE, 2L, 2.5e6, structure = static_stats)
  expect_match(.auto_evidence(d), "^2,500,000 equations, no chain, partition reg \\(3 blocks, border 6.4%\\), n_tasks 2$")
  d <- .auto_decide(TRUE, 4L, 10524, structure = inter_stats)
  expect_match(.auto_evidence(d), "chain alltime \\(3 blocks\\), partition reg \\(12 blocks, border 2.8%\\)")
  d <- .auto_decide(FALSE, 4L, 3485)
  expect_match(.auto_evidence(d), "^3,485 equations, n_tasks 4; structural probe skipped \\(not a candidate\\)")
  d <- .auto_decide(FALSE, 2L, 1.3e5, n_reg = 3L, condensed = TRUE, n_backsolve_ele = 60000)
  expect_match(.auto_evidence(d), "^130,000 equations \\(condensed\\), n_tasks 2; structural probe skipped")
  # the solve record
  host <- list(cores = 8L, mem_gb = 12)
  r <- .resolve_resources("SBBD", host, plain_size = 4.5e6)
  r$fit <- .memory_fit_check("SBBD", r$n_tasks, 4.5e6, host = host)
  lines <- .resources_record_lines(r)
  expect_length(lines, 2L)
  expect_match(lines[1], "^Resources: n_tasks 8, n_threads 1, inmemory solver default, tempdir solver default \\(container 8 core\\(s\\), 12.00 GB\\)$")
  expect_match(lines[2], "^  memory check: SBBD at 8 task\\(s\\) estimated 2.34 GB = 0.52 kB/eq x 4,500,000 plain-equivalent equations -> (19|20)% of 12.00 GB \\(fits\\)$")
  manual <- list(method = "LU", n_tasks = 1L, n_threads = 1L, inmemory = FALSE, cores = NULL, mem_gb = NULL, tempdir = "/tmp")
  lines <- .resources_record_lines(manual)
  expect_match(lines[1], "^Resources: n_tasks 1, n_threads 1, inmemory false, tempdir /tmp \\(container not inspected\\)$")
  expect_match(lines[2], "not applied")
  expect_null(.resources_record_lines(NULL))
  # the fit check in report mode never aborts
  expect_identical(.memory_fit_check("DBBD", 4L, 7.69e6, host = host, report_only = TRUE)$verdict, "exceeds")
  expect_identical(.memory_fit_check("DBBD", 2L, 8.2e6, host = host, report_only = TRUE)$verdict, "tight")
})
