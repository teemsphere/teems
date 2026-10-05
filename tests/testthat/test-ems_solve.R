skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "solve")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

static_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

dynamic_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

dynamic_model <- "GTAP-RE"
dynamic_model_files <- ems_example(dynamic_model, write_dir)
dynamic_model_file <- dynamic_model_files[["model_file"]]
dynamic_closure_file <- dynamic_model_files[["closure_file"]]
dynamic_model <- ems_model(dynamic_model_file, dynamic_closure_file)

static_model <- "GTAPv7"
static_model_files <- ems_example(static_model, write_dir)
static_model_file <- static_model_files[["model_file"]]
static_closure_file <- static_model_files[["closure_file"]]
# the vetted GTAPv7 file condenses on load (gtapv7.sti); the fixture for
# the tests below is the full system
static_model <- suppressMessages(suppressWarnings(
  ems_model(static_model_file, static_closure_file, ignore_condense = TRUE)
))


# the shock behind every equivalence test below: a real productivity
# shock, so that alternative TAB forms, condensation and the solution /
# matrix methods are compared on a non-trivial solution (a numeraire
# shock scales every price uniformly and cannot discriminate a defect)
real_shock <- ems_uniform_shock("aoall", 5)
# the intertemporal model at three time steps is under-resolved by
# Gragg 2-4-8 on two subintervals under +5 (43% of variables to four
# digits, an accuracy warning); +2 resolves it without one
dynamic_shock <- ems_uniform_shock("aoall", 2)

# snapshot normaliser: the log stamp and the resolved image tag are
# environment-specific
# path, log-name and tag scrubbing: scrub_paths (helper-script_run.R)

test_that("ems_solve suppress_outputs returns cmf_path character", {
  nest_temp("solve_suppress", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  result <- ems_solve(cmf_path, suppress_outputs = TRUE)
  expect_type(result, "NULL")
})

test_that("ems_solve errors when cmf_path is missing", {
  expect_snapshot_error(ems_solve())
})

test_that("ems_solve errors when n_tasks is not integerish", {
  nest_temp("solve_err_tasks", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, n_tasks = 1.5))
})

test_that("ems_solve errors when steps is not one to three whole numbers", {
  nest_temp("solve_err_steps", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, steps = c(2L, 4L, 6L, 8L)))
  expect_snapshot_error(ems_solve(cmf_path, solution_method = "Euler", steps = 2.5))
})

test_that("ems_solve errors when Gragg steps mix parity", {
  nest_temp("solve_err_parity", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(
    ems_solve(cmf_path, solution_method = "Gragg", steps = c(2L, 3L, 4L))
  )
})

test_that("a scalar variable shock is written without sets or uniform", {
  nest_temp("solve_scalar_shock", write_dir)
  cmf_path <- ems_deploy(static_data, static_model, ems_uniform_shock("pfactwld", 1))
  shf <- list.files(dirname(cmf_path), pattern = "\\.shf$", full.names = TRUE)
  expect_true(any(grepl("Shock pfactwld = 1;", unlist(lapply(shf, readLines)), fixed = TRUE)))
  outputs <- ems_solve(cmf_path, solution_method = "Johansen")
  expect_equal(outputs$dat[[match("pfactwld", outputs$name)]]$Value, 1, tolerance = 1e-6)
  shocks <- c(plain = "Shock pfactwld = 1;\n", uniform = "Shock pfactwld = uniform 1;\n")
  for (form in names(shocks)) {
    shf <- tempfile(fileext = ".shf")
    cat(shocks[[form]], file = shf)
    nest_temp(paste0("solve_scalar_shock_file_", form), write_dir)
    cmf_path <- ems_deploy(static_data, static_model, shock_file = shf)
    outputs <- ems_solve(cmf_path, solution_method = "Johansen")
    expect_equal(outputs$dat[[match("pfactwld", outputs$name)]]$Value, 1, tolerance = 1e-6)
  }
})

test_that("single multi-step runs and odd Gragg step counts solve", {
  nest_temp("solve_single_run", write_dir)
  cmf_path <- ems_deploy(static_data, static_model, real_shock)
  ref <- ems_solve(cmf_path, solution_method = "Gragg", steps = c(2L, 4L, 8L))
  qgdp <- \(o) o$dat[[match("qgdp", o$name)]]$Value
  exec <- file.path(dirname(cmf_path), "model_exec.txt")
  for (m in c("Euler", "Gragg")) {
    single <- ems_solve(cmf_path, solution_method = m, steps = 16L)
    expect_equal(qgdp(single), qgdp(ref), tolerance = 1e-2, label = m)
  }
  odd <- ems_solve(cmf_path, solution_method = "Gragg", steps = c(3L, 5L, 7L))
  expect_equal(qgdp(odd), qgdp(ref), tolerance = 1e-4)
  cmd <- .construct_cmd(
    paths = list(run = "/r", docker_cmf = "/c", docker_run = "/d", cmf = cmf_path),
    terminal_run = FALSE, timeID = "t", n_tasks = 1L, n_subintervals = 1L,
    solmed = "Euler", matsol = 0L, steps = 16L,
    assertions = .o_assertions(),
    range_test_initial = .o_range_test_initial(),
    range_test_updated = .o_range_test_updated()
  )
  expect_match(cmd$solve, "-step1 16 -single_run 1", fixed = TRUE)
  expect_false(grepl("-step2", cmd$solve, fixed = TRUE))
  expect_false(grepl("-random_seed", cmd$solve, fixed = TRUE))
})

test_that("the midpoint method solves and extrapolates", {
  nest_temp("solve_midpoint", write_dir)
  cmf_path <- ems_deploy(static_data, static_model, real_shock)
  qgdp <- \(o) o$dat[[match("qgdp", o$name)]]$Value
  ref <- ems_solve(cmf_path, solution_method = "Gragg", steps = c(2L, 4L, 8L))
  mid <- ems_solve(cmf_path, solution_method = "Midpoint", steps = c(2L, 4L, 8L))
  expect_equal(qgdp(mid), qgdp(ref), tolerance = 1e-4)
  single <- ems_solve(cmf_path, solution_method = "Midpoint", steps = 16L)
  expect_equal(qgdp(single), qgdp(ref), tolerance = 1e-2)
  cmd <- .construct_cmd(
    paths = list(run = "/r", docker_cmf = "/c", docker_run = "/d", cmf = cmf_path),
    terminal_run = FALSE, timeID = "t", n_tasks = 1L, n_subintervals = 1L,
    solmed = "Midpoint", matsol = 0L, steps = c(2L, 4L, 8L),
    assertions = .o_assertions(),
    range_test_initial = .o_range_test_initial(),
    range_test_updated = .o_range_test_updated()
  )
  expect_match(cmd$solve, "-step1 2 -step2 4 -step3 8", fixed = TRUE)
  expect_match(cmd$solve, "-solmed Midpoint", fixed = TRUE)
  expect_snapshot_error(
    ems_solve(cmf_path, solution_method = "Midpoint", steps = c(2L, 3L, 4L))
  )
})

test_that("two-solution runs and the convergence rule solve", {
  nest_temp("solve_two_run", write_dir)
  withr::defer(ems_option_set(convergence_rule = "off"))
  cmf_path <- ems_deploy(static_data, static_model, real_shock)
  qgdp <- \(o) o$dat[[match("qgdp", o$name)]]$Value
  ref <- ems_solve(cmf_path, solution_method = "Gragg", steps = c(2L, 4L, 8L))
  cmd <- function(steps) {
    .construct_cmd(
      paths = list(run = "/r", docker_cmf = "/c", docker_run = "/d", cmf = cmf_path),
      terminal_run = FALSE, timeID = "t", n_tasks = 1L, n_subintervals = 1L,
      solmed = "Gragg", matsol = 0L, steps = steps,
      assertions = .o_assertions(),
      range_test_initial = .o_range_test_initial(),
      range_test_updated = .o_range_test_updated()
    )$solve
  }
  expect_match(cmd(c(2L, 4L)), "-step1 2 -step2 4 -two_run 1", fixed = TRUE)
  expect_false(grepl("-convrule", cmd(c(2L, 4L, 8L)), fixed = TRUE))
  two <- ems_solve(cmf_path, solution_method = "Gragg", steps = c(2L, 4L))
  expect_equal(qgdp(two), qgdp(ref), tolerance = 1e-3)
  ems_option_set(convergence_rule = "on")
  expect_match(cmd(c(2L, 4L, 8L)), "-step3 8 -convrule 1", fixed = TRUE)
  ruled <- ems_solve(cmf_path, solution_method = "Gragg", steps = c(2L, 4L, 8L))
  expect_equal(qgdp(ruled), qgdp(ref), tolerance = 1e-4)
})

test_that("ems_solve errors when steps are not increasing", {
  nest_temp("solve_err_increasing", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(
    ems_solve(cmf_path, solution_method = "Gragg", steps = c(4L, 4L, 8L))
  )
  # odd steps are fine under Euler (no parity rule): the decreasing
  # order alone must trigger the abort
  expect_snapshot_error(
    ems_solve(cmf_path, solution_method = "Euler", steps = c(9L, 5L, 3L))
  )
})

test_that("ems_solve errors on invalid Runge-Kutta arguments", {
  nest_temp("solve_err_rk", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  # RK methods take a single step count, not the extrapolation triple
  expect_snapshot_error(
    ems_solve(cmf_path, solution_method = "RK4", steps = c(2L, 4L, 8L))
  )
  expect_snapshot_error(
    ems_solve(cmf_path, solution_method = "DoPri54", steps = 2.5)
  )
  # adaptive control needs an embedded pair
  expect_snapshot_error(
    ems_solve(cmf_path, solution_method = "RK4", steps = 8L, adaptive = "yes")
  )
  # subintervals only benefit the extrapolating methods
  expect_snapshot_error(
    ems_solve(cmf_path,
      solution_method = "BoSha32", steps = 8L,
      n_subintervals = 2L
    )
  )
  expect_snapshot_error(
    ems_solve(cmf_path,
      solution_method = "DoPri54", steps = 8L,
      adaptive = "yes", eps_tolerance = -0.1
    )
  )
})

test_that("ems_solve errors when SBBD used with static model", {
  nest_temp("solve_err_sbbd", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, matrix_method = "SBBD"))
})

test_that("ems_solve errors when inmemory is not a logical scalar", {
  nest_temp("solve_err_inmemory", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, inmemory = "yes"))
  expect_snapshot_error(ems_solve(cmf_path, inmemory = c(TRUE, FALSE)))
})

test_that("ems_solve errors when verbosity is invalid", {
  nest_temp("solve_err_verbosity", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, verbosity = 1.5))
  expect_snapshot_error(ems_solve(cmf_path, verbosity = 3L))
})

test_that("inmemory and verbosity reach the solver command", {
  nest_temp("solve_flags_cmd", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  run_dir <- dirname(cmf_path)
  suppressMessages(
    ems_solve(cmf_path,
      inmemory = FALSE,
      verbosity = 0L,
      terminal_run = TRUE
    )
  )
  cmd <- readLines(file.path(run_dir, "model_exec.txt"), warn = FALSE)
  expect_match(paste(cmd, collapse = " "), "-inmemory 0", fixed = TRUE)
  expect_match(paste(cmd, collapse = " "), "-verbosity 0", fixed = TRUE)

  # defaults: inmemory is the solver's per-method choice and stays off
  # the command; verbosity has a literal default and is always recorded
  suppressMessages(ems_solve(cmf_path, terminal_run = TRUE))
  cmd <- readLines(file.path(run_dir, "model_exec.txt"), warn = FALSE)
  expect_no_match(paste(cmd, collapse = " "), "-inmemory", fixed = TRUE)
  expect_match(paste(cmd, collapse = " "), "-verbosity 1", fixed = TRUE)
  expect_match(paste(cmd, collapse = " "), "-assertions 2 -range_test_initial 1 -range_test_updated 1", fixed = TRUE)
})

test_that("Runge-Kutta flags reach the solver command", {
  nest_temp("solve_rk_cmd", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  run_dir <- dirname(cmf_path)
  suppressMessages(
    ems_solve(cmf_path,
      solution_method = "DoPri54", steps = 6L,
      adaptive = "yes", eps_tolerance = 0.01,
      terminal_run = TRUE
    )
  )
  cmd <- paste(readLines(file.path(run_dir, "model_exec.txt"), warn = FALSE), collapse = " ")
  expect_match(cmd, "-solmed DoPri54", fixed = TRUE)
  expect_match(cmd, "-step1 6", fixed = TRUE)
  expect_no_match(cmd, "-step2", fixed = TRUE)
  expect_match(cmd, "-adaptive yes -epstol 0.01", fixed = TRUE)

  # fixed-step runs pass neither adaptive flag
  suppressMessages(
    ems_solve(cmf_path, solution_method = "RK4", steps = 8L, terminal_run = TRUE)
  )
  cmd <- paste(readLines(file.path(run_dir, "model_exec.txt"), warn = FALSE), collapse = " ")
  expect_match(cmd, "-solmed RK4", fixed = TRUE)
  expect_match(cmd, "-step1 8", fixed = TRUE)
  expect_no_match(cmd, "-adaptive", fixed = TRUE)
})

test_that("ems_solve errors when solution errors detected", {
  nest_temp("solve_err_error", write_dir)
  shock <- ems_uniform_shock("pop", 1e6)
  cmf_path <- ems_deploy(static_data, static_model, shock)
  # the range test on updated values is a warning by default (manual
  # 25.4.4); make it fatal so the bound violations abort the run
  ems_option_set(range_test_updated = "fatal")
  withr::defer(ems_option_set(range_test_updated = "warn"))
  expect_snapshot(ems_solve(cmf_path),
    error = TRUE,
    transform = scrub_paths
  )
})

test_that("ems_deploy errors when the closure does not square the system", {
  nest_temp("solve_err_sing", write_dir)
  # an unpaired swap_out leaves 3 too few exogenous elements; the
  # count-squaring pre-flight rejects it before any solver call
  expect_snapshot(ems_deploy(static_data, static_model, swap_out = "pop"),
    error = TRUE,
    transform = scrub_paths
  )
})

test_that("ems_solve warns when poor accuracy", {
  nest_temp("solve_wrn_accur", write_dir)
  shock <- ems_uniform_shock("pop", 200)
  cmf_path <- ems_deploy(static_data, static_model, shock)
  expect_snapshot_warning(ems_solve(
    cmf_path,
    solution_method = "Gragg"
  ))
})

test_that("ems_solve returns NULL when suppress_outputs", {
  nest_temp("suppress", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_null(ems_solve(cmf_path, suppress_outputs = TRUE))
})

test_that("ems_solve informs terminal run", {
  nest_temp("solve_info_terminal", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot(
    ems_solve(cmf_path, terminal_run = TRUE),
    transform = scrub_paths
  )
})

test_that("a second solve in one deploy directory is refused by name", {
  nest_temp("solve_lock", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  lock <- file.path(dirname(cmf_path), ".solve_lock")
  dir.create(lock)
  # a claim held by another process: concurrent runs are supported a
  # deploy directory at a time, and two in one directory would share the
  # solver's scratch files and the outputs the CMF names
  writeLines("run 010203_999 (pid 999) started 2026-01-01 01:02:03 UTC", file.path(lock, "owner"))
  expect_snapshot(ems_solve(cmf_path), error = TRUE, transform = scrub_paths)

  # released, the directory takes a solve again, and the solve leaves no
  # claim behind
  unlink(lock, recursive = TRUE)
  expect_s3_class(ems_solve(cmf_path), "data.frame")
  expect_false(dir.exists(lock))
})

test_that("matrix_method has no auto: the run is what is given, and the record says what ran", {
  nest_temp("solve_manual_record", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_error(ems_solve(cmf_path, matrix_method = "auto"), "must be one of")
  out <- ems_solve(cmf_path)
  expect_s3_class(out, "data.frame")
  expect_false(file.exists(file.path(
    dirname(cmf_path), "out", "variables", "bin", "sol.probe.json"
  )))
  record <- readLines(file.path(dirname(cmf_path), "model_diagnostics.txt"))
  expect_true(any(grepl("^Matrix method: LU", record)))
  expect_true(any(grepl("^Resources: n_tasks 1, n_threads 1, inmemory solver default, tempdir solver default \\(container ", record)))
  expect_true(any(grepl("^  memory check: LU at 1 task\\(s\\) estimated .* plain-equivalent equations -> [0-9]+% of .* \\(fits\\)$", record)))
  # a scratch-backed run gets its scratch inside the container
  # filesystem (docker's /dev/shm is 64 MB by default) and the record
  # says so
  nest_temp("solve_manual_record_dyn", write_dir)
  cmf_path <- ems_deploy(dynamic_data, dynamic_model)
  metadata <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))
  expect_identical(metadata$n_time, 3L)
  out <- ems_solve(cmf_path, solution_method = "Gragg", matrix_method = "NDBBD", n_tasks = 2L)
  expect_s3_class(out, "data.frame")
  record <- readLines(file.path(dirname(cmf_path), "model_diagnostics.txt"))
  expect_true(any(grepl("^Resources: n_tasks 2, n_threads 1, inmemory solver default, tempdir /tmp \\(container ", record)))
  expect_match(readLines(file.path(dirname(cmf_path), "model_exec.txt")), "-tempdir /tmp", fixed = TRUE, all = FALSE)
})

test_that("deploy metadata records system size", {
  nest_temp("solve_size_meta", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  metadata <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))
  expect_identical(metadata$system_size, 3494)
  expect_identical(metadata$n_reg, 3L)
})

test_that("set expressions solve identically to pairwise forms", {
  nest_temp("solve_set_expr_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model)
  base <- ems_solve(cmf_base)

  nest_temp("solve_set_expr", write_dir)
  expr_file <- write_modified_model(
    static_model_file,
    NULL,
    .fn = function(m, t) {
      old1 <- 'COSTS # industry cost summary # = "IntDom" + "IntImp" + ENDW + "PTAX"  ;'
      old2 <- "ENDWM # mobile endowments # = ENDW - ENDWFS;"
      stopifnot(grepl(old1, m, fixed = TRUE), grepl(old2, m, fixed = TRUE))
      m <- sub(old1, 'COSTS # industry cost summary # = (("IntDom" + "IntImp") UNION ENDW) + "PTAX";', m, fixed = TRUE)
      sub(old2, "ENDWM # mobile endowments # = ENDW - ENDWF - ENDWS;", m, fixed = TRUE)
    }
  )
  expr_model <- ems_model(expr_file, static_closure_file, ignore_condense = TRUE)
  cmf_expr <- ems_deploy(static_data, expr_model)
  expr_out <- ems_solve(cmf_expr)
  expect_equal(expr_out, base)
})

test_that("set equality solves identically through an equation quantifier", {
  nest_temp("solve_set_eq_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model)
  base <- ems_solve(cmf_base)

  nest_temp("solve_set_eq", write_dir)
  eq_file <- write_modified_model(
    static_model_file,
    NULL,
    .fn = function(m, t) {
      old1 <- "ENDWC is subset of ENDWMS;"
      old2 <- paste0(
        "# defines the real (tax-inclusive) return to mobile and sluggish factor e in r #\r\n",
        "(all,e,ENDWMS)(all,r,REG)"
      )
      stopifnot(grepl(old1, m, fixed = TRUE), grepl(old2, m, fixed = TRUE))
      m <- sub(
        old1,
        paste0(old1, "\r\nSet\r\n    ENDWMS2 # identical to ENDWMS # = ENDWMS;"),
        m,
        fixed = TRUE
      )
      sub(old2, sub("ENDWMS)", "ENDWMS2)", old2, fixed = TRUE), m, fixed = TRUE)
    }
  )
  eq_model <- ems_model(eq_file, static_closure_file, ignore_condense = TRUE)
  cmf_eq <- ems_deploy(static_data, eq_model)
  eq_out <- ems_solve(cmf_eq)
  expect_equal(eq_out, base)
})

test_that("conditional set builders resolve identically in R and the solver (manual 10.1.2)", {
  nest_temp("solve_setbuild_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model, real_shock)
  base <- ems_solve(cmf_base)
  base_size <- readRDS(file.path(dirname(cmf_base), "metadata.rds"))$system_size

  # expected selection from the aggregated data R deploys
  evfb <- static_data[["EVFB"]]
  expected <- unique(evfb$ENDW[evfb$ACTS == "food" & evfb$REG == "chn" & evfb$Value > 0])
  expect_gt(length(expected), 0L)
  expect_lt(length(expected), length(unique(evfb$ENDW)))

  nest_temp("solve_setbuild", write_dir)
  sb_file <- write_modified_model(
    static_model_file,
    paste(
      'Set ENDWX # endowments used by food in chn # = (all,e,ENDW: EVFB(e,"food","chn") > 0);',
      "Variable (all,e,ENDWX) sbx(e) # builder-domain probe #;",
      'Equation E_sbx (all,e,ENDWX) sbx(e) = qfe(e,"food","chn");',
      sep = "\n"
    )
  )
  sb_model <- ems_model(sb_file, static_closure_file, ignore_condense = TRUE)
  # the builder statement reaches the solver verbatim; R mirrors it
  expect_true(any(grepl("= (all,e,ENDW: EVFB", sb_model$tab, fixed = TRUE)))
  cmf_sb <- ems_deploy(static_data, sb_model, real_shock)
  # R-side elements size the system
  sb_size <- readRDS(file.path(dirname(cmf_sb), "metadata.rds"))$system_size
  expect_equal(sb_size, base_size + length(expected))
  # solver-side elements agree: the probe equation resolves over the
  # same elements and the base solution is untouched
  sb_out <- ems_solve(cmf_sb)
  # the extra rows reorder the factorization: the base solution agrees
  # to solver noise, not bit-for-bit as in the pure set-rewrite tests
  rest <- sb_out[sb_out$name != "sbx", ]
  expect_identical(rest[c("name", "label", "type")], base[c("name", "label", "type")])
  for (k in seq_len(nrow(base))) {
    a <- rest$dat[[k]]
    b <- base$dat[[k]]
    expect_identical(names(a), names(b))
    expect_lt(max(abs(a$Value - b$Value)), 1e-6)
  }
  sbx <- sb_out$dat[[which(sb_out$name == "sbx")]]
  expect_setequal(sbx$ENDWXe, expected)
  qfe <- sb_out$dat[[which(sb_out$name == "qfe")]]
  qfe <- qfe[qfe$ACTSa == "food" & qfe$REGr == "chn" & qfe$ENDWe %in% expected, ]
  expect_true(any(qfe$Value != 0))
  expect_equal(
    sbx$Value[match(expected, sbx$ENDWXe)],
    qfe$Value[match(expected, qfe$ENDWe)]
  )
})

test_that("expression IF conditions solve identically to a hand-staged helper (LULC shape)", {
  probe <- function(cond_a, cond_b, extra = character(0)) {
    write_modified_model(
      static_model_file,
      paste(
        c(
          extra,
          "Variable (all,c,COMM)(all,r,REG) ifxv(c,r) # expression-condition probe #;",
          paste0(
            "Equation E_ifxv (all,c,COMM)(all,r,REG) ifxv(c,r) = IF[", cond_a,
            ", pds(c,r)] + IF[", cond_b, ", pms(c,r)];"
          ),
          "Coefficient (all,c,COMM)(all,r,REG) IFXF(c,r) # formula probe #;",
          paste0("Formula (initial) (all,c,COMM)(all,r,REG) IFXF(c,r) = IF[", cond_a, ", VDB(c,r)];")
        ),
        collapse = "\n"
      )
    )
  }
  nest_temp("solve_ifexpr_hand", write_dir)
  # 5e11 splits the big3/macro_sector data (helper values span 1e10-1e14)
  hand_file <- probe(
    "PRDX(c,r) > 5e11", "PRDX(c,r) <= 5e11",
    c(
      "Coefficient (all,c,COMM)(all,r,REG) PRDX(c,r) # hand-staged condition #;",
      "Formula (all,c,COMM)(all,r,REG) PRDX(c,r) = VDB(c,r)*VCB(c,r);"
    )
  )
  hand_model <- ems_model(hand_file, static_closure_file, ignore_condense = TRUE)
  hand_out_cmf <- ems_deploy(static_data, hand_model, real_shock)
  hand_out <- ems_solve(hand_out_cmf)

  nest_temp("solve_ifexpr", write_dir)
  expr_file <- probe(
    "VDB(c,r)*VCB(c,r) > 5e11", "VDB(c,r)*VCB(c,r) <= 5e11"
  )
  expr_model <- ems_model(expr_file, static_closure_file, ignore_condense = TRUE)
  # one (always) helper shared by the equation's two conditions, one
  # (initial) helper for the formula host
  expect_true(any(grepl("Formula (all,c,COMM)(all,r,REG) IFX1(c,r) = VDB(c,r)*VCB(c,r)", expr_model$tab, fixed = TRUE)))
  expect_true(any(grepl("Formula (initial) (all,c,COMM)(all,r,REG) IFX2(c,r) = VDB(c,r)*VCB(c,r)", expr_model$tab, fixed = TRUE)))
  expect_false(any(grepl("IFX3", expr_model$tab, fixed = TRUE)))
  cmf_expr <- ems_deploy(static_data, expr_model, real_shock)
  expr_out <- ems_solve(cmf_expr)

  ifxv <- expr_out$dat[[which(expr_out$name == "ifxv")]]
  expect_true(any(ifxv$Value != 0))
  expect_equal(ifxv, hand_out$dat[[which(hand_out$name == "ifxv")]])
  expect_equal(
    expr_out$dat[[which(expr_out$name == "pds")]],
    hand_out$dat[[which(hand_out$name == "pds")]]
  )
  # both branches fire somewhere in the data
  pds <- expr_out$dat[[which(expr_out$name == "pds")]]
  pms <- expr_out$dat[[which(expr_out$name == "pms")]]
  key <- paste(ifxv$COMMc, ifxv$REGr)
  a <- abs(ifxv$Value - pds$Value[match(key, paste(pds$COMMc, pds$REGr))]) < 1e-8
  b <- abs(ifxv$Value - pms$Value[match(key, paste(pms$COMMc, pms$REGr))]) < 1e-8
  helper <- expr_out$dat[[which(expr_out$name == "IFX1")]]
  hv <- helper$Value[match(key, paste(helper$COMMc, helper$REGr))]
  expect_true(any(hv > 5e11) && any(hv <= 5e11))
  expect_true(all(a[hv > 5e11]) && all(b[hv <= 5e11]))
  # the formula probe (initial IF over the same condition) composes
  # identically to the hand-staged one
  cf_expr <- ems_compose(cmf_expr, "IFXF")
  cf_hand <- ems_compose(file.path(dirname(hand_out_cmf), basename(hand_out_cmf)), "IFXF")
  expect_equal(cf_expr, cf_hand)
})

test_that("AND/OR/NOT and index conditions solve identically to hand-staged indicators (manual 11.4.5, 11.4.11)", {
  nest_temp("solve_cond_compound", write_dir)
  cond_file <- write_modified_model(
    static_model_file,
    paste(
      "Coefficient (all,c,COMM)(all,r,REG) CPX(c,r) # compound IF #;",
      "Formula (initial) (all,c,COMM)(all,r,REG) CPX(c,r) = IF[VDB(c,r) > 20*VMPB(c,r) and not VDB(c,r) > 4*VDPB(c,r), VDB(c,r)];",
      "Coefficient (all,c,COMM)(all,r,REG) OTH(c,r) # index condition in a sum #;",
      "Formula (initial) (all,c,COMM)(all,r,REG) OTH(c,r) = sum{s,REG: s <> r, VDB(c,s)};",
      "Variable (all,c,COMM)(all,r,REG) othv(c,r) # index compound over a variable #;",
      "Equation E_othv (all,c,COMM)(all,r,REG) othv(c,r) = sum{s,REG: [s <> r] and not [s = \"usa\"], VDB(c,s)*pds(c,s)};",
      "Variable (all,c,COMM)(all,r,REG) cmpv(c,r) # compound IF in an equation #;",
      "Equation E_cmpv (all,c,COMM)(all,r,REG) cmpv(c,r) = IF[VDB(c,r) > 20*VMPB(c,r) or r = \"usa\", pds(c,r)];",
      sep = "\n"
    )
  )
  cond_model <- ems_model(cond_file, static_closure_file, ignore_condense = TRUE)
  expect_true(any(grepl("sum{s,REG: [s <> r] and not [s = \"usa\"], VDB(c,s)*pds(c,s)}", cond_model$tab, fixed = TRUE)))
  cond_cmf <- ems_deploy(static_data, cond_model, real_shock)
  cond_out <- ems_solve(cond_cmf)

  nest_temp("solve_cond_hand", write_dir)
  hand_file <- write_modified_model(
    static_model_file,
    paste(
      "Coefficient (all,c,COMM)(all,r,REG) CPX(c,r) # hand-staged #;",
      "Formula (initial) (all,c,COMM)(all,r,REG) CPX(c,r) = 0;",
      "Formula (initial) (all,c,COMM)(all,r,REG: VDB(c,r) > 20*VMPB(c,r)) CPX(c,r) = VDB(c,r);",
      "Formula (initial) (all,c,COMM)(all,r,REG: VDB(c,r) > 4*VDPB(c,r)) CPX(c,r) = 0;",
      "Coefficient (all,c,COMM)(all,r,REG) OTH(c,r) # hand-staged #;",
      "Formula (initial) (all,c,COMM)(all,r,REG) OTH(c,r) = sum{s,REG, VDB(c,s)} - VDB(c,r);",
      "Coefficient (all,r,REG)(all,s,REG) HOS(r,s) # hand-staged #;",
      "Formula (all,r,REG)(all,s,REG) HOS(r,s) = 1;",
      "Formula (all,r,REG)(all,s,REG: $POS(s) = $POS(r)) HOS(r,s) = 0;",
      "Formula (all,r,REG)(all,s,REG: $POS(s) = $POS(\"usa\",REG)) HOS(r,s) = 0;",
      "Variable (all,c,COMM)(all,r,REG) othv(c,r) # hand-staged #;",
      "Equation E_othv (all,c,COMM)(all,r,REG) othv(c,r) = sum{s,REG, HOS(r,s)*VDB(c,s)*pds(c,s)};",
      "Coefficient (all,c,COMM)(all,r,REG) HCM(c,r) # hand-staged #;",
      "Formula (all,c,COMM)(all,r,REG) HCM(c,r) = 0;",
      "Formula (all,c,COMM)(all,r,REG: VDB(c,r) > 20*VMPB(c,r)) HCM(c,r) = 1;",
      "Formula (all,c,COMM)(all,r,REG: $POS(r) = $POS(\"usa\",REG)) HCM(c,r) = 1;",
      "Variable (all,c,COMM)(all,r,REG) cmpv(c,r) # hand-staged #;",
      "Equation E_cmpv (all,c,COMM)(all,r,REG) cmpv(c,r) = HCM(c,r)*pds(c,r);",
      sep = "\n"
    )
  )
  hand_model <- ems_model(hand_file, static_closure_file, ignore_condense = TRUE)
  hand_cmf <- ems_deploy(static_data, hand_model, real_shock)
  hand_out <- ems_solve(hand_cmf)

  for (v in c("othv", "cmpv", "pds", "qgdp")) {
    expect_equal(cond_out$dat[[which(cond_out$name == v)]], hand_out$dat[[which(hand_out$name == v)]])
  }
  othv <- cond_out$dat[[which(cond_out$name == "othv")]]
  cmpv <- cond_out$dat[[which(cond_out$name == "cmpv")]]
  expect_true(any(othv$Value != 0) && any(cmpv$Value != 0) && any(cmpv$Value == 0))
  cpx <- ems_compose(cond_cmf, "CPX")$dat[[1]]
  expect_equal(cpx, ems_compose(hand_cmf, "CPX")$dat[[1]])
  expect_true(any(cpx$Value != 0) && any(cpx$Value == 0))
  # the hand form subtracts its own term from the full sum: equal to
  # single-precision rounding, not bit for bit
  oth <- ems_compose(cond_cmf, "OTH")$dat[[1]]
  expect_equal(oth, ems_compose(hand_cmf, "OTH")$dat[[1]], tolerance = 1e-6)
})

test_that("IF formulas solve identically to their hand adaptations", {
  nest_temp("solve_if_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model)
  base <- ems_solve(cmf_base)

  nest_temp("solve_if", write_dir)
  if_file <- write_modified_model(
    static_model_file,
    NULL,
    .fn = function(m, t) {
      old_vcb <- paste0(
        "Formula (all,c,MARG)(all,r,REG)\r\n",
        "    VCB(c,r) = VDB(c,r) + sum{d,REG, VXSB(c,r,d)} + VST(c,r);\r\n",
        "Formula (all,c,NMRG)(all,r,REG)\r\n",
        "    VCB(c,r) = VDB(c,r) + sum{d,REG, VXSB(c,r,d)};"
      )
      new_vcb <- paste0(
        "Formula (all,c,COMM)(all,r,REG)\r\n",
        "    VCB(c,r) = VDB(c,r) + sum{d,REG, VXSB(c,r,d)} + IF[c in MARG, VST(c,r)];"
      )
      old_vxw <- paste0(
        "Formula (all,c,MARG)(all,r,REG)\r\n",
        "    VXW(c,r) = VXDFOB(c,r) + VST(c,r);\r\n",
        "Formula (all,c,NMRG)(all,r,REG)\r\n",
        "    VXW(c,r) = VXDFOB(c,r);"
      )
      new_vxw <- paste0(
        "Formula (all,c,COMM)(all,r,REG)\r\n",
        "    VXW(c,r) = VXDFOB(c,r) + IF[c in MARG, VST(c,r)];"
      )
      # the 7.1 fixture carries the IF forms: graft the hand splits over
      # them and compare
      stopifnot(
        grepl(new_vcb, m, fixed = TRUE),
        grepl(new_vxw, m, fixed = TRUE)
      )
      m <- sub(new_vcb, old_vcb, m, fixed = TRUE)
      sub(new_vxw, old_vxw, m, fixed = TRUE)
    }
  )
  if_model <- ems_model(if_file, static_closure_file, ignore_condense = TRUE)
  cmf_if <- ems_deploy(static_data, if_model)
  if_out <- ems_solve(cmf_if)
  expect_equal(if_out, base)
})

test_that("RANDOM draws are reproducible, seeded by the random_seed option and recorded", {
  withr::defer(ems_option_set(random_seed = 1L))
  rand_file <- write_modified_model(
    static_model_file,
    paste(
      "Coefficient (all,r,REG) RNDA(r) # random probe #;",
      "Formula (all,r,REG) RNDA(r) = RANDOM(2, 3);",
      "Variable (all,r,REG) zrnd(r) # random-coefficient probe #;",
      "Equation E_zrnd (all,r,REG) zrnd(r) = RANDOM(0.5, 1.5) * qgdp(r);",
      sep = "\n"
    )
  )
  rand_model <- suppressMessages(suppressWarnings(
    ems_model(rand_file, static_closure_file, ignore_condense = TRUE)
  ))
  val <- \(o, v) o$dat[[match(v, o$name)]]$Value
  run <- function(name, seed, method) {
    ems_option_set(random_seed = seed)
    nest_temp(name, write_dir)
    cmf <- ems_deploy(static_data, rand_model, shock = real_shock)
    out <- ems_solve(cmf, solution_method = method)
    list(out = out, record = readLines(file.path(dirname(cmf), "model_diagnostics.txt")))
  }
  j7 <- run("solve_random_j7", 7L, "Johansen")
  j7b <- run("solve_random_j7b", 7L, "Johansen")
  j8 <- run("solve_random_j8", 8L, "Johansen")
  g7 <- run("solve_random_g7", 7L, "Gragg")
  a <- val(j7$out, "RNDA")
  expect_true(all(a >= 2 & a < 3))
  expect_length(unique(a), length(a))
  expect_identical(val(j7b$out, "RNDA"), a)
  expect_false(identical(val(j8$out, "RNDA"), a))
  expect_true(any(grepl("Random seed: 7 (RANDOM draws in the model)", j7$record, fixed = TRUE)))
  # the equation's coefficient is one draw per region at every step and
  # extrapolation pass: Johansen gives it as zrnd/qgdp, and Gragg then
  # compounds qgdp by exactly that power
  u <- val(j7$out, "zrnd") / val(j7$out, "qgdp")
  expect_true(all(u >= 0.5 & u < 1.5))
  expect_equal(
    val(g7$out, "zrnd"),
    100 * ((1 + val(g7$out, "qgdp") / 100)^u - 1),
    tolerance = 1e-4
  )
})

test_that("IF equations solve identically to their hand adaptations", {
  nest_temp("solve_ifeq_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model)
  base <- ems_solve(cmf_base)

  nest_temp("solve_ifeq", write_dir)
  if_file <- write_modified_model(
    static_model_file,
    NULL,
    .fn = function(m, t) {
      # the 7.1 fixture carries the IF forms (E_qca, E_pca, E_pds): graft
      # the hand adaptations over them -- indicator coefficients for the
      # data conditions, a MARG/NMRG domain split for the in-set IF
      qca_span <- "qca\\(c,a,r\\) = IF\\[MAKES\\(c,a,r\\) gt 0,\\s*qo\\(a,r\\) - ETRAQ\\(a,r\\) \\* \\[ps\\(c,a,r\\) - po\\(a,r\\)\\]\\];"
      pca_span <- "pca\\(c,a,r\\) = IF\\[MAKEB\\(c,a,r\\) gt 0,\\s*pds\\(c,r\\) - ESUBQ\\(c,r\\) \\* \\[qca\\(c,a,r\\) - qc\\(c,r\\)\\]\\];"
      pds_span <- "(?s)Equation E_pds\\r?\\n.*?\\+ tradslack\\(c,r\\);"
      stopifnot(
        grepl(qca_span, m, perl = TRUE),
        grepl(pca_span, m, perl = TRUE),
        grepl(pds_span, m, perl = TRUE)
      )
      hand_qca <- paste0(
        "qca(c,a,r) = MAKESUNIT(c,a,r) * qo(a,r) - ",
        "MAKESUNIT(c,a,r) * ETRAQ(a,r) * [ps(c,a,r) - po(a,r)];"
      )
      hand_pca <- paste0(
        "pca(c,a,r) = MAKEBUNIT(c,a,r) * pds(c,r) - ",
        "MAKEBUNIT(c,a,r) * ESUBQ(c,r) * [qca(c,a,r) - qc(c,r)];"
      )
      unit_s <- paste(
        "Coefficient (all,c,COMM)(all,a,ACTS)(all,r,REG) MAKESUNIT(c,a,r) # make unit #;",
        "Formula (all,c,COMM)(all,a,ACTS)(all,r,REG) MAKESUNIT(c,a,r) = 0;",
        "Formula (all,c,COMM)(all,a,ACTS)(all,r,REG: MAKES(c,a,r) > 0) MAKESUNIT(c,a,r) = 1;",
        "Equation E_qca", sep = "\r\n"
      )
      unit_b <- paste(
        "Coefficient (all,c,COMM)(all,a,ACTS)(all,r,REG) MAKEBUNIT(c,a,r) # make unit #;",
        "Formula (all,c,COMM)(all,a,ACTS)(all,r,REG) MAKEBUNIT(c,a,r) = 0;",
        "Formula (all,c,COMM)(all,a,ACTS)(all,r,REG: MAKEB(c,a,r) > 0) MAKEBUNIT(c,a,r) = 1;",
        "Equation E_pca", sep = "\r\n"
      )
      hand_pds <- paste(
        "Equation E_pdsm",
        "# assures market clearing for margin commodities #",
        "(all,c,MARG)(all,r,REG)",
        "    qc(c,r) = DSSHR(c,r) * qds(c,r) + sum(d,REG, XSSHR(c,r,d) * qxs(c,r,d))",
        "            + STSHR(c,r) * qst(c,r)",
        "            + tradslack(c,r);",
        "Equation E_pdsnm",
        "# assures market clearing for commodities #",
        "(all,c,NMRG)(all,r,REG)",
        "    qc(c,r) = DSSHR(c,r) * qds(c,r) + sum(d,REG, XSSHR(c,r,d) * qxs(c,r,d))",
        "            + tradslack(c,r);", sep = "\r\n"
      )
      m <- sub(qca_span, hand_qca, m, perl = TRUE)
      m <- sub(pca_span, hand_pca, m, perl = TRUE)
      m <- sub("Equation E_qca", unit_s, m, fixed = TRUE)
      m <- sub("Equation E_pca", unit_b, m, fixed = TRUE)
      m <- sub(pds_span, hand_pds, m, perl = TRUE)
      # element-condition probe, value-checked below
      paste(
        m,
        "Coefficient (all,r,REG) IFELEM(r) # element condition probe #;",
        'Formula (all,r,REG) IFELEM(r) = 2 + IF[r="chn", 1];',
        sep = "\n"
      )
    }
  )
  if_model <- ems_model(if_file, static_closure_file, ignore_condense = TRUE)
  cmf_if <- ems_deploy(static_data, if_model)
  if_out <- ems_solve(cmf_if)

  # the hand indicators are ordinary coefficients and appear in the
  # composed output, as the rewrite's synthesized ones do in the base
  expect_true(all(c("MAKESUNIT", "MAKEBUNIT", "IFELEM") %in% setdiff(if_out$name, base$name)))
  common <- intersect(base$name, if_out$name)
  b2 <- base[match(common, base$name), ]
  i2 <- if_out[match(common, if_out$name), ]
  attr(b2, "row.names") <- attr(i2, "row.names") <- seq_along(common)
  expect_equal(i2, b2)

  probe <- if_out$dat[["IFELEM"]]
  expect_equal(probe$Value, ifelse(probe$REGr == "chn", 3, 2))
})

test_that("netcut proxy rewrite solves identically to a hand proxy (roadmap 6.5 E2)", {
  # hand-proxied reference: minimal intertemporal proxy written by the modeler
  nest_temp("solve_netcut_ref", write_dir)
  ref_file <- write_modified_model(
    dynamic_model_file,
    paste(
      "Variable (all,r,REG)(all,t,ALLTIME) ncref(r,t) # hand proxy #;",
      paste0(
        "Equation E_ncref # hand proxy link # (all,r,REG)(all,t,ALLTIME) ",
        'ncref(r,t) = qfe("capital","svces",r,t);'
      ),
      "Variable (all,r,REG)(all,t,FWDTIME) nctv(r,t) # probe #;",
      paste0(
        "Equation E_nctv # probe # (all,r,REG)(all,t,FWDTIME) ",
        "nctv(r,t) = ncref(r,t+1);"
      ),
      sep = "\n"
    )
  )
  ref_model <- ems_model(ref_file, dynamic_closure_file)
  cmf_ref <- ems_deploy(dynamic_data, ref_model)
  ref <- ems_solve(cmf_ref)

  # direct element-slice lead: the rewrite must synthesize the same proxy
  nest_temp("solve_netcut", write_dir)
  slice_file <- write_modified_model(
    dynamic_model_file,
    paste(
      "Variable (all,r,REG)(all,t,FWDTIME) nctv(r,t) # probe #;",
      paste0(
        "Equation E_nctv # probe # (all,r,REG)(all,t,FWDTIME) ",
        'nctv(r,t) = qfe("capital","svces",r,t+1);'
      ),
      sep = "\n"
    )
  )
  slice_model <- suppressMessages(ems_model(slice_file, dynamic_closure_file))
  expect_true(all(c("NCV1", "E_NCV1") %in% slice_model$name))
  cmf_slice <- ems_deploy(dynamic_data, slice_model)
  out <- ems_solve(cmf_slice)

  expect_setequal(setdiff(out$name, ref$name), "NCV1")
  expect_setequal(setdiff(ref$name, out$name), "ncref")
  common <- intersect(ref$name, out$name)
  r2 <- ref[match(common, ref$name), ]
  o2 <- out[match(common, out$name), ]
  attr(r2, "row.names") <- attr(o2, "row.names") <- seq_along(common)
  expect_equal(o2, r2)

  # the synthesized proxy carries the hand proxy's values
  proxy <- out$dat[[match("NCV1", out$name)]]
  hand <- ref$dat[[match("ncref", ref$name)]]
  expect_equal(proxy, hand)
})

test_that("condensed models solve equivalently and recover backsolved values (roadmap 6.2)", {
  nest_temp("solve_condense", write_dir)
  bs <- c(
    "qint", "qva", "pva", "pint", "qfa", "pca", "ps", "qfe", "afe",
    "pfd", "pfm"
  )
  # ps exercises the combined coefficient pivot (its defining equation
  # retains the variable on both sides after rearrangement)
  cond_model <- suppressWarnings(
    ems_model(static_model_file, static_closure_file,
      backsolve = bs, ignore_condense = TRUE
    )
  )

  plain_cmf <- ems_deploy(static_data, static_model, real_shock)
  plain <- ems_solve(plain_cmf, solution_method = "Gragg", matrix_method = "LU")
  cond_cmf <- ems_deploy(static_data, cond_model, real_shock)
  cond <- ems_solve(cond_cmf, solution_method = "Gragg", matrix_method = "LU")

  # backsolved variables are recovered by the solver and reported
  expect_all_true(bs %in% cond$name)

  # recovered values and surviving core variables match the uncondensed run
  core <- c(bs, "qo", "pds", "pms", "qxs")
  for (v in core) {
    a <- plain$dat[[match(v, plain$name)]]
    b <- cond$dat[[match(v, cond$name)]]
    expect_true(isTRUE(all.equal(a, b, tolerance = 1e-4)), label = v)
  }

  # exogenous-shock identity survives condensation
  aoall <- cond$dat[[match("aoall", cond$name)]]
  expect_true(max(abs(aoall$Value - 5)) < 1e-6)
})

test_that("a mutually referencing backsolve pair solves like the uncondensed model", {
  nest_temp("solve_condense_pair", write_dir)
  pair_file <- file.path(write_dir, "solve_condense_pair", "pair.tab")
  writeLines(c(
    readLines(static_model_file),
    "Variable (all,c,COMM)(all,r,REG) zx(c,r) # make supply #;",
    "Variable (all,c,COMM)(all,r,REG) zp(c,r) # make price #;",
    "Variable (all,c,COMM)(all,r,REG) zc(c,r) # make total #;",
    "Equation E_zx (all,c,COMM)(all,r,REG) zx(c,r) = qc(c,r) + 2*[zp(c,r) - pds(c,r)];",
    "Equation E_zp (all,c,COMM)(all,r,REG) zp(c,r) = pds(c,r) - 0.05*[zx(c,r) - zc(c,r)];",
    "Equation E_zc (all,c,COMM)(all,r,REG) zc(c,r) = zx(c,r);"
  ), pair_file)
  plain_model <- suppressMessages(suppressWarnings(
    ems_model(pair_file, static_closure_file, ignore_condense = TRUE)
  ))
  pair_model <- suppressMessages(suppressWarnings(
    ems_model(pair_file, static_closure_file,
      backsolve = c("zp", "zx"), ignore_condense = TRUE
    )
  ))
  plain <- ems_solve(ems_deploy(static_data, plain_model, real_shock),
    solution_method = "Johansen", matrix_method = "LU"
  )
  cond <- ems_solve(ems_deploy(static_data, pair_model, real_shock),
    solution_method = "Johansen", matrix_method = "LU"
  )

  qc <- plain$dat[[match("qc", plain$name)]]
  zc <- cond$dat[[match("zc", cond$name)]]
  expect_true(max(abs(qc$Value)) > 0.1)
  expect_equal(zc$Value, qc$Value, tolerance = 1e-5)
  for (v in c("zx", "zp", "zc", "qc", "pds")) {
    a <- plain$dat[[match(v, plain$name)]]
    b <- cond$dat[[match(v, cond$name)]]
    expect_true(isTRUE(all.equal(a, b, tolerance = 1e-5)), label = v)
  }
})

test_that("names match case-insensitively and keep their declared spelling", {
  nest_temp("solve_name_case", write_dir)
  case_dir <- file.path(write_dir, "solve_name_case")
  case_tab <- file.path(case_dir, "CASE.TAB")
  writeLines(c(
    readLines(static_model_file),
    "Coefficient (all,C,comm)(all,R,Reg) ZCOEF(C,R) # upper-case indices #;",
    "Formula (all,C,COMM)(all,R,REG) zcoef(C,R) = 2;",
    "Coefficient (all,r,REG) ZSave(r) # saving, read through another spelling #;",
    "Read ZSAVE from file gtapdata header \"SAVE\";",
    "Variable (all,c,comm)(all,r,reg) zExo(c,r) # exogenous #;",
    "Variable (all,c,COMM)(all,r,REG) zEndo(c,r) # endogenous #;",
    "Equation E_zendo (all,c,Comm)(all,r,REG) ZENDO(c,r) = ZCoef(c,r)*zexo(c,r);"
  ), case_tab)
  case_cls <- file.path(case_dir, "case.cls")
  cls <- readLines(static_closure_file)
  writeLines(c(cls[1], "ZEXO", cls[-1]), case_cls)

  case_model <- suppressMessages(suppressWarnings(
    ems_model(case_tab, case_cls, ignore_condense = TRUE)
  ))
  expect_true("zExo" %in% case_model$name)
  expect_true(any(grepl("zEndo(c,r) = ZCOEF(c,r)*zExo(c,r)", case_model$tab, fixed = TRUE)))

  cmf <- ems_deploy(static_data, case_model, ems_uniform_shock("ZEXO", 1))
  expect_identical(basename(cmf), "CASE.cmf")
  expect_true(any(grepl("zEndo", readLines(file.path(dirname(cmf), "CASE.TAB")), fixed = TRUE)))
  out <- ems_solve(cmf, solution_method = "Johansen", matrix_method = "LU")
  zendo <- out$dat[[match("zEndo", out$name)]]
  expect_equal(unique(round(zendo$Value, 6)), 2)
  zcoef <- ems_compose(cmf, "ZCOEF")$dat$ZCOEF
  expect_identical(colnames(zcoef), c("COMMC", "REGR", "Value"))
  expect_true(all(zcoef$Value == 2))
  zsave <- ems_compose(cmf, c("ZSave", "SAVE"))$dat
  expect_equal(zsave$ZSave$Value, zsave$SAVE$Value)
})

test_that("deploy metadata records the condensation state", {
  nest_temp("solve_condense_meta", write_dir)
  cond_model <- suppressWarnings(
    ems_model(static_model_file, static_closure_file,
      backsolve = c("qint", "qva"),
      ignore_condense = TRUE
    )
  )
  cmf_path <- ems_deploy(static_data, cond_model)
  condense <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))$condense
  expect_identical(condense$n_backsolve, 2L)
  expect_true(condense$n_backsolve_ele > 0)
  expect_true(condense$elimination_share > 0 && condense$elimination_share < 1)

  # an uncondensed deployment records zeros, so the advisory stays quiet
  plain_cmf <- ems_deploy(static_data, static_model)
  plain <- readRDS(file.path(dirname(plain_cmf), "metadata.rds"))$condense
  expect_identical(plain$n_backsolve, 0L)
  expect_identical(plain$elimination_share, 0)
})

test_that("condensed deployments are advised against bordered methods (roadmap 6.2)", {
  nest_temp("solve_condense_advice", write_dir)
  cond_model <- suppressWarnings(
    ems_model(static_model_file, static_closure_file,
      backsolve = c("qint", "qva"), ignore_condense = TRUE
    )
  )
  cmf_path <- ems_deploy(static_data, cond_model)
  expect_snapshot(
    ems_solve(cmf_path,
      matrix_method = "DBBD",
      n_tasks = 2L,
      terminal_run = TRUE
    ),
    transform = scrub_paths
  )

  # LU is the method condensation was measured to help: no advice
  lu_msg <- testthat::capture_messages(
    ems_solve(cmf_path, matrix_method = "LU", terminal_run = TRUE)
  )
  expect_false(any(grepl("bordered method", lu_msg)))
})

test_that("condensed intertemporal deployments are advised against (roadmap 6.2)", {
  nest_temp("solve_condense_inter", write_dir)
  cond_model <- suppressWarnings(
    ems_model(dynamic_model_file, dynamic_closure_file, backsolve = c("qint", "qva"))
  )
  cmf_path <- ems_deploy(dynamic_data, cond_model)
  expect_snapshot(
    ems_solve(cmf_path,
      solution_method = "Gragg",
      matrix_method = "SBBD",
      terminal_run = TRUE
    ),
    transform = scrub_paths
  )
})

test_that("ems_solve returns the same output across static matrix methods", {
  nest_temp("solve_static_method", write_dir)
  cmf_path <- ems_deploy(static_data, static_model, real_shock)
  LU <- ems_solve(
    cmf_path,
    solution_method = "Gragg",
    matrix_method = "LU",
    n_subintervals = 2
  )

  DBBD <- ems_solve(
    cmf_path,
    solution_method = "Gragg",
    matrix_method = "DBBD",
    n_subintervals = 2,
    n_tasks = 2
  )

  check <- all.equal(LU, DBBD, tolerance = 1e-4)
  expect_true(check)
})

test_that("ems_solve returns the same output across dynamic matrix methods", {
  nest_temp("solve_dynamic_method", write_dir)
  cmf_path <- ems_deploy(dynamic_data, dynamic_model, dynamic_shock)
  LU <- ems_solve(
    cmf_path,
    solution_method = "Gragg",
    matrix_method = "LU",
    n_subintervals = 2
  )

  SBBD <- ems_solve(
    cmf_path,
    solution_method = "Gragg",
    matrix_method = "SBBD",
    n_subintervals = 2,
    n_tasks = 2
  )

  NDBBD <- ems_solve(
    cmf_path,
    solution_method = "Gragg",
    matrix_method = "NDBBD",
    n_subintervals = 2,
    n_tasks = 2
  )
  
  LU_SBBD_check <- all.equal(LU, SBBD, tolerance = 1e-4)
  LU_NDBBD_check <- all.equal(LU, NDBBD, tolerance = 1e-4)
  check <- c(LU_SBBD_check, LU_NDBBD_check)
  expect_all_true(check)
})

test_that("Runge-Kutta methods solve consistently and expose accuracy metrics (roadmap 6.3c)", {
  nest_temp("solve_rk_e2e", write_dir)
  cmf_path <- ems_deploy(static_data, static_model, real_shock)
  gragg <- ems_solve(cmf_path, solution_method = "Gragg")
  rk4 <- ems_solve(cmf_path, solution_method = "RK4", steps = 8L)
  # compare with the solver's accuracy metric (absolute below 1,
  # relative above); the welfare/CNT/del_ aggregates are excluded —
  # differences of $-million components carry float32 cancellation
  # noise on which any cross-method comparison is loose (the
  # documented Johansen-vs-Gragg floor is the same order)
  # `u` is excluded as well: its asymptote is unconverged by every
  # method under a real shock (values of 1e7 and up on one region), so
  # it measures nothing about method agreement
  # the PostSim report tables (sums of the same $-million welfare
  # contributions) are excluded for the same reason
  # coefficients are stored in float32, so a small element that is the
  # difference of large ones (DPTAX -7.6 beside 7e5) carries a rounding
  # step of the header's largest value: coefficient differences are
  # scaled by that, variable differences element by element
  rk_metric <- function(a, b) {
    keep <- !grepl("^(ev|wev|cnt|del_)", a$name, ignore.case = TRUE) &
      a$name != "u" & a$type != "postsim"
    gaps <- mapply(function(g, r, type) {
      scale <- if (type == "coefficient") {
        max(1, abs(g$Value))
      } else {
        pmax(1, abs(g$Value))
      }
      max(abs(g$Value - r$Value) / scale)
    }, a$dat[keep], b$dat[keep], a$type[keep])
    max(gaps)
  }
  # 2e-3: under share weights on big3 with aoall +5, RK4 and DoPri54
  # agree with Gragg 2-4-8 to 1.6e-5 and 8.5e-6 on the variables, while
  # unscaled DPTAX/XTAXD rounding steps reach 4.1e-3; Johansen sits at
  # 2.37, so the bound separates a method defect from extrapolation noise
  expect_lt(rk_metric(gragg, rk4), 2e-3)

  # fixed-step explicit runs carry no accuracy metrics
  expect_false("error_estimate" %in% colnames(rk4$dat[["qgdp"]]))

  dopri <- ems_solve(cmf_path,
    solution_method = "DoPri54", steps = 4L,
    adaptive = "yes"
  )
  # embedded runs ride an error_estimate column alongside every Value
  expect_true("error_estimate" %in% colnames(dopri$dat[["qgdp"]]))
  expect_true(all(dopri$dat[["qgdp"]]$error_estimate >= 0))
  # the exogenous shock identity survives the RK integration
  expect_equal(unique(round(dopri$dat[["aoall"]]$Value, 6)), 5)
  expect_lt(rk_metric(gragg, dopri), 2e-3)
  # a non-embedded re-solve in the same directory must not inherit the
  # embedded run's estimate file
  gragg_again <- ems_solve(cmf_path, solution_method = "Gragg")
  expect_false("error_estimate" %in% colnames(gragg_again$dat[["qgdp"]]))
})

test_that("GTAPv6 in-TAB condensation solves equivalently to the full system", {
  nest_temp("solve_condense_gtapv6", write_dir)
  v6_inputs <- GTAP_convert(dat_input, par_input, set_input, target = "GTAPv6")
  v6_data <- ems_data(
    dat_input = v6_inputs$dat,
    par_input = v6_inputs$par,
    set_input = v6_inputs$set,
    REG = "big3",
    PROD_COMM = "macro_sector",
    ENDW_COMM = "labor_agg"
  )
  v6 <- ems_example("GTAPv6", write_dir)
  cond_model <- ems_model(v6[["model_file"]], v6[["closure_file"]])
  plain_model <- suppressMessages(
    ems_model(v6[["model_file"]], v6[["closure_file"]], ignore_condense = TRUE)
  )

  # double precision so that the comparison is not bounded by float32
  # roundoff in the coefficients (single precision agrees to ~1e-6)
  solve_v6 <- function(model) {
    cmf <- ems_deploy(v6_data, model, real_shock)
    ems_solve(cmf,
      solution_method = "Gragg", matrix_method = "LU",
      precision = "double"
    )
  }
  plain <- solve_v6(plain_model)
  cond <- solve_v6(cond_model)

  cond_flags <- cond_model[cond_model$type == "Variable", ]
  backsolved <- cond_flags$name[cond_flags$condense %in% "backsolve"]
  expect_all_true(backsolved %in% cond$name)

  # every surviving variable agrees to roundoff, backsolved ones included
  # (relative to the variable's scale, absolute for the identically-zero
  # slacks such as walraslack)
  common <- intersect(plain$name, cond$name)
  common <- common[common %in% plain_model$name[plain_model$type == "Variable"]]
  expect_gt(length(common), 150L)
  for (v in common) {
    a <- plain$dat[[match(v, plain$name)]]$Value
    b <- cond$dat[[match(v, cond$name)]]$Value
    expect_lt(max(abs(a - b)) / max(1, max(abs(a))), 1e-9, label = v)
  }
  aoall <- cond$dat[[match("aoall", cond$name)]]
  expect_true(max(abs(aoall$Value - 5)) < 1e-6)
})

test_that("GTAPv7 in-TAB condensation solves equivalently and the PostSim reports return", {
  nest_temp("solve_condense_gtapv7", write_dir)
  v7 <- ems_example("GTAPv7", write_dir)
  cond_model <- suppressWarnings(suppressMessages(
    ems_model(v7[["model_file"]], v7[["closure_file"]])
  ))
  plain_model <- suppressWarnings(suppressMessages(
    ems_model(v7[["model_file"]], v7[["closure_file"]], ignore_condense = TRUE)
  ))
  solve_v7 <- function(model) {
    cmf <- ems_deploy(static_data, model, real_shock)
    ems_solve(cmf,
      solution_method = "Gragg", matrix_method = "LU",
      precision = "double"
    )
  }
  plain <- solve_v7(plain_model)
  cond <- solve_v7(cond_model)

  common <- intersect(plain$name, cond$name)
  common <- common[common %in% plain_model$name[plain_model$type == "Variable"]]
  expect_gt(length(common), 200L)
  # 68 substitutions through synthesized pivots: agreement to 1e-7 of
  # each variable's scale (measured 1e-8 on the welfare contributions)
  for (v in common) {
    a <- plain$dat[[match(v, plain$name)]]$Value
    b <- cond$dat[[match(v, cond$name)]]$Value
    expect_lt(max(abs(a - b)) / max(1, max(abs(a))), 1e-7, label = v)
  }
  # upstream welfare-report coefficients computed in PostSim
  expect_true(all(c("WELFARE", "CNTalleff", "ATAX", "TRADE", "WTOT") %in% cond$name))
  welfare <- cond$dat[[match("WELFARE", cond$name)]]
  expect_true(all(c("alloc_a1", "tot_e1") %in% tolower(welfare[[2]])))
})

test_that("ems_solve examples work", {
  nest_temp("solve_examples", write_dir)
  cmf_path <- ems_deploy(dynamic_data,
                         dynamic_model)
  # The following examples require the teems solver to be built.
  # See https://teemsphere.github.io/ to get started.

  # Solving a static model with Johansen:
  expect_s3_class(ems_solve(cmf_path), "tbl_df")

  # Solving a dynamic model with the SBBD method:
  expect_s3_class(ems_solve(cmf_path,
            solution_method = "Gragg",
            matrix_method = "SBBD",
            n_tasks = 6), "tbl_df")
})

test_that("a run with -jacdump leaves a Jacobian that its solution satisfies (e2e)", {
  nest_temp("solve_jacdump", write_dir)
  cmf_path <- ems_deploy(static_data, static_model, ems_uniform_shock("aoall", 5))
  ems_solve(cmf_path, solution_method = "Johansen", suppress_outputs = TRUE, jacdump = TRUE)
  sol <- file.path(dirname(cmf_path), "out", "variables", "bin", "sol.")
  jac <- .parse_jacobian(sol)
  expect_false(is.null(jac))
  x <- readBin(paste0(sol, "bin"), "double", n = jac$ncol, size = 8L, endian = "little")
  cx <- tapply(jac$value * x[jac$col + 1L], jac$row, sum)
  mag <- tapply(abs(jac$value * x[jac$col + 1L]), jac$row, sum)
  expect_length(cx, jac$nrow)
  expect_lt(max(abs(cx) / pmax(mag, .Machine$double.xmin)), 1e-9)
  expect_identical(sum(jac$equations$nrows), jac$nrow)
})

unlink(write_dir, recursive = TRUE)