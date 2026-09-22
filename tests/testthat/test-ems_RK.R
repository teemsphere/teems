skip_on_cran()

# ems_RK(): Runge-Kutta front end delegating to ems_solve(); the RK
# step controls (adaptive, eps_tolerance, max_retries, retry_adjust)
# left ems_solve()'s formals and ride its dots — one validation and
# one record path.

# a minimal runnable cmf: the path check and the tab read both pass,
# so validation (and, with terminal_run, command construction) is
# exercised without a solver
mk_cmf <- function(dir) {
  tab <- file.path(dir, "m.tab")
  writeLines("x", tab)
  cmf <- file.path(dir, "m.cmf")
  writeLines(sprintf("tabfile \"%s\";", tab), cmf)
  cmf
}

exec_cmd <- function(dir) {
  readLines(file.path(dir, "model_exec.txt"), warn = FALSE)
}

test_that("RK controls pass through the ems_solve dots", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  out <- suppressMessages(ems_solve(
    cmf,
    solution_method = "DoPri54",
    steps = 4L,
    adaptive = "yes",
    eps_tolerance = 0.05,
    max_retries = 5,
    terminal_run = TRUE
  ))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-solmed DoPri54", fixed = TRUE)
  expect_match(cmd, "-adaptive yes -epstol 0.05", fixed = TRUE)
  expect_match(cmd, "-maxretries 5", fixed = TRUE)
})

test_that("unknown dot arguments abort", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  expect_snapshot_error(ems_solve(cmf, bogus_argument = 1))
  expect_error(
    ems_RK(cmf, bogus_argument = 1),
    "bogus_argument",
    class = "rlang_error"
  )
})

test_that("dot-passed RK controls are still validated", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  expect_error(
    ems_solve(cmf, adaptive = "maybe"),
    "adaptive",
    class = "rlang_error"
  )
  expect_error(
    ems_solve(cmf, solution_method = "RK2", steps = 4L, adaptive = "yes"),
    "adaptive",
    class = "rlang_error"
  )
  expect_error(
    ems_solve(cmf, solution_method = "RK4", steps = 4L, eps_tolerance = -1),
    "eps_tolerance",
    class = "rlang_error"
  )
})

test_that("ems_RK defaults: adaptive DoPri54; fixed-step for RK4/RK2", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  suppressMessages(ems_RK(cmf, terminal_run = TRUE))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-solmed DoPri54", fixed = TRUE)
  expect_match(cmd, "-adaptive yes", fixed = TRUE)

  suppressMessages(ems_RK(cmf, solution_method = "RK4", terminal_run = TRUE))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-solmed RK4", fixed = TRUE)
  expect_no_match(cmd, "-adaptive", fixed = TRUE)
})

test_that("ems_RK forwards ems_solve arguments and validation", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  suppressMessages(ems_RK(cmf, precision = "double", terminal_run = TRUE))
  expect_match(exec_cmd(d), "teems-solver-f64", fixed = TRUE)
  expect_error(
    ems_RK(cmf, solution_method = "Gragg"),
    "solution_method",
    class = "rlang_error"
  )
  expect_error(
    ems_RK(cmf, solution_method = "RK2", adaptive = "yes"),
    "adaptive",
    class = "rlang_error"
  )
  expect_error(
    ems_RK(cmf, n_subintervals = 2L),
    "n_subintervals",
    class = "rlang_error"
  )
})

test_that("ems_RK forwards the chart, norm, controller, initial step and guard", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  suppressMessages(ems_RK(cmf, terminal_run = TRUE))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-rkchart log", fixed = TRUE)
  expect_match(cmd, "-rknorm max", fixed = TRUE)
  expect_match(cmd, "-rkctrl std", fixed = TRUE)
  expect_match(cmd, "-rkscope pct", fixed = TRUE)
  expect_no_match(cmd, "-rk_h0", fixed = TRUE)
  expect_no_match(cmd, "-rkguard", fixed = TRUE)
  suppressMessages(ems_RK(cmf,
    terminal_run = TRUE, chart = "percent", error_norm = "rms",
    controller = "pi", scope = "all", h_init = 0.25, guard = 1e6
  ))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-rkchart percent", fixed = TRUE)
  expect_match(cmd, "-rknorm rms", fixed = TRUE)
  expect_match(cmd, "-rkctrl pi", fixed = TRUE)
  expect_match(cmd, "-rkscope all", fixed = TRUE)
  expect_match(cmd, "-rk_h0 0.25", fixed = TRUE)
  expect_match(cmd, "-rkguard 1e+06", fixed = TRUE)
  expect_error(ems_RK(cmf, chart = "bogus"))
  expect_error(ems_solve(cmf, solution_method = "DoPri54", steps = 4L, rk_h0 = 2))
  expect_error(ems_solve(cmf, solution_method = "DoPri54", steps = 4L, rk_guard = 0.5))
  expect_error(ems_solve(cmf, solution_method = "DoPri54", steps = 4L, rk_norm = "l1"))
})

test_that("Heun is accepted as a fixed-step Runge-Kutta flavor", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  suppressMessages(ems_RK(cmf, solution_method = "Heun", steps = 8L, terminal_run = TRUE))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-solmed Heun", fixed = TRUE)
  expect_match(cmd, "-step1 8", fixed = TRUE)
  expect_no_match(cmd, "-adaptive", fixed = TRUE)
  expect_error(ems_solve(cmf, solution_method = "Heun", steps = 4L, adaptive = "yes"))
})

test_that("ems_RK returns invisibly when there are no outputs to return", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  res <- suppressMessages(withVisible(ems_RK(cmf, terminal_run = TRUE)))
  expect_null(res$value)
  expect_false(res$visible)
})
