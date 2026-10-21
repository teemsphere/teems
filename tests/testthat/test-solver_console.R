# the solver log is one stream: tee writes every line to
# out/solver_out_<id>.txt (later appended to model_diagnostics.txt)
# while R relays a verbosity-filtered subset to the console

acc_block <- function(header) {
  c(
    header,
    "  figures  results  levels values",
    "        6     2796           3104",
    "        5      528            269",
    "        0        0              0",
    "  judged zero 20 (codes C0 and FC0: both extrapolations below 1e-6; at 6 figures among the levels values)",
    "  code  meaning                                          results  lowest figures",
    "  XMA   three extrapolations equal to machine accuracy      1657               6",
    "  CX    confidence in the extrapolated result               1632               2",
    "  observed order of convergence (expected 2): 803 within 0.5 of it, 397 lower, 371 higher, 246 with oscillating passes, 1677 not measured (agreeing to working precision, near zero or a zero pass difference)"
  )
}

zdiv <- function(stmt) {
  paste0(
    "Warning: 3 division(s) of a nonzero value by zero or power(s) of zero to a negative exponent in Formula ",
    stmt, " took the Zerodivide default 0 (GEMPACK manual 10.11, 34.3): ", stmt, " = A / B;"
  )
}

solver_log <- c(
  "Variable calculation time 0.21 s",
  zdiv("VXWD"),
  "",
  "Prepare time 0.120000",
  "LU time 0.300000",
  "memory: after step, resident 0.08 GB max per rank, 0.08 GB over 1 rank(s); high-water 0.08 GB max, 0.08 GB sum",
  "Update time 0.000014",
  "Last Update time 0.000003",
  zdiv("VIWS"),
  "memory: after solve, resident 0.50 GB max per rank, 0.50 GB over 1 rank(s); high-water 0.60 GB max, 0.60 GB sum",
  acc_block("Accuracy summary for subinterval 1 of 2 (GEMPACK manual 26.3): 3494 endogenous and backsolved results, each judged on its movement across the subinterval"),
  acc_block("Accuracy summary for subinterval 2 of 2 (GEMPACK manual 26.3): 3494 endogenous and backsolved results, each judged on its movement across the subinterval"),
  acc_block("Overall accuracy summary over 2 subintervals (GEMPACK manual 26.3.1): 3494 endogenous and backsolved results, each at its lowest figures and worst code across the subintervals"),
  "Updated data accuracy summary over 2 subintervals (GEMPACK manual 26.2.2): 1050 values Read from data files and updated, by the significant figures to which the two two-pass extrapolations agree, the lowest across the subintervals",
  "  figures      gtapdata",
  "        6          1047",
  "        0             0",
  "  judged zero         0 (both extrapolations below 1e-6)",
  "Gragg solve time 1.20 s",
  "Maximum residual ratio across the whole simulation is 6.22280560e-13 (30 solves checked; GEMPACK manual 30.1.5)",
  "postsim: running 126 statement(s)"
)

test_that("verbosity 1 keeps phase progress and the overall figures; timings, memory, residual, postsim and Zerodivide lines stay in the log", {
  state <- .solver_console_state()
  shown <- .solver_console_lines(solver_log, state, 1L)
  expect_identical(shown, c(
    "Variable calculation time 0.21 s",
    "",
    "Overall accuracy summary over 2 subintervals (GEMPACK manual 26.3.1): 3494 endogenous and backsolved results, each at its lowest figures and worst code across the subintervals",
    "  figures  results  levels values",
    "        6     2796           3104",
    "        5      528            269",
    "        0        0              0",
    "Gragg solve time 1.20 s"
  ))
})

test_that("a blank line after a dropped line is not carried to the console", {
  state <- .solver_console_state()
  shown <- .solver_console_lines(c("Statement order: 1 Read(s) replayed at their file position, 2 formula/assertion segment(s) (TAB; GEMPACK manual 10.1, 11.11.8)", "Backsolving 68 variables", zdiv("VXWD"), "", "System size 998 equations", "inmemory: keeping ~0 MB of value arrays resident per rank"), state, 1L)
  expect_identical(shown, c("Backsolving 68 variables", "System size 998 equations"))
})

test_that("the log copy gets a blank line before each accuracy block and none after a Zerodivide warning", {
  formatted <- .solver_log_format(solver_log)
  headers <- grep("^(Accuracy summary|Overall accuracy summary|Updated data accuracy summary)", formatted)
  expect_length(headers, 4L)
  expect_true(all(formatted[headers - 1L] == ""))
  zd <- grep("Zerodivide default", formatted)
  expect_false(any(formatted[zd + 1L] == ""))
  expect_identical(formatted[formatted != ""], solver_log[solver_log != ""])
  expect_identical(.solver_log_format(.solver_log_format(solver_log)), formatted)
})

test_that("verbosity 0 keeps errors, warnings and the figures table", {
  state <- .solver_console_state()
  shown <- .solver_console_lines(c(
    "Error: cannot open data.har",
    "condest: WARNING: the linear system is numerically near-singular",
    "Accuracy estimates: not available from two solutions",
    solver_log
  ), state, 0L)
  expect_identical(shown, c(
    "Error: cannot open data.har",
    "condest: WARNING: the linear system is numerically near-singular",
    "",
    "Accuracy estimates: not available from two solutions",
    "",
    "Overall accuracy summary over 2 subintervals (GEMPACK manual 26.3.1): 3494 endogenous and backsolved results, each at its lowest figures and worst code across the subintervals",
    "  figures  results  levels values",
    "        6     2796           3104",
    "        5      528            269",
    "        0        0              0"
  ))
})

test_that("verbosity 2 relays every line", {
  state <- .solver_console_state()
  expect_identical(.solver_console_lines(solver_log, state, 2L), solver_log)
})

test_that("a single-subinterval summary keeps its figures and drops its codes", {
  state <- .solver_console_state()
  shown <- .solver_console_lines(
    acc_block("Accuracy summary (GEMPACK manual 26.2.1): 3494 endogenous and backsolved results"),
    state, 1L
  )
  expect_length(shown, 5L)
  expect_match(shown[1], "^Accuracy summary \\(")
  expect_match(shown[5], "^        0 ")
  expect_false(any(grepl("judged zero|code  meaning|observed order", shown)))
})

test_that("the filter holds state across chunks of one line", {
  state <- .solver_console_state()
  shown <- unlist(lapply(solver_log, .solver_console_lines, state = state, verbosity = 1L))
  state2 <- .solver_console_state()
  expect_identical(shown, .solver_console_lines(solver_log, state2, 1L))
})

test_that("the relay returns the exit status and the filtered console", {
  skip_on_os("windows")
  withr::local_options(list())
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_set(verbose = FALSE))
  script <- file.path(withr::local_tempdir(), "log.txt")
  writeLines(solver_log, script)
  cmd <- paste("cat", shQuote(script), "; exit 3")
  out <- capture.output(status <- .relay_solver_cmd(cmd, verbosity = 1L, timeID = "000000_0"))
  expect_identical(status, 3L)
  expect_true(any(grepl("^Overall accuracy summary", out)))
  expect_false(any(grepl("code  meaning", out)))
  expect_false(any(grepl("Zerodivide default", out)))
  expect_false(any(grepl("^(Prepare|LU|Update) time", out)))

  ems_option_set(verbose = FALSE)
  out <- capture.output(status <- .relay_solver_cmd(cmd, verbosity = 1L, timeID = "000000_0"))
  expect_identical(status, 3L)
  expect_identical(out, character(0))
})
