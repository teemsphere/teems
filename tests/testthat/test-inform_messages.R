# informational messages gated on the verbose option; the other test
# files run with verbose = FALSE, so these are exercised here directly

test_that("loaded data reports its version, reference year and format", {
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_reset())
  expect_snapshot(.inform_metadata(list(
    full_database_version = "GTAPv11c",
    reference_year = 2017,
    data_format = "GTAPv7"
  )))
  ems_option_set(verbose = FALSE)
  expect_no_message(.inform_metadata(list(
    full_database_version = "GTAPv11c",
    reference_year = 2017,
    data_format = "GTAPv7"
  )))
})

test_that("EFVE overrides are reported", {
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_reset())
  overrides <- data.table::data.table(
    ACTS = c("gas", "peakload", "peakload"),
    REG = c("chn", "chn", "usa"),
    rule = c("supply", "placeholder", "placeholder"),
    from = c(0.0105378, 1e-6, 1e-6),
    to = c(1.182738, 1, 1)
  )
  expect_snapshot(.inform_efve(metadata = list(efve_overrides = overrides), call = NULL))
  expect_no_message(.inform_efve(metadata = list(efve_overrides = overrides[0]), call = NULL))
  expect_no_message(.inform_efve(metadata = list(), call = NULL))
  ems_option_set(verbose = FALSE)
  expect_no_message(.inform_efve(metadata = list(efve_overrides = overrides), call = NULL))
})

local_accuracy_stats <- function(run_dir, linear, levels, judged_zero, subintervals = 1L) {
  bin_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(bin_dir, recursive = TRUE)
  stats <- list(
    version = 2L,
    accuracy = list(
      population = "endogenous+backsolved",
      summarised = sum(levels),
      subintervals = subintervals,
      linear = list(figures = linear, judged_zero = judged_zero),
      levels = list(figures = levels)
    )
  )
  jsonlite::write_json(stats, file.path(bin_dir, "sol.stats.json"), auto_unbox = TRUE)
  return(invisible(run_dir))
}

test_that("a finished run reports its elapsed time and accuracy", {
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_reset())
  run_dir <- withr::local_tempdir()
  local_accuracy_stats(
    run_dir,
    linear = c(0, 0, 0, 2, 3, 5, 90),
    levels = c(0, 0, 0, 2, 0, 3, 95),
    judged_zero = 0
  )
  expect_snapshot(.inform_diagnostics(
    elapsed_time = c(0, 0, 75.4),
    model_log = "solver log",
    run_dir = run_dir,
    call = NULL
  ))
  record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("^Elapsed time: +1m 15s$", record)))
  expect_true(any(grepl("^Accuracy \\(levels, 4 figures\\): 98%$", record)))
  expect_true(any(grepl("^ +3 +2 +2$", record)))
})

test_that("the accuracy share is judged on levels figures, judged-zero results counting at 6", {
  ems_option_set(verbose = FALSE)
  withr::defer(ems_option_reset())
  run_dir <- withr::local_tempdir()
  local_accuracy_stats(run_dir, linear = integer(7), levels = c(0, 0, 0, 0, 0, 0, 4478), judged_zero = 4478)
  expect_no_warning(.inform_diagnostics(
    elapsed_time = c(0, 0, 1),
    model_log = "solver log",
    run_dir = run_dir,
    call = NULL
  ))
  record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("^Accuracy \\(levels, 4 figures\\): 100%$", record)))
  expect_true(any(grepl("^Judged zero: +4478$", record)))
  run_dir <- withr::local_tempdir()
  local_accuracy_stats(
    run_dir,
    linear = c(30, 30, 0, 0, 10, 0, 30),
    levels = c(10, 20, 0, 0, 30, 0, 40),
    judged_zero = 0
  )
  expect_warning(.inform_diagnostics(
    elapsed_time = c(0, 0, 1),
    model_log = "solver log",
    run_dir = run_dir,
    call = NULL
  ), "70%")
})

test_that("a run without an accuracy summary reports nothing", {
  run_dir <- withr::local_tempdir()
  expect_no_message(.inform_diagnostics(
    elapsed_time = c(0, 0, 1),
    model_log = "solver log",
    run_dir = run_dir,
    call = NULL
  ))
  record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_false(any(grepl("^-- Run summary --$", record)))
})
