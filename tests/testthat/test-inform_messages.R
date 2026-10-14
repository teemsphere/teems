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

local_accuracy_stats <- function(run_dir, linear, levels, judged_zero, subintervals = 1L, data = NULL) {
  bin_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(bin_dir, recursive = TRUE)
  stats <- list(
    version = 2L,
    accuracy = list(
      covers = "endogenous+backsolved",
      required_figures = 3L,
      codes = list(
        XMA = list(results = 90L, lowest_figures = 6L),
        FC0 = list(results = 2L, lowest_figures = NA),
        "MD!" = list(results = 1L, lowest_figures = 0L)
      ),
      summarised = sum(levels),
      subintervals = subintervals,
      linear = list(figures = linear, judged_zero = judged_zero),
      levels = list(figures = levels),
      data = data
    )
  )
  jsonlite::write_json(stats, file.path(bin_dir, "sol.stats.json"), auto_unbox = TRUE, na = "null")
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
  expect_true(any(grepl("^Accuracy \\(solution, 4 figures\\): 98%$", record)))
  expect_true(any(grepl("^ +3 +2 +2$", record)))
  expect_true(any(grepl("^Required figures \\(XRA\\): 3$", record)))
  expect_true(any(grepl("^XMA +90 +6$", record)))
  expect_true(any(grepl("^FC0 +2$", record)))
  expect_true(any(grepl("^MD! +1 +0$", record)))
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
  expect_true(any(grepl("^Accuracy \\(solution, 4 figures\\): 100%$", record)))
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

test_that("the accuracy criterion picks the summary judged, and a run without updated data falls back to the solution", {
  withr::defer(ems_option_reset())
  ems_option_set(verbose = FALSE)
  data_file <- function(name, figures, zero) {
    return(list(name = name, values = sum(figures) + zero, figures = figures, judged_zero = zero))
  }
  files <- list(
    data_file("gtapdata", c(0, 0, 20, 20, 0, 0, 40), 20),
    data_file("extra", c(0, 0, 0, 0, 0, 0, 20), 0)
  )
  data <- list(
    subintervals = 1L,
    files = files,
    all = list(values = 120, figures = c(0, 0, 20, 20, 0, 0, 60), judged_zero = 20)
  )
  stats <- function(data) {
    run_dir <- withr::local_tempdir(.local_envir = parent.frame())
    local_accuracy_stats(run_dir,
      linear = c(0, 0, 0, 0, 0, 10, 90),
      levels = c(0, 0, 0, 0, 0, 10, 90),
      judged_zero = 0,
      data = data
    )
    return(run_dir)
  }
  judge <- function(run_dir) {
    .inform_diagnostics(elapsed_time = c(0, 0, 1), model_log = "solver log", run_dir = run_dir, call = NULL)
    record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
    return(grep("^Accuracy \\(", record, value = TRUE))
  }

  run_dir <- stats(data)
  expect_no_warning(line <- judge(run_dir))
  expect_identical(line, "Accuracy (solution, 4 figures): 100%")
  record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("^Updated data values: 120", record)))
  expect_true(any(grepl("gtapdata +extra +all files", record)))
  expect_true(any(grepl("^Data judged zero: +20  0  20$", record)))

  ems_option_set(accuracy_criterion = "data")
  expect_warning(line <- judge(stats(data)), "of updated data values accurate to at least 4")
  expect_identical(line, "Accuracy (data, 4 figures): 67%")

  ems_option_set(accuracy_criterion = "both")
  expect_no_warning(line <- judge(stats(data)))
  expect_identical(line, "Accuracy (both, 4 figures): 82%")

  ems_option_set(accuracy_criterion = "data", accuracy_figures = 3)
  expect_no_warning(line <- judge(stats(data)))
  expect_identical(line, "Accuracy (data, 3 figures): 83%")

  ems_option_set(accuracy_figures = 6, accuracy_threshold = 0.95)
  run_dir <- stats(NULL)
  expect_warning(line <- judge(run_dir), "of endogenous results \\(their levels values\\) accurate to at least 6")
  expect_identical(line, "Accuracy (solution, 6 figures): 90%")
  record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("^Accuracy criterion: data \\(no updated data; judged on the solution\\)$", record)))
})

test_that("a summary of selected variables says so in the warning and the run summary", {
  withr::defer(ems_option_reset())
  ems_option_set(verbose = FALSE)
  run_dir <- withr::local_tempdir()
  bin_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(bin_dir, recursive = TRUE)
  stats <- list(accuracy = list(
    covers = "xac-retained",
    variables = c("qgdp", "aoall"),
    summarised = 18L,
    subintervals = 1L,
    linear = list(figures = c(0, 0, 10, 0, 0, 0, 8), judged_zero = 0),
    levels = list(figures = c(0, 0, 10, 0, 0, 0, 8))
  ))
  jsonlite::write_json(stats, file.path(bin_dir, "sol.stats.json"), auto_unbox = TRUE)
  expect_warning(
    .inform_diagnostics(elapsed_time = c(0, 0, 1), model_log = "solver log", run_dir = run_dir, call = NULL),
    "of results of the selected variables \\(their levels values\\) accurate"
  )
  record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("^Results summarised: 18 \\(variables qgdp, aoall\\)$", record)))
})
