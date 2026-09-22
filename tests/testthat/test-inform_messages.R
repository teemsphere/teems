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

test_that("a finished run reports its elapsed time and accuracy", {
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_reset())
  run_dir <- withr::local_tempdir()
  model_log <- c(
    "Accurate at 6 digits        90",
    "Accurate at 5 digits        5",
    "Accurate at 4 digits        3",
    "Accurate at 3 digits        2",
    "Accurate at 2 digits        0",
    "Accurate at 1 digit or none 0"
  )
  expect_snapshot(.inform_diagnostics(
    elapsed_time = c(0, 0, 75.4),
    model_log = model_log,
    run_dir = run_dir,
    call = NULL
  ))
  record <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("^Elapsed time: +1m 15s$", record)))
  expect_true(any(grepl("^Accuracy \\(4-digit\\): 98%$", record)))
})
