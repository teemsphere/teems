test_that("a previous run's solution files are cleared and the probe record survives", {
  run_dir <- withr::local_tempdir()
  bin_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(bin_dir, recursive = TRUE)
  stale <- file.path(bin_dir, paste0("sol", c(
    ".bin", ".est", ".acc", ".var", ".sel", ".set", ".mds",
    ".cof", ".cbin", ".stats.json"
  )))
  kept <- file.path(bin_dir, "sol.probe.json")
  for (f in c(stale, kept)) {
    writeLines("stale", f)
  }
  expect_no_error(.clear_solution_files(run_dir = run_dir))
  expect_false(any(file.exists(stale)))
  expect_true(file.exists(kept))
})

test_that("clearing an empty or absent solution directory is a no-op", {
  run_dir <- withr::local_tempdir()
  expect_no_error(.clear_solution_files(run_dir = run_dir))
  expect_no_error(.clear_solution_files(run_dir = file.path(run_dir, "missing")))
})
