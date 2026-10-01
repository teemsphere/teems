test_that("a previous run's solution files are cleared and the probe record survives", {
  run_dir <- withr::local_tempdir()
  bin_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(bin_dir, recursive = TRUE)
  stale <- file.path(bin_dir, paste0("sol", c(
    ".bin", ".est", ".acc", ".var", ".sel", ".set", ".mds",
    ".cof", ".cbin", ".stats.json", ".cbin0", ".xac", ".cols",
    ".cols.json", ".jac", ".jac.json", ".outputs.json", ".outputs.json.tmp"
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

test_that("a side-car is trusted only when the run's completion record lists it", {
  bin_dir <- withr::local_tempdir()
  sol_prefix <- file.path(bin_dir, "sol.")
  writeLines("x", paste0(sol_prefix, "cbin0"))
  writeLines("x", paste0(sol_prefix, "xac"))
  expect_false(.solver_output_listed(sol_prefix, "cbin0"))
  jsonlite::write_json(
    list(
      version = 1, complete = TRUE, run_id = "r",
      files = data.frame(name = "sol.cbin0", path = "/opt/teems/out/variables/bin/sol.cbin0")
    ),
    paste0(sol_prefix, "outputs.json"),
    auto_unbox = TRUE
  )
  expect_true(.solver_output_listed(sol_prefix, "cbin0"))
  expect_false(.solver_output_listed(sol_prefix, "xac"))
  unlink(paste0(sol_prefix, "cbin0"))
  expect_false(.solver_output_listed(sol_prefix, "cbin0"))
  writeLines("{ not json", paste0(sol_prefix, "outputs.json"))
  writeLines("x", paste0(sol_prefix, "cbin0"))
  expect_false(.solver_output_listed(sol_prefix, "cbin0"))
})
