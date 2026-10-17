test_that("a previous run's solution files and probe record are cleared", {
  run_dir <- withr::local_tempdir()
  bin_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(bin_dir, recursive = TRUE)
  stale <- file.path(bin_dir, paste0("sol", c(
    ".bin", ".est", ".acc", ".var", ".sel", ".set", ".mds",
    ".cof", ".cbin", ".stats.json", ".cbin0", ".xac", ".cols",
    ".cols.json", ".jac", ".jac.json", ".outputs.json", ".outputs.json.tmp",
    ".probe.json", ".probe.pattern"
  )))
  kept <- file.path(bin_dir, "notes.txt")
  for (f in c(stale, kept)) {
    writeLines("stale", f)
  }
  dir.create(file.path(run_dir, "out", "sets"))
  dir.create(file.path(run_dir, "out", "coefficients"))
  stale_csv <- file.path(run_dir, "out", c("sets", "coefficients"), c("REG.csv", "VDFB.csv"))
  kept_txt <- file.path(run_dir, "out", "coefficients", "readme.txt")
  for (f in c(stale_csv, kept_txt)) {
    writeLines("stale", f)
  }
  expect_no_error(.clear_solution_files(run_dir = run_dir))
  expect_false(any(file.exists(stale)))
  expect_false(any(file.exists(stale_csv)))
  expect_true(file.exists(kept))
  expect_true(file.exists(kept_txt))
})

test_that("clearing an empty or absent solution directory is a no-op", {
  run_dir <- withr::local_tempdir()
  expect_no_error(.clear_solution_files(run_dir = run_dir))
  expect_no_error(.clear_solution_files(run_dir = file.path(run_dir, "missing")))
})

test_that("the in-container clearing command removes the same files as the host clearing", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("bash")), "bash not available")
  run_dir <- withr::local_tempdir()
  bin_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(bin_dir, recursive = TRUE)
  stale <- file.path(bin_dir, paste0("sol", .solution_file_suffixes()))
  kept <- file.path(bin_dir, c("notes.txt", "sol.ud5"))
  dir.create(file.path(run_dir, "out", "sets"))
  dir.create(file.path(run_dir, "out", "coefficients"))
  stale_csv <- file.path(run_dir, "out", c("sets", "coefficients"), c("REG.csv", "VDFB.csv"))
  kept_txt <- file.path(run_dir, "out", "coefficients", "readme.txt")
  for (f in c(stale, kept, stale_csv, kept_txt)) {
    writeLines("stale", f)
  }
  cmd <- .clear_solution_files_cmd(docker_run = run_dir)
  expect_identical(system2("bash", c("-c", shQuote(cmd))), 0L)
  expect_false(any(file.exists(stale)))
  expect_false(any(file.exists(stale_csv)))
  expect_true(all(file.exists(kept)))
  expect_true(file.exists(kept_txt))
  unlink(file.path(run_dir, "out"), recursive = TRUE)
  expect_identical(system2("bash", c("-c", shQuote(cmd))), 0L)
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
