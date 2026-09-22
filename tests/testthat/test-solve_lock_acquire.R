test_that("a lock without an owner record is attributed to an earlier run", {
  run_dir <- withr::local_tempdir()
  dir.create(file.path(run_dir, ".solve_lock"))
  expect_snapshot(
    .solve_lock_acquire(run_dir = run_dir, timeID = "010203_1", call = NULL),
    error = TRUE,
    transform = \(lines) gsub(run_dir, "<run_dir>", lines, fixed = TRUE)
  )
})

test_that("a free directory is claimed and the claim names its owner", {
  run_dir <- withr::local_tempdir()
  lock_path <- .solve_lock_acquire(run_dir = run_dir, timeID = "010203_1", call = NULL)
  expect_true(dir.exists(lock_path))
  expect_match(readLines(file.path(lock_path, "owner")), "^run 010203_1 \\(pid [0-9]+\\) started ")
})
