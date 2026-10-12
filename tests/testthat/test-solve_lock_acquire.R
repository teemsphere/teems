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

test_that("a lock whose owner has ended is taken over", {
  run_dir <- withr::local_tempdir()
  dir.create(file.path(run_dir, ".solve_lock"))
  writeLines("run 010203_999 (pid 999) started 2026-01-01 01:02:03 UTC", file.path(run_dir, ".solve_lock", "owner"))
  removed <- character()
  local_mocked_bindings(
    .pid_alive = function(pid, ...) FALSE,
    .container_remove = function(timeID) removed <<- c(removed, timeID)
  )
  expect_message(
    lock_path <- .solve_lock_acquire(run_dir = run_dir, timeID = "040506_2", call = NULL),
    "Taking over the lock of run 010203_999"
  )
  expect_identical(removed, "010203_999")
  expect_match(readLines(file.path(lock_path, "owner")), "^run 040506_2 \\(pid [0-9]+\\) started ")
})

test_that("a lock whose owner is still running is refused", {
  run_dir <- withr::local_tempdir()
  dir.create(file.path(run_dir, ".solve_lock"))
  writeLines("run 010203_999 (pid 999) started 2026-01-01 01:02:03 UTC", file.path(run_dir, ".solve_lock", "owner"))
  local_mocked_bindings(.pid_alive = function(pid, ...) TRUE)
  expect_error(
    .solve_lock_acquire(run_dir = run_dir, timeID = "040506_2", call = NULL),
    "already running in this deploy directory: run 010203_999"
  )
  expect_identical(readLines(file.path(run_dir, ".solve_lock", "owner")), "run 010203_999 (pid 999) started 2026-01-01 01:02:03 UTC")
})

test_that("the lock owner record is parsed, and an unreadable one is left alone", {
  expect_identical(.lock_owner_parse("run 010203_999 (pid 999) started 2026-01-01"), list(run = "010203_999", pid = 999L))
  expect_identical(.lock_owner_parse(character()), list(run = NA_character_, pid = NA_integer_))
  expect_identical(.lock_owner_parse("garbage"), list(run = NA_character_, pid = NA_integer_))
})

test_that("this R process is seen alive and a pid that cannot exist is not", {
  expect_true(.pid_alive(Sys.getpid()))
  expect_false(.pid_alive(2147483647L))
})

test_that("run ids are unique within a session even in the same second", {
  ids <- replicate(3L, .run_id())
  expect_length(unique(ids), 3L)
  expect_match(ids, paste0("^[0-9]{6}_", Sys.getpid(), "_[0-9]+$"))
  counts <- as.integer(sub(".*_", "", ids))
  expect_identical(diff(counts), c(1L, 1L))
})
