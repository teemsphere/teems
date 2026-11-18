skip_on_cran()

reset_caches <- function() {
  rm(list = ls(.solver_release_cache), envir = .solver_release_cache)
}

mock_latest <- function(latest) {
  local_mocked_bindings(
    .solver_release_latest = function(timeout = 2) latest,
    .env = parent.frame()
  )
}

test_that("a newer release of the same major version points to the rebuild", {
  reset_caches()
  mock_latest(list(version = "1.2.0", url = "https://github.com/teemsphere/teems-solver/releases/tag/v1.2.0"))
  expect_snapshot(newer <- .solver_release_notice("teems:latest", "1.1.0", call = NULL))
  expect_true(newer)
})

test_that("a newer release of the next major version asks for the package first", {
  reset_caches()
  mock_latest(list(version = "2.0.0", url = NULL))
  expect_snapshot(newer <- .solver_release_notice("teems:latest", "1.2.0", call = NULL))
  expect_true(newer)
})

test_that("a current image, a build ahead of the release and an unreachable GitHub are silent", {
  reset_caches()
  mock_latest(list(version = "1.2.0", url = NULL))
  expect_silent(newer <- .solver_release_notice("teems:a", "1.2.0", call = NULL))
  expect_false(newer)
  reset_caches()
  mock_latest(list(version = "1.2.0", url = NULL))
  expect_silent(newer <- .solver_release_notice("teems:b", "1.2.1-dev.1", call = NULL))
  expect_false(newer)
  reset_caches()
  mock_latest(NULL)
  expect_silent(newer <- .solver_release_notice("teems:c", "1.1.0", call = NULL))
  expect_false(newer)
})

test_that("the notice is given once per image per session and GitHub is read once", {
  reset_caches()
  rec <- new.env(parent = emptyenv())
  rec$calls <- 0L
  local_mocked_bindings(
    .solver_release_latest = function(timeout = 2) {
      rec$calls <- rec$calls + 1L
      list(version = "1.2.0", url = NULL)
    }
  )
  expect_message(.solver_release_notice("teems:a", "1.1.0", call = NULL), "1\\.2\\.0 is available")
  expect_silent(.solver_release_notice("teems:a", "1.1.0", call = NULL))
  expect_message(.solver_release_notice("teems:b", "1.1.0", call = NULL), "1\\.2\\.0 is available")
  expect_identical(rec$calls, 1L)
})

test_that("the release reader returns NULL on an unreachable host", {
  withr::local_options(timeout = 2)
  local_mocked_bindings(.solver_release_api = function() "https://127.0.0.1:9/nothing")
  expect_null(.solver_release_latest(timeout = 2))
})

test_that("the release reader parses the published tag (live)", {
  skip_if_offline()
  latest <- .solver_release_latest()
  skip_if(is.null(latest), "GitHub API not reachable or rate limited")
  expect_false(is.na(.solver_version_core(latest$version)))
})
