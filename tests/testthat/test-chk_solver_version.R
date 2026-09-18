skip_on_cran()

withr::defer(ems_option_reset(), teardown_env())

reset_cache <- function() {
  rm(list = ls(.solver_version_cache), envir = .solver_version_cache)
}

test_that("the version core drops pre-release and development suffixes", {
  expect_equal(.solver_version_core("1.1.0-dev.4"), numeric_version("1.1.0"))
  expect_equal(.solver_version_core("1.0.0.9000"), numeric_version("1.0.0"))
  expect_equal(.solver_version_core("2.3.1"), numeric_version("2.3.1"))
  expect_true(is.na(.solver_version_core("garbage")))
})

test_that("a silent image (pre-1.1) aborts by name", {
  reset_cache()
  ems_option_reset()
  local_mocked_bindings(.solver_version_query = function(image) NA_character_)
  expect_error(
    .check_solver_version("teems:old", call = NULL),
    "predates the versioned interface"
  )
  expect_error(.check_solver_version("teems:old", call = NULL), "1\\.1\\.0")
})

test_that("a major-version mismatch aborts by name", {
  reset_cache()
  ems_option_reset()
  local_mocked_bindings(.solver_version_query = function(image) "2.0.0")
  expect_error(
    .check_solver_version("teems:two", call = NULL),
    "major version"
  )
})

test_that("a solver below the package floor aborts by name", {
  reset_cache()
  ems_option_reset()
  local_mocked_bindings(.solver_version_query = function(image) "1.0.9")
  expect_error(
    .check_solver_version("teems:floor", call = NULL),
    "requires 1\\.1\\.0 or later"
  )
})

test_that("a conforming image passes, is cached, and is asked once", {
  reset_cache()
  ems_option_reset()
  rec <- new.env(parent = emptyenv())
  rec$calls <- 0L
  local_mocked_bindings(.solver_version_query = function(image) {
    rec$calls <- rec$calls + 1L
    "1.1.0-dev.4"
  })
  expect_identical(.check_solver_version("teems:ok", call = NULL), "1.1.0-dev.4")
  expect_identical(.check_solver_version("teems:ok", call = NULL), "1.1.0-dev.4")
  expect_identical(rec$calls, 1L)
  expect_identical(get("teems:ok", envir = .solver_version_cache), "1.1.0-dev.4")
})

test_that("version_check = 'warn' downgrades the abort to a warning, once", {
  reset_cache()
  ems_option_set(version_check = "warn")
  local_mocked_bindings(.solver_version_query = function(image) NA_character_)
  expect_warning(
    .check_solver_version("teems:old", call = NULL),
    "predates the versioned interface"
  )
  expect_no_warning(.check_solver_version("teems:old", call = NULL))
  ems_option_reset()
})

test_that("version_check = 'off' never asks the image", {
  reset_cache()
  ems_option_set(version_check = "off")
  local_mocked_bindings(.solver_version_query = function(image) stop("asked"))
  expect_null(.check_solver_version("teems:any", call = NULL))
  ems_option_reset()
})

test_that("the local solve image answers -version with a semantic version", {
  skip_if_not(nzchar(Sys.which("docker")))
  image <- paste0("teems:", .resolve_docker_tag(quiet = TRUE))
  v <- .solver_version_query(image)
  expect_match(v, "^[0-9]+\\.[0-9]+\\.[0-9]+")
  reset_cache()
  ems_option_reset()
  expect_identical(.check_solver_version(image, call = NULL), v)
})
