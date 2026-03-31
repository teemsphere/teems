skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

# --- error tests ---

test_that("ems_uniform_shock errors when var is missing", {
  expect_snapshot_error(ems_uniform_shock())
})

test_that("ems_uniform_shock errors when value is missing", {
  expect_snapshot_error(ems_uniform_shock(var = "pop"))
})

test_that("ems_uniform_shock errors when var is not character", {
  expect_snapshot_error(ems_uniform_shock(var = 1, value = 2))
})

test_that("ems_uniform_shock errors when value is not numeric", {
  expect_snapshot_error(ems_uniform_shock(var = "pop", value = "a"))
})

# --- acceptance tests: full uniform shock ---

test_that("ems_uniform_shock returns a list for full shock", {
  result <- ems_uniform_shock(var = "pop", value = 1)
  expect_type(result, "list")
})

test_that("ems_uniform_shock result inherits 'uniform' class", {
  result <- ems_uniform_shock(var = "pop", value = 1)
  expect_true(inherits(result[[1]], "uniform"))
})

test_that("ems_uniform_shock result inherits 'shock' class", {
  result <- ems_uniform_shock(var = "pop", value = 1)
  expect_true(inherits(result[[1]], "shock"))
})

test_that("ems_uniform_shock result carries call attribute", {
  result <- ems_uniform_shock(var = "pop", value = 1)
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_uniform_shock result carries var attribute", {
  result <- ems_uniform_shock(var = "pop", value = 1)
  expect_equal(result[[1]]$var, "pop")
})

test_that("ems_uniform_shock result carries value attribute", {
  result <- ems_uniform_shock(var = "pop", value = 2.5)
  expect_equal(result[[1]]$input, 2.5)
})

# --- acceptance tests: partial uniform shock ---

test_that("ems_uniform_shock accepts partial shock with single element", {
  result <- ems_uniform_shock(var = "afeall", REGr = "chn", value = 2)
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "uniform"))
})

test_that("ems_uniform_shock accepts partial shock with multiple set filters", {
  result <- ems_uniform_shock(var = "aoall", REGr = "chn", ACTSa = "crops", value = -3)
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "uniform"))
})
