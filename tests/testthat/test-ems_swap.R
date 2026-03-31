skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

# --- error tests ---

test_that("ems_swap errors when var is missing", {
  expect_snapshot_error(ems_swap())
})

test_that("ems_swap errors when var is not character", {
  expect_snapshot_error(ems_swap(var = 1))
})

# --- acceptance tests: full variable swap ---

test_that("ems_swap returns a list for full variable swap", {
  result <- ems_swap(var = "tfd")
  expect_type(result, "list")
})

test_that("ems_swap result has var", {
  result <- ems_swap(var = "tfd")
  expect_equal(result[[1]]$var, "tfd")
})

test_that("ems_swap result carries call attribute", {
  result <- ems_swap(var = "tfd")
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_swap result inherits single, full, ems classes", {
  result <- ems_swap(var = "tfd")
  expect_true(inherits(result[[1]], "single"))
  expect_true(inherits(result[[1]], "full"))
})

# --- acceptance tests: partial variable swap ---

test_that("ems_swap accepts partial swap with single element", {
  result <- ems_swap(var = "tfd", TRAD_COMMi = "food", REGr = "chn")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "single"))
  expect_true(inherits(result[[1]], "partial"))
})

test_that("ems_swap accepts partial swap with multiple elements", {
  result <- ems_swap(var = "tfd", TRAD_COMMi = c("food", "crops"), REGr = "usa")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "multi"))
  expect_true(inherits(result[[1]], "partial"))
})
