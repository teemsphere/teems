skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

temp_dir <- withr::local_tempdir()

# minimal data frame for custom shock input
REGr <- c("asia", "eit", "lam")
ACTSa <- c("svces", "food", "crops")
df_input <- expand.grid(ACTSa = ACTSa, REGr = REGr, stringsAsFactors = FALSE)
df_input$Value <- runif(nrow(df_input))

csv_path <- file.path(temp_dir, "aoall_shock.csv")
write.csv(df_input, csv_path, row.names = FALSE)

# --- error tests ---

test_that("ems_custom_shock errors when var is missing", {
  expect_snapshot_error(ems_custom_shock())
})

test_that("ems_custom_shock errors when input is missing", {
  expect_snapshot_error(ems_custom_shock(var = "aoall"))
})

test_that("ems_custom_shock errors when var is not character", {
  expect_snapshot_error(ems_custom_shock(var = 1, input = df_input))
})

test_that("ems_custom_shock errors when input is numeric", {
  expect_snapshot_error(ems_custom_shock(var = "aoall", input = 1))
})

test_that("ems_custom_shock errors when input data frame lacks Value column", {
  no_val <- df_input
  no_val$Value <- NULL
  expect_snapshot_error(ems_custom_shock(var = "aoall", input = no_val))
})

# --- acceptance tests: data frame input ---

test_that("ems_custom_shock accepts data frame input", {
  result <- ems_custom_shock(var = "aoall", input = df_input)
  expect_type(result, "list")
})

test_that("ems_custom_shock result inherits 'custom' class", {
  result <- ems_custom_shock(var = "aoall", input = df_input)
  expect_true(inherits(result[[1]], "custom"))
})

test_that("ems_custom_shock result carries call attribute", {
  result <- ems_custom_shock(var = "aoall", input = df_input)
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_custom_shock result carries var attribute", {
  result <- ems_custom_shock(var = "aoall", input = df_input)
  expect_equal(result[[1]]$var, "aoall")
})

# --- acceptance tests: CSV input ---

test_that("ems_custom_shock accepts CSV path input", {
  result <- ems_custom_shock(var = "aoall", input = csv_path)
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "custom"))
})
