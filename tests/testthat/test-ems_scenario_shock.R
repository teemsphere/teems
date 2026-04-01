skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

temp_dir <- withr::local_tempdir()

# minimal data frame for scenario shock input (absolute values with Year column)
REGr <- c("usa", "chn", "row")
Year <- c(2017, 2018, 2019)
df_input <- expand.grid(REGr = REGr, Year = Year, stringsAsFactors = FALSE)
df_input$Value <- runif(nrow(df_input), min = 1e6, max = 1e9)

csv_path <- file.path(temp_dir, "pop_scenario.csv")
write.csv(df_input, csv_path, row.names = FALSE)

# --- error tests ---

test_that("ems_scenario_shock errors when var is missing", {
  expect_snapshot_error(ems_scenario_shock())
})

test_that("ems_scenario_shock errors when input is missing", {
  expect_snapshot_error(ems_scenario_shock(var = "pop"))
})

test_that("ems_scenario_shock errors when var is not character", {
  expect_snapshot_error(ems_scenario_shock(var = 1, input = df_input))
})

test_that("ems_scenario_shock errors when input is numeric", {
  expect_snapshot_error(ems_scenario_shock(var = "pop", input = 42))
})

test_that("ems_scenario_shock errors when input data frame lacks Value column", {
  no_val <- df_input
  no_val$Value <- NULL
  expect_snapshot_error(ems_scenario_shock(var = "pop", input = no_val))
})

# --- acceptance tests: data frame input ---

test_that("ems_scenario_shock accepts data frame input", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_type(result, "list")
})

test_that("ems_scenario_shock result inherits 'scenario' class", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_true(inherits(result[[1]], "scenario"))
})

test_that("ems_scenario_shock result carries call attribute", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_scenario_shock result carries var attribute", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_equal(result[[1]]$var, "pop")
})

# --- acceptance tests: CSV input ---

test_that("ems_scenario_shock accepts CSV path input", {
  result <- ems_scenario_shock(var = "pop", input = csv_path)
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "scenario"))
})
