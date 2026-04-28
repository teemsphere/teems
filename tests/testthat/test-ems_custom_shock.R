skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "custom_shock")

if (dir.exists(write_dir)) { 
  unlink(list.dirs(write_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(write_dir, recursive = TRUE)
}

model <- "GTAP-RE"
model_files <- ems_example(model, write_dir = write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

# general test data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

# general test model
model <- ems_model(model_file = model_file, closure_file = closure_file)
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

test_that("ems_custom_shock errors when both year and int set are provided", {
  extra_col <- data.frame(REGr = "row", Value = 2, ALLTIMEt = 0, Year = 2017)

  extra_col <- ems_custom_shock(
    var = "pop",
    input = extra_col
  )

  ems_option_set(write_sub_dir = "cust_extra_col")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = extra_col,
    write_dir = write_dir
  ))
})

test_that("ems_custom_shock errors when invalid year is provided", {
  invalid_year <- data.frame(REGr = "row", Value = 2, Year = 2016)
  invalid_year <- ems_custom_shock(
    var = "pop",
    input = invalid_year
  )

  ems_option_set(write_sub_dir = "cust_invalid_year")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_year,
    write_dir = write_dir
  ))
})

test_that("ems_custom_shock errors when input is missing sets", {
  missing_set <- data.frame(REGr = "not_an_ele", Value = 2)
  
  missing_set <- ems_custom_shock(
    var = "pop",
    input = missing_set
  )

  ems_option_set(write_sub_dir = "cust_missing_set")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = missing_set,
    write_dir = write_dir
  ))
})

test_that("ems_custom_shock errors when invalid tuple is provided", {
  invalid_tup <- data.frame(REGr = "not_an_ele", ALLTIMEt = 0, Value = 2)
  
  invalid_tup <- ems_custom_shock(
    var = "pop",
    input = invalid_tup
  )

  ems_option_set(write_sub_dir = "cust_invalid_tup")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_tup,
    write_dir = write_dir
  ))
})

test_that("ems_custom_shock errors when some shock tuples are endogenous", {
  invalid_tup <- data.frame(ENDWMSe = "capital", REGr = "row", ALLTIMEt = c(0, 1), Value = 2)
  invalid_tup <- ems_custom_shock(
    var = "qe",
    input = invalid_tup
  )

  ems_option_set(write_sub_dir = "cust_endo_tup")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_tup,
    write_dir = write_dir
  ))
})