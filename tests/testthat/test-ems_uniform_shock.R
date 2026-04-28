skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "uniform_shock")

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

test_that("ems_uniform_shock subsets are provided as named lists", {
  expect_snapshot_error(ems_uniform_shock(
    var = "aoall",
    "chn",
    value = -1
  ))
  
  China <- "chn"
  expect_snapshot_error(ems_uniform_shock(
    var = "aoall",
    China,
    value = -1
  ))
})

test_that("ems_uniform_shock errors when variable is not present in the model file", {
  not_a_var <- ems_uniform_shock(
    var = "not_a_var",
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_not_a_var")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = not_a_var,
    write_dir = write_dir
  ))
})

test_that("ems_uniform_shock errors when a set is specified that does not belong to a variable", {
  invalid_set <- ems_uniform_shock(
    REGs = "row",
    var = "pop",
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_invalid_set")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_set,
    write_dir = write_dir
  ))
})

test_that("ems_uniform_shock errors when both int set and year are provided", {
  extra_col <- ems_uniform_shock(
    var = "pop",
    ALLTIMEt = 0,
    Year = 2017,
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_extra_col")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = extra_col,
    write_dir = write_dir
  ))
})

test_that("ems_uniform_shock errors when full var shock is applied to not fully exogenous variable", {
  invalid_shk <- ems_uniform_shock(
    var = "qe",
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_x_full_exo")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_shk,
    write_dir = write_dir
  ))
})

test_that("ems_uniform_shock errors when part var shock is applied to fully endogenous variable", {
  invalid_shk <- ems_uniform_shock(
    REGr = "row",
    var = "qfd",
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_x_full_exo_part")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_shk,
    write_dir = write_dir
  ))
})

test_that("ems_uniform_shock errors when invalid year provide", {
  invalid_year <- ems_uniform_shock(
    REGr = "row",
    Year = 2014,
    var = "pop",
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_invalid_year")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_year,
    write_dir = write_dir
  ))
})

test_that("ems_uniform_shock errors when invalid elements are", {
  invalid_ele <- ems_uniform_shock(
    REGr = "not_an_ele",
    var = "pop",
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_invalid_ele")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = invalid_ele,
    write_dir = write_dir
  ))
})

test_that("ems_uniform_shock errors when endogenous components are allocated shock", {
  endo_ele <- ems_uniform_shock(
    ENDWMSe = "capital",
    ALLTIMEt = 1,
    var = "qe",
    value = 1
  )

  ems_option_set(write_sub_dir = "uni_x_part_exo")
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = endo_ele,
    write_dir = write_dir
  ))
})
