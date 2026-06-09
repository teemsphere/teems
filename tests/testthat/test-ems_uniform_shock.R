skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "uniform_shock")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model <- "GTAP-RE"
model_files <- ems_example(model, write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

model <- ems_model(model_file, closure_file)

test_that("ems_uniform_shock errors when var is missing", {
  expect_snapshot_error(ems_uniform_shock())
})

test_that("ems_uniform_shock errors when value is missing", {
  expect_snapshot_error(ems_uniform_shock("pop"))
})

test_that("ems_uniform_shock errors when var is not character", {
  expect_snapshot_error(ems_uniform_shock(1, 2))
})

test_that("ems_uniform_shock errors when value is not numeric", {
  expect_snapshot_error(ems_uniform_shock("pop", "a"))
})

test_that("ems_uniform_shock returns a list for full shock", {
  result <- ems_uniform_shock("pop", 1)
  expect_type(result, "list")
})

test_that("ems_uniform_shock result inherits 'uniform' class", {
  result <- ems_uniform_shock("pop", 1)
  expect_true(inherits(result[[1]], "uniform"))
})

test_that("ems_uniform_shock result carries call attribute", {
  result <- ems_uniform_shock("pop", 1)
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_uniform_shock result carries var attribute", {
  result <- ems_uniform_shock("pop", 1)
  expect_equal(result[[1]]$var, "pop")
})

test_that("ems_uniform_shock result carries value attribute", {
  result <- ems_uniform_shock("pop", 2.5)
  expect_equal(result[[1]]$input, 2.5)
})

test_that("ems_uniform_shock accepts partial shock with single element", {
  result <- ems_uniform_shock("afeall", 2, REGr = "chn")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "uniform"))
})

test_that("ems_uniform_shock accepts partial shock with multiple set filters", {
  result <- ems_uniform_shock("aoall", -3, REGr = "chn", ACTSa = "crops")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "uniform"))
})

test_that("ems_uniform_shock errors when variable is not present in the model file", {
  not_a_var <- ems_uniform_shock("not_a_var", 1)
  nest_temp("uni_not_a_var", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, not_a_var))
})

test_that("ems_uniform_shock errors when a set is specified that does not belong to a variable", {
  invalid_set <- ems_uniform_shock("pop", 1, REGs = "row")
  nest_temp("uni_invalid_set", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_set))
})

test_that("ems_uniform_shock errors when both int set and year are provided", {
  extra_col <- ems_uniform_shock("pop", 1, ALLTIMEt = 0, Year = 2023)
  nest_temp("uni_extra_col", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, extra_col))
})

test_that("ems_uniform_shock errors when full var shock is applied to not fully exogenous variable", {
  invalid_shk <- ems_uniform_shock("qe", 1)
  nest_temp("uni_x_full_exo", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_shk))
})

test_that("ems_uniform_shock errors when part var shock is applied to fully endogenous variable", {
  invalid_shk <- ems_uniform_shock("qfd", 1, REGr = "row")
  nest_temp("uni_x_full_exo_part", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_shk))
})

test_that("ems_uniform_shock errors when invalid year provided", {
  invalid_year <- ems_uniform_shock("pop", 1, REGr = "row", Year = 2014)
  nest_temp("uni_invalid_year", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_year))
})

test_that("ems_uniform_shock errors when invalid elements are", {
  invalid_ele <- ems_uniform_shock("pop", 1, REGr = "not_an_ele")
  nest_temp("uni_invalid_ele", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_ele))
})

test_that("ems_uniform_shock errors when endogenous components are allocated shock", {
  endo_ele <- ems_uniform_shock("qe", 1, ENDWMSe = "capital", ALLTIMEt = 1)
  nest_temp("uni_x_part_exo", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, endo_ele))
})

test_that("ems_uniform_shock errors dots passed without names", {
  expect_snapshot_error(ems_uniform_shock("qe", 1, "capital", ALLTIMEt = 1))
})

test_that("ems_uniform examples work", {
  # Full uniform: 2% shock imposed on all afeall tuples
  expect_s3_type(ems_uniform_shock(var = "afeall", value = 2), "list")

  # Partial uniform by element: applied only to the "chn"
  # element in set REGr (REG). Note that set designations must
  # consist of the concatenation of the standard set (e.g., REG)
  # and variable-specific index (e.g., r).
  expect_s3_type(ems_uniform_shock(var = "afeall",
                    REGr = "chn",
                    value = 2), "list")

  # tested in GTAP-RE/part_uniform_subset_swap.R
  
  # Partially uniform by subset and element: applied to "chn"
  # and "usa" across the MARG and FWDTIME subsets.
  expect_s3_type(ems_uniform_shock(var = "qxs",
                    COMMc = "MARG",
                    REGs = c("chn", "usa"),
                    ALLTIMEt = "FWDTIME",
                    value = -1), "list")
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)