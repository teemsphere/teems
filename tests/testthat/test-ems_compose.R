skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

model <- "GTAP-RE"
model_files <- ems_example(model)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "v11", model)

if (!dir.exists(write_dir)) {
  dir.create(write_dir, recursive = TRUE)
}

.data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  COMM = "macro_sector",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2, 3)
)
model <- ems_model(model_file = model_file, closure_file = closure_file)
n_var <- nrow(model[which(model$type == "Variable"),])
n_coeff <- nrow(model[which(model$type == "Coefficient"),])
ems_option_set(write_sub_dir = "compose_test")
cmf_path <- ems_deploy(.data = .data, model = model, write_dir = write_dir)
ems_solve(cmf_path = cmf_path, suppress_outputs = TRUE)

# --- error tests ---

test_that("ems_compose errors when cmf_path is missing", {
  expect_snapshot_error(ems_compose())
})

test_that("ems_compose errors when type is invalid", {
  expect_snapshot_error(ems_compose(cmf_path = cmf_path, type = "bad_type"))
})

test_that("ems_compose errors when name is not character", {
  expect_snapshot_error(ems_compose(cmf_path = cmf_path, name = 1))
})

# --- acceptance tests ---

test_that("ems_compose returns tibble for type = 'all'", {
  result <- ems_compose(cmf_path = cmf_path, type = "all")
  expect_s3_class(result, "tbl")
})

test_that("ems_compose returns list for type = 'variable'", {
  result <- ems_compose(cmf_path = cmf_path, type = "variable")
  expect_s3_class(result, "tbl")
})

test_that("ems_compose returns list for type = 'coefficient'", {
  result <- ems_compose(cmf_path = cmf_path, type = "coefficient")
  expect_s3_class(result, "tbl")
})

test_that("ems_compose variable output contains expected elements", {
  result <- ems_compose(cmf_path = cmf_path, type = "variable")
  expect_equal(nrow(result), n_var)
})

test_that("ems_compose coefficient output contains expected elements", {
  result <- ems_compose(cmf_path = cmf_path, type = "coefficient")
  expect_equal(nrow(result), n_coeff)
})

test_that("ems_compose accepts name filter for variables", {
  result <- ems_compose(cmf_path = cmf_path, type = "variable", name = "qfd")
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose accepts name filter for coefficients", {
  result <- ems_compose(cmf_path = cmf_path, type = "coefficient", name = "SAVE")
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose accepts minimal = TRUE", {
  result <- ems_compose(cmf_path = cmf_path, minimal = TRUE)
  expect_type(result, "list")
})

