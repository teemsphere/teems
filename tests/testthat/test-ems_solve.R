skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

model <- "GTAPv7"
model_files <- ems_example(model)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "v11", model)

if (!dir.exists(write_dir)) {
  dir.create(write_dir, recursive = TRUE)
}

# --- error tests ---

test_that("ems_solve errors when cmf_path is missing", {
  expect_snapshot_error(ems_solve())
})

test_that("ems_solve errors when n_tasks is not integerish", {
  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  mod <- ems_model(model_file = model_file, closure_file = closure_file)
  ems_option_set(write_sub_dir = "solve_err_tasks")
  cmf_path <- ems_deploy(.data = .data, model = mod, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, n_tasks = 1.5))
})

test_that("ems_solve errors when steps is not length 3", {
  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  mod <- ems_model(model_file = model_file, closure_file = closure_file)
  ems_option_set(write_sub_dir = "solve_err_steps")
  cmf_path <- ems_deploy(.data = .data, model = mod, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, steps = c(2L, 4L)))
})

test_that("ems_solve errors when steps are mixed odd/even", {
  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  mod <- ems_model(model_file = model_file, closure_file = closure_file)
  ems_option_set(write_sub_dir = "solve_err_mixed")
  cmf_path <- ems_deploy(.data = .data, model = mod, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, steps = c(2L, 3L, 4L)))
})

test_that("ems_solve errors when SBBD used with static model", {
  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  mod <- ems_model(model_file = model_file, closure_file = closure_file)
  ems_option_set(write_sub_dir = "solve_err_sbbd")
  cmf_path <- ems_deploy(.data = .data, model = mod, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, matrix_method = "SBBD"))
})

# --- integration tests ---

test_that("ems_solve suppress_outputs returns cmf_path character", {
  .data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    COMM = "macro_sector",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  mod <- ems_model(model_file = model_file, closure_file = closure_file)
  ems_option_set(write_sub_dir = "solve_suppress")
  cmf_path <- ems_deploy(.data = .data, model = mod, write_dir = write_dir)
  result <- ems_solve(cmf_path = cmf_path, suppress_outputs = TRUE)
  expect_type(result, "NULL")
})
