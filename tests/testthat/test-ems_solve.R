skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

# general test data
static_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

dynamic_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "deploy")

if (dir.exists(write_dir)) {
  unlink(list.dirs(write_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(write_dir, recursive = TRUE)
}

dynamic_model <- "GTAP-RE"
dynamic_model_files <- ems_example(dynamic_model, write_dir = write_dir)
dynamic_model_file <- dynamic_model_files[["model_file"]]
dynamic_closure_file <- dynamic_model_files[["closure_file"]]

# dynamic test model
dynamic_model <- ems_model(model_file = dynamic_model_file, closure_file = dynamic_closure_file)

static_model <- "GTAPv7"
static_model_files <- ems_example(static_model, write_dir = write_dir)
static_model_file <- static_model_files[["model_file"]]
static_closure_file <- static_model_files[["closure_file"]]

# static test model
static_model <- ems_model(model_file = static_model_file, closure_file = static_closure_file)

# --- integration tests ---

test_that("ems_solve suppress_outputs returns cmf_path character", {
  ems_option_set(write_sub_dir = "solve_suppress")
  cmf_path <- ems_deploy(.data = static_data, model = static_model, write_dir = write_dir)
  result <- ems_solve(cmf_path = cmf_path, suppress_outputs = TRUE)
  expect_type(result, "NULL")
})

# --- error tests ---

test_that("ems_solve errors when cmf_path is missing", {
  expect_snapshot_error(ems_solve())
})

test_that("ems_solve errors when n_tasks is not integerish", {
  ems_option_set(write_sub_dir = "solve_err_tasks")
  cmf_path <- ems_deploy(.data = static_data, model = static_model, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, n_tasks = 1.5))
})

test_that("ems_solve errors when steps is not length 3", {
  ems_option_set(write_sub_dir = "solve_err_steps")
  cmf_path <- ems_deploy(.data = static_data, model = static_model, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, steps = c(2L, 4L)))
})

test_that("ems_solve errors when steps are mixed odd/even", {
  ems_option_set(write_sub_dir = "solve_err_mixed")
  cmf_path <- ems_deploy(.data = static_data, model = static_model, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, steps = c(2L, 3L, 4L)))
})

test_that("ems_solve errors when SBBD used with static model", {
  ems_option_set(write_sub_dir = "solve_err_sbbd")
  cmf_path <- ems_deploy(.data = static_data, model = static_model, write_dir = write_dir)
  expect_snapshot_error(ems_solve(cmf_path = cmf_path, matrix_method = "SBBD"))
})

test_that("ems_solve errors when solution errors detected", {
  ems_option_set(write_sub_dir = "solve_err_error")
  shock <- ems_uniform_shock(
    var = "pop",
    value = 1e6
  )
  cmf_path <- ems_deploy(.data = static_data, model = static_model, shock = shock, write_dir = write_dir)
  expect_snapshot(ems_solve(cmf_path = cmf_path),
    error = TRUE,
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    }
  )
})

test_that("ems_solve errors when solution singularity detected", {
  ems_option_set(write_sub_dir = "solve_err_sing")
  cmf_path <- ems_deploy(.data = static_data, model = static_model, swap_out = "pop", write_dir = write_dir)
  expect_snapshot(ems_solve(cmf_path = cmf_path),
    error = TRUE,
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    }
  )
})

test_that("ems_solve warns when poor accuracy", {
  ems_option_set(write_sub_dir = "solve_wrn_accur")
  shock <- ems_uniform_shock(
    var = "pop",
    value = 200
  )
  cmf_path <- ems_deploy(.data = static_data, model = static_model, shock = shock, write_dir = write_dir)
  expect_snapshot_warning(ems_solve(
    cmf_path = cmf_path,
    solution_method = "mod_midpoint"
  ))
})

test_that("ems_solve informs terminal run", {
  ems_option_set(write_sub_dir = "solve_info_terminal")
  cmf_path <- ems_deploy(.data = static_data, model = static_model, write_dir = write_dir)
  expect_snapshot(
    ems_solve(
      cmf_path = cmf_path,
      terminal_run = TRUE
    ),
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    }
  )
})

test_that("ems_solve returns the same output across static matrix methods", {
  ems_option_set(write_sub_dir = "solve_static_method")
  numeraire <- ems_uniform_shock(
    var = "pfactwld",
    value = 5
  )
  cmf_path <- ems_deploy(.data = static_data, model = static_model, shock = numeraire, write_dir = write_dir)
  LU <- ems_solve(
    cmf_path = cmf_path,
    n_subintervals = 2,
    matrix_method = "LU",
    solution_method = "mod_midpoint"
  )

  DBBD <- ems_solve(
    cmf_path = cmf_path,
    n_tasks = 2,
    n_subintervals = 2,
    matrix_method = "DBBD",
    solution_method = "mod_midpoint"
  )

  check <- all.equal(LU, DBBD, tolerance = 1e-4)
  expect_true(check)
})

test_that("ems_solve returns the same output across static matrix methods", {
  ems_option_set(write_sub_dir = "solve_dynamic_method")
  numeraire <- ems_uniform_shock(
    var = "pfactwld",
    value = 5
  )
  cmf_path <- ems_deploy(.data = dynamic_data, model = dynamic_model, shock = numeraire, write_dir = write_dir)
  LU <- ems_solve(
    cmf_path = cmf_path,
    n_subintervals = 2,
    matrix_method = "LU",
    solution_method = "mod_midpoint"
  )
  
  SBBD <- ems_solve(
    cmf_path = cmf_path,
    n_tasks = 2,
    n_subintervals = 2,
    matrix_method = "SBBD",
    solution_method = "mod_midpoint"
  )
  
  NDBBD <- ems_solve(
    cmf_path = cmf_path,
    n_tasks = 2,
    n_subintervals = 2,
    matrix_method = "NDBBD",
    solution_method = "mod_midpoint"
  )
  
  LU_SBBD_check <- all.equal(LU, SBBD, tolerance = 1e-4)
  LU_NDBBD_check <- all.equal(LU, NDBBD, tolerance = 1e-4)
  expect_all_true(check1, check2)
})