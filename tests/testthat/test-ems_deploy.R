skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "deploy")

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

test_that("ems_deploy errors when .data is missing", {
  expect_snapshot_error(ems_deploy())
})

test_that("ems_deploy errors when model is missing", {
  expect_snapshot_error(ems_deploy(.data = dat))
})

# --- acceptance tests ---

test_that("ems_deploy returns character path to CMF file", {
  ems_option_set(write_sub_dir = "deploy_test")
  cmf_path <- ems_deploy(.data = dat, model = model, write_dir = write_dir)
  expect_type(cmf_path, "character")
})

test_that("ems_deploy returns path to an existing CMF file", {
  ems_option_set(write_sub_dir = "deploy_test2")
  cmf_path <- ems_deploy(.data = dat, model = model, write_dir = write_dir)
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts ems_swap variable swap", {
  shk <- ems_uniform_shock(var = "qfd", value = 1)
  swap_in <- ems_swap(var = "qfd")
  swap_out <- ems_swap(var = "tfd")
  ems_option_set(write_sub_dir = "deploy_swap")
  cmf_path <- ems_deploy(
    .data = dat, model = model, shock = shk,
    swap_in = swap_in, swap_out = swap_out,
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts direct input full variable swap", {
  shk <- ems_uniform_shock(var = "qfd", value = 1)
  ems_option_set(write_sub_dir = "deploy_swap")
  cmf_path <- ems_deploy(
    .data = dat, model = model, shock = shk,
    swap_in = "qfd", swap_out = "tfd",
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap full variable swap", {
  shk <- ems_uniform_shock(var = "qfd", value = 1)
  swap_in <- ems_swap(var = "yp")
  swap_out <- ems_swap(var = "dppriv")
  ems_option_set(write_sub_dir = "deploy_swap")
  cmf_path <- ems_deploy(
    .data = dat, model = model, shock = shk,
    swap_in = list(swap_in, "qfd"), swap_out = list(swap_out, "tfd"),
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap partial variable swap", {
  shk <- ems_uniform_shock(var = "qfd", value = 1)
  swap_in <- ems_swap(
    var = "yp",
    REGr = "row"
  )
  swap_out <- ems_swap(
    var = "dppriv",
    REGr = "row"
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  cmf_path <- ems_deploy(
    .data = dat, model = model, shock = shk,
    swap_in = list(swap_in, "qfd"), swap_out = list(swap_out, "tfd"),
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy errors when invalid variable provided for swap-in", {
  ems_option_set(write_sub_dir = "invalid_swap")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_in = "not_a_var",
      swap_out = "tfd",
      write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when invalid variable provided for swap-out", {
  ems_option_set(write_sub_dir = "invalid_swap")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_in = "qfd",
      swap_out = "not_a_var",
      write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when shock_file and shock are both provided", {
  shk <- ems_uniform_shock(var = "pop", value = 1)
  ems_option_set(write_sub_dir = "deploy_shk_file")
  expect_snapshot_error(
    ems_deploy(
      .data = dat, model = model, shock = shk,
      shock_file = "fake.shf", write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when read-in headers not present in data", {
  mod_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data <- mod_data[!names(mod_data) %in% "SAVE"]
  ems_option_set(write_sub_dir = "deploy_missing_hdr")
  expect_snapshot_error(
    ems_deploy(
      .data = mod_data, model = model, write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when read-in headers are missing mapping", {
  mod_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data <- mod_data[!names(mod_data) %in% "REG"]
  ems_option_set(write_sub_dir = "deploy_missing_map")
  expect_snapshot_error(
    ems_deploy(
      .data = mod_data, model = model, write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when timesteps provided to static model", {
  model <- "GTAPv7"
  model_files <- ems_example(model)
  model_file <- model_files[["model_file"]]
  closure_file <- model_files[["closure_file"]]

  model <- ems_model(model_file = model_file, closure_file = closure_file)
  write_sub_dir <- "deploy_static_ts"
  ems_option_set(write_sub_dir = write_sub_dir)
  dir.create(file.path(write_dir, write_sub_dir))
  expect_snapshot_error(
    ems_deploy(
      .data = dat, model = model, write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when timesteps not provided to a dynamic model", {
  static_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )

  ems_option_set(write_sub_dir = "deploy_dynamic_no_ts")
  expect_snapshot_error(
    ems_deploy(
      .data = static_data, model = model, write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when set-calculated number of entries does not match a finalized data header", {
  mod_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data$REG[.N, mapping := "row"]

  ems_option_set(write_sub_dir = "deploy_set_mismatch")
  expect_snapshot_error(
    ems_deploy(
      .data = mod_data, model = model, write_dir = write_dir
    )
  )
})

test_that("ems_deploy errors when write_dir does not exist", {
  expect_snapshot_error(
    ems_deploy(
      .data = dat, model = model, write_dir = "/tmp2/does_not_exist"
    )
  )
})

test_that("ems_deploy warns when creating write_dir", {
  dir <- file.path(dirname(tempdir()), "write_dir")
  dir.create(dir)
  expect_snapshot_warning(
    ems_deploy(
      .data = dat, model = model, write_dir = file.path(dir, "tmp")
    )
  )
  unlink(dir, recursive = TRUE)
})

test_that("ems_deploy errors when aggregated inputs are incomplete", {
  mod_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  SAVE <- mod_data$SAVE[!.N, ]
  SAVE$ALLTIMEt <- 0
  colnames(SAVE)[1] <- "REGr"
  model <- ems_model(model_file = model_file, closure_file = closure_file, SAVE = SAVE)
  ems_option_set(write_sub_dir = "deploy_incomplete")
  expect_snapshot_error(
    ems_deploy(
      .data = mod_data, model = model, write_dir = write_dir
    )
  )
})


