skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

data_dir <- "~/src/teems/teems_dat"
dat_input <- file.path(data_dir, "gsdfdat.har")
par_input <- file.path(data_dir, "gsdfpar.har")
set_input <- file.path(data_dir, "gsdfset.har")

model <- "GTAP-RE"
model_files <- ems_example(model)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "v11", model)

if (!dir.exists(write_dir)) {
  dir.create(write_dir, recursive = TRUE)
}

# --- error tests ---

test_that("ems_deploy errors when .data is missing", {
  expect_snapshot_error(ems_deploy())
})

test_that("ems_deploy errors when model is missing", {
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
  expect_snapshot_error(ems_deploy(.data = .data))
})

# --- acceptance tests ---

test_that("ems_deploy returns character path to CMF file", {
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
  ems_option_set(write_sub_dir = "deploy_test")
  cmf_path <- ems_deploy(.data = .data, model = model, write_dir = write_dir)
  expect_type(cmf_path, "character")
})

test_that("ems_deploy returns path to an existing CMF file", {
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
  ems_option_set(write_sub_dir = "deploy_test2")
  cmf_path <- ems_deploy(.data = .data, model = model, write_dir = write_dir)
  expect_true(file.exists(cmf_path))
})

test_that("ems_swap in variable not valid", {
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
  swap_in <- ems_swap(var = "not_a_var")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})

test_that("ems_swap out variable not valid", {
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
  swap_out <- ems_swap(var = "not_a_var")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap in subset not valid", {
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
  swap_in <- ems_swap(var = "pop",
                       NOT_A_SET = "usa")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})

test_that("ems_swap out subset not valid", {
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
  swap_out <- ems_swap(var = "pop",
                       NOT_A_SET = "usa")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap in ele component not valid", {
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
  swap_in <- ems_swap(var = "qfd",
                      COMMc = "zzz")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})

test_that("ems_swap out var component no entry", {
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
  swap_out <- ems_swap(var = "qfd",
                       COMMc = "zzz")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("full ems_swap out var not fully exogenous", {
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
  swap_out <- ems_swap(var = "qe")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

# test_that("ems_swap out some ele not exogenous", {
#   .data <- ems_data(
#     dat_input = dat_input,
#     par_input = par_input,
#     set_input = set_input,
#     REG = "big3",
#     COMM = "macro_sector",
#     ACTS = "macro_sector",
#     ENDW = "labor_agg",
#     time_steps = c(0, 1, 2, 3)
#   )
# 
#   model <- ems_model(model_file = model_file, closure_file = closure_file)
#   swap_out <- ems_swap(var = "qe",
#                        ENDWMSe = "ENDWC",
#                        ALLTIMEt = "FWDTIME")
#   ems_option_set(write_sub_dir = "deploy_swap")
#   expect_snapshot_error(ems_deploy(
#     .data = .data, model = model,
#     swap_out = swap_out, write_dir = write_dir
#   ))
# })

test_that("ems_swap out no entry", {
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
  swap_out <- ems_swap(var = "qfd",
                       COMMc = "food")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap duplicate tup in closure", {
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
  swap_in <- ems_swap(var = "qe",
                      ENDWMSe = "ENDWC",
                      ALLTIMEt = 0)
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = .data, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})


test_that("ems_deploy accepts ems_swap variable swap", {
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
  shk <- ems_uniform_shock(var = "qfd", value = 1)
  swap_in <- ems_swap(var = "qfd")
  swap_out <- ems_swap(var = "tfd")
  ems_option_set(write_sub_dir = "deploy_swap")
  cmf_path <- ems_deploy(
    .data = .data, model = model, shock = shk,
    swap_in = swap_in, swap_out = swap_out,
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts direct input full variable swap", {
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
  shk <- ems_uniform_shock(var = "qfd", value = 1)
  ems_option_set(write_sub_dir = "deploy_swap")
  cmf_path <- ems_deploy(
    .data = .data, model = model, shock = shk,
    swap_in = "qfd", swap_out = "tfd",
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap full variable swap", {
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
  shk <- ems_uniform_shock(var = "qfd", value = 1)
  swap_in <- ems_swap(var = "yp")
  swap_out <- ems_swap(var = "dppriv")
  ems_option_set(write_sub_dir = "deploy_swap")
  cmf_path <- ems_deploy(
    .data = .data, model = model, shock = shk,
    swap_in = list(swap_in, "qfd"), swap_out = list(swap_out, "tfd"),
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap partial variable swap", {
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
    .data = .data, model = model, shock = shk,
    swap_in = list(swap_in, "qfd"), swap_out = list(swap_out, "tfd"),
    write_dir = write_dir
  )
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy errors when shock_file and shock are both provided", {
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
  shk <- ems_uniform_shock(var = "pop", value = 1)
  expect_snapshot_error(
    ems_deploy(
      .data = .data, model = model, shock = shk,
      shock_file = "fake.shf", write_dir = write_dir
    )
  )
})