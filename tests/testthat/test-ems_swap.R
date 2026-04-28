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

test_that("ems_swap errors when var is missing", {
  expect_snapshot_error(ems_swap())
})

test_that("ems_swap errors when var is not character", {
  expect_snapshot_error(ems_swap(var = 1))
})

# --- acceptance tests: full variable swap ---

test_that("ems_swap returns a list for full variable swap", {
  result <- ems_swap(var = "tfd")
  expect_type(result, "list")
})

test_that("ems_swap result has var", {
  result <- ems_swap(var = "tfd")
  expect_equal(result[[1]]$var, "tfd")
})

test_that("ems_swap result carries call attribute", {
  result <- ems_swap(var = "tfd")
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_swap result inherits single, full, ems classes", {
  result <- ems_swap(var = "tfd")
  expect_true(inherits(result[[1]], "single"))
  expect_true(inherits(result[[1]], "full"))
})

# --- acceptance tests: partial variable swap ---

test_that("ems_swap accepts partial swap with single element", {
  result <- ems_swap(var = "tfd", TRAD_COMMi = "food", REGr = "chn")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "single"))
  expect_true(inherits(result[[1]], "partial"))
})

test_that("ems_swap accepts partial swap with multiple elements", {
  result <- ems_swap(var = "tfd", TRAD_COMMi = c("food", "crops"), REGr = "usa")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "multi"))
  expect_true(inherits(result[[1]], "partial"))
})

test_that("ems_swap errors when invalid closure element entry swapped in", {
  swap_out <- ems_swap(
    var = "tfd",
    REGr = "row"
  )
  swap_in <- ems_swap(
    var = "qfd",
    REGr = "not_an_ele"
  )
  ems_option_set(write_sub_dir = "invalid_swap")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_in = swap_in,
      swap_out = swap_out,
      write_dir = write_dir
    )
  )
})

test_that("ems_swap errors when invalid closure element entry swapped out", {
  swap_out <- ems_swap(
    var = "tfd",
    REGr = "not_an_ele"
  )
  swap_in <- ems_swap(
    var = "qfd",
    REGr = "row"
  )
  ems_option_set(write_sub_dir = "invalid_swap")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_in = swap_in,
      swap_out = swap_out,
      write_dir = write_dir
    )
  )
})

test_that("ems_swap errors when invalid set provided to swap-in", {
  ems_option_set(write_sub_dir = "invalid_swap")
  
  swap_in <- ems_swap(var = "qfd",
                      REGs = "row")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_in = swap_in,
      write_dir = write_dir
    )
  )
})

test_that("ems_swap errors when invalid set provided to swap-out", {
  ems_option_set(write_sub_dir = "invalid_swap")
  
  swap_out <- ems_swap(var = "tfd",
                       REGs = "row")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_out = swap_out,
      write_dir = write_dir
    )
  )
})

test_that("ems_swap errors when invalid element provided to swap-in", {
  ems_option_set(write_sub_dir = "invalid_swap")
  
  swap_in <- ems_swap(var = "qfd",
                      REGr = "not_an_ele")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_in = swap_in,
      write_dir = write_dir
    )
  )
})

test_that("ems_swap errors when invalid element provided to swap-out", {
  ems_option_set(write_sub_dir = "invalid_swap")
  
  swap_out <- ems_swap(var = "tfd",
                       REGr = "not_an_ele")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_out = swap_out,
      write_dir = write_dir
    )
  )
})

test_that("ems_swap errors when endogenous components are selected to swap out", {
  ems_option_set(write_sub_dir = "invalid_swap")
  
  swap_out <- ems_swap(var = "tfd",
                       REGr = "not_an_ele")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      swap_out = swap_out,
      write_dir = write_dir
    )
  )
})

test_that("full ems_swap out var not fully exogenous", {
  swap_out <- ems_swap(var = "qe")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap out some ele not exogenous", {
  swap_out <- ems_swap(
    var = "qe",
    ENDWMSe = "ENDWC",
    ALLTIMEt = "FWDTIME"
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap out no entry", {
  swap_out <- ems_swap(
    var = "qfd",
    COMMc = "food"
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap duplicate tup in closure", {
  swap_in <- ems_swap(
    var = "qe",
    ENDWMSe = "ENDWC",
    ALLTIMEt = 0
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})

test_that("ems_swap in ele component not valid", {
  swap_in <- ems_swap(
    var = "qfd",
    COMMc = "zzz"
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})

test_that("ems_swap out var component no entry", {
  swap_out <- ems_swap(
    var = "qfd",
    COMMc = "zzz"
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap in variable not valid", {
  swap_in <- ems_swap(var = "not_a_var")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})

test_that("ems_swap out variable not valid", {
  swap_out <- ems_swap(var = "not_a_var")
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})

test_that("ems_swap in subset not valid", {
  swap_in <- ems_swap(
    var = "pop",
    NOT_A_SET = "usa"
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_in = swap_in, write_dir = write_dir
  ))
})

test_that("ems_swap out subset not valid", {
  swap_out <- ems_swap(
    var = "pop",
    NOT_A_SET = "usa"
  )
  ems_option_set(write_sub_dir = "deploy_swap")
  expect_snapshot_error(ems_deploy(
    .data = dat, model = model,
    swap_out = swap_out, write_dir = write_dir
  ))
})