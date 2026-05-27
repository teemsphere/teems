skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "swap")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model <- "GTAP-RE"
model_files <- ems_example(write_dir, model)
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

test_that("ems_swap errors when var is missing", {
  expect_snapshot_error(ems_swap())
})

test_that("ems_swap errors when var is not character", {
  expect_snapshot_error(ems_swap(1))
})

test_that("ems_swap returns a list for full variable swap", {
  result <- ems_swap("tfd")
  expect_type(result, "list")
})

test_that("ems_swap result has var", {
  result <- ems_swap("tfd")
  expect_equal(result[[1]]$var, "tfd")
})

test_that("ems_swap result carries call attribute", {
  result <- ems_swap("tfd")
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_swap result inherits single, full, ems classes", {
  result <- ems_swap("tfd")
  expect_true(inherits(result[[1]], "single"))
  expect_true(inherits(result[[1]], "full"))
})

test_that("ems_swap accepts partial swap with single element", {
  result <- ems_swap("tfd", TRAD_COMMi = "food", REGr = "chn")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "single"))
  expect_true(inherits(result[[1]], "partial"))
})

test_that("ems_swap accepts partial swap with multiple elements", {
  result <- ems_swap("tfd", TRAD_COMMi = c("food", "crops"), REGr = "usa")
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "multi"))
  expect_true(inherits(result[[1]], "partial"))
})

test_that("ems_swap errors when invalid closure element entry swapped in", {
  swap_out <- ems_swap("tfd", REGr = "row")
  swap_in <- ems_swap("qfd", REGr = "not_an_ele")
  nest_temp("invalid_swap", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, swap_in = swap_in, swap_out = swap_out)
  )
})

test_that("ems_swap errors when invalid closure element entry swapped out", {
  swap_out <- ems_swap("tfd", REGr = "not_an_ele")
  swap_in <- ems_swap("qfd", REGr = "row")
  nest_temp("invalid_swap2", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, swap_in = swap_in, swap_out = swap_out)
  )
})

test_that("ems_swap errors when invalid set provided to swap-in", {
  swap_in <- ems_swap("qfd", REGs = "row")
  nest_temp("invalid_swap3", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, swap_in = swap_in))
})

test_that("ems_swap errors when dots passed without names", {
  expect_snapshot_error( ems_swap("tfd", "usa"))
})

test_that("ems_swap errors when invalid set provided to swap-out", {
  nest_temp("invalid_swap4", write_dir)
  swap_out <- ems_swap("tfd", REGs = "row")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("ems_swap errors when invalid element provided to swap-in", {
  nest_temp("invalid_swap5", write_dir)
  swap_in <- ems_swap("qfd", REGr = "not_an_ele")
  expect_snapshot_error(ems_deploy(dat, model, swap_in = swap_in))
})

test_that("ems_swap errors when invalid element provided to swap-out", {
  nest_temp("invalid_swap6", write_dir)
  swap_out <- ems_swap("tfd", REGr = "not_an_ele")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("ems_swap errors when endogenous components are selected to swap out", {
  nest_temp("invalid_swap7", write_dir)
  swap_out <- ems_swap("tfd", REGr = "not_an_ele")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("full ems_swap out var not fully exogenous", {
  nest_temp("deploy_swap", write_dir)
  swap_out <- ems_swap("qe")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("ems_swap out some ele not exogenous", {
  nest_temp("deploy_swap2", write_dir)
  swap_out <- ems_swap("qe", ENDWMSe = "ENDWC", ALLTIMEt = "FWDTIME")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("ems_swap out no entry", {
  nest_temp("deploy_swap3", write_dir)
  swap_out <- ems_swap( "qfd", COMMc = "food")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("ems_swap duplicate tup in closure", {
  nest_temp("deploy_swap4", write_dir)
  swap_in <- ems_swap("qe", ENDWMSe = "ENDWC", ALLTIMEt = 0)
  expect_snapshot_error(ems_deploy(dat, model, swap_in = swap_in))
})

test_that("ems_swap in ele component not valid", {
  nest_temp("deploy_swap5", write_dir)
  swap_in <- ems_swap("qfd", COMMc = "zzz")
  expect_snapshot_error(ems_deploy(dat, model, swap_in = swap_in))
})

test_that("ems_swap out var component no entry", {
  nest_temp("deploy_swap6", write_dir)
  swap_out <- ems_swap("qfd", COMMc = "zzz")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("ems_swap in variable not valid", {
  nest_temp("deploy_swap7", write_dir)
  swap_in <- ems_swap("not_a_var")
  expect_snapshot_error(ems_deploy(dat, model, swap_in = swap_in))
})

test_that("ems_swap out variable not valid", {
  nest_temp("deploy_swap8", write_dir)
  swap_out <- ems_swap("not_a_var")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

test_that("ems_swap in subset not valid", {
  nest_temp("deploy_swap9", write_dir)
  swap_in <- ems_swap("pop", NOT_A_SET = "usa")
  expect_snapshot_error(ems_deploy(dat, model, swap_in = swap_in))
})

test_that("ems_swap out subset not valid", {
  nest_temp("deploy_swap10", write_dir)
  swap_out <- ems_swap("pop", NOT_A_SET = "usa")
  expect_snapshot_error(ems_deploy(dat, model, swap_out = swap_out))
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)