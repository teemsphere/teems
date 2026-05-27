skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "compose")

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
  dat_input,
  par_input,
  set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)
model <- ems_model(model_file, closure_file)
n_var <- nrow(model[which(model$type == "Variable"), ])
n_coeff <- nrow(model[which(model$type == "Coefficient"), ])
cmf_path <- ems_deploy(dat, model)
ems_solve(cmf_path, suppress_outputs = TRUE)

variant <- Sys.info()["sysname"]

test_that("ems_compose errors when cmf_path is missing", {
  expect_snapshot_error(ems_compose())
})

test_that("ems_compose errors when which is not character", {
  expect_snapshot_error(ems_compose(cmf_path, which = 1))
})

test_that("ems_compose errors when invalid which", {
  expect_snapshot_error(ems_compose(cmf_path, which = "not_a_var"))
})

test_that("ems_compose returns tibble for which = 'all'", {
  result <- ems_compose(cmf_path)
  expect_s3_class(result, "tbl")
})

test_that("ems_compose variable output contains expected elements", {
  result <- ems_compose(cmf_path)
  expect_equal(nrow(result), n_var + n_coeff)
})

test_that("ems_compose selects a variable", {
  result <- ems_compose(cmf_path, "qfd")
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose selects a coefficient", {
  result <- ems_compose(cmf_path, "SAVE")
  SAVE_out <- result$dat$SAVE[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(SAVE_out) <- colnames(dat$SAVE)
  expect_true(all.equal(SAVE_out, dat$SAVE, check.attributes = FALSE, tolerance = 1e-7))
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose selects multiple coefficients", {
  result <- ems_compose(cmf_path, c("SAVE", "VTMFSD"))
  SAVE_out <- result$dat$SAVE[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(SAVE_out) <- colnames(dat$SAVE)
  expect_true(all.equal(SAVE_out, dat$SAVE, check.attributes = FALSE, tolerance = 1e-7))
  VTWR_out <- result$dat$VTMFSD[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(VTWR_out) <- colnames(dat$VTWR)
  expect_true(all.equal(VTWR_out, dat$VTWR, check.attributes = FALSE, tolerance = 1e-7))
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose selects variable/coefficient mix", {
  result <- ems_compose(cmf_path, c("qtmfsd", "SAVE"))
  SAVE_out <- result$dat$SAVE[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(SAVE_out) <- colnames(dat$SAVE)
  expect_true(all.equal(SAVE_out, dat$SAVE, check.attributes = FALSE, tolerance = 1e-7))
  expect_all_true(result$dat$qtmfsd$Value == 0)
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose errors when cmf_path does not exist", {
  expect_snapshot_error(ems_compose(cmf_path = file.path("not_a_path")))
})

test_that("ems_compose examples run", {
  # All variables and coefficients
  outputs <- ems_compose(cmf_path)
  
  # Specific variables and/or coefficients by name
  outputs <- ems_compose(cmf_path, c("qfd", "EVFP"))
  expect_equal(nrow(outputs), 2)
  expect_equal(ncol(outputs), 4)
  expect_equal(names(outputs), c("name", "label", "type", "dat"))
  expect_equal(names(outputs$dat), c("qfd", "EVFP"))
})

test_that("ems_compose errors when model run has not taken place", {
  unlink(file.path(write_dir, "out"), recursive = TRUE)
  expect_snapshot_error(ems_compose(cmf_path),
                        variant = variant)
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)