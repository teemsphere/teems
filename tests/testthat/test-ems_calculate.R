skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "calculate")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]

static_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

nest_temp("calculate_deploy", write_dir)
deployed <- ems_deploy(static_data, quiet_pivot(ems_model(model_file, model_files[["closure_file"]])))
deploy_dir <- dirname(deployed)
inputs <- list(
  GTAPSETS = file.path(deploy_dir, "GTAPSETS.txt"),
  GTAPDATA = file.path(deploy_dir, "GTAPDATA.txt"),
  GTAPPARM = file.path(deploy_dir, "GTAPPARM.txt")
)

test_that("a data program is calculated from its input files", {
  run_dir <- file.path(write_dir, "calculate_program")
  dir.create(run_dir)
  tab <- write_data_program(withr::local_tempdir())
  out <- ems_calculate(tab,
                       GTAPSETS = inputs$GTAPSETS,
                       GTAPDATA = inputs$GTAPDATA,
                       model_dir = run_dir)
  expect_setequal(out$name, c("TOT", "VDFB"))
  expect_true(all(out$type == "coefficient"))
  tot <- out$dat[[which(out$name == "TOT")]]
  vdfb <- as.data.frame(out$dat[[which(out$name == "VDFB")]])
  by_reg <- tapply(vdfb$Value, vdfb$REGr, sum)
  expect_equal(tot$Value, as.vector(by_reg[tot$REGr]), tolerance = 1e-6)
  expect_false(file.exists(file.path(run_dir, "out", "variables", "bin", "sol.bin")))
})

test_that("a full model is calculated without a solve, to its null-shock values", {
  run_dir <- file.path(write_dir, "calculate_model")
  dir.create(run_dir)
  calc <- quiet_pivot(do.call(ems_calculate, c(list(model_file, model_dir = run_dir), inputs)))
  expect_true(all(calc$type == "coefficient"))
  expect_true(any(grepl("-solmed nosim", readLines(file.path(run_dir, "model_exec.txt")))))
  expect_false(file.exists(file.path(run_dir, "out", "variables", "bin", "sol.bin")))
  solved <- ems_solve(deployed, solution_method = "Johansen")
  solved <- solved[solved$type == "coefficient", ]
  expect_false(any(calc$type == "postsim"))
  expect_setequal(calc$name, solved$name)
  for (nm in calc$name) {
    expect_identical(
      calc$dat[[which(calc$name == nm)]]$Value,
      solved$dat[[which(solved$name == nm)]]$Value,
      label = nm
    )
  }
})

test_that("ems_calculate requires its input files", {
  expect_snapshot_error(quiet_pivot(ems_calculate(model_file)))
  expect_snapshot_error(quiet_pivot(ems_calculate(model_file, GTAPSETS = inputs$GTAPSETS, model_dir = write_dir)))
  expect_no_warning(expect_snapshot_error(quiet_pivot(ems_calculate(model_file,
                                                                    GTAPSETS = inputs$GTAPSETS,
                                                                    GTAPDATA = "not_a_file.txt",
                                                                    GTAPPARM = inputs$GTAPPARM,
                                                                    model_dir = write_dir))))
  expect_snapshot_error(ems_calculate(model_file, inputs$GTAPSETS, model_dir = write_dir))
  expect_snapshot_error(ems_calculate(model_file, GTAPSETS = inputs$GTAPSETS,
                                      model_dir = file.path(write_dir, "absent")))
})
