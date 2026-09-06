skip_on_cran()

# Every example script under inst/scripts is solved once per model and
# database: the GTAPv7-format models (GTAPv7, GTAP-RE) on v11 and v12
# natively and on v10 converted up; GTAPv6 on v10 natively and on v11
# and v12 converted down. GTAP-INT is exercised by test-ems_example.R.
ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "scripts")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(write_dir, recursive = TRUE)

db_inputs <- list(
  v10 = list(
    dat = Sys.getenv("GTAP10A_dat"), par = Sys.getenv("GTAP10A_par"),
    set = Sys.getenv("GTAP10A_set"), format = "GTAPv6", year = 2014
  ),
  v11 = list(
    dat = Sys.getenv("GTAP11c_dat"), par = Sys.getenv("GTAP11c_par"),
    set = Sys.getenv("GTAP11c_set"), format = "GTAPv7", year = 2017
  ),
  v12 = list(
    dat = Sys.getenv("GTAP12_dat"), par = Sys.getenv("GTAP12_par"),
    set = Sys.getenv("GTAP12_set"), format = "GTAPv7", year = 2023
  )
)

model_format <- c(GTAPv6 = "GTAPv6", GTAPv7 = "GTAPv7", "GTAP-RE" = "GTAPv7")

# the COMM elements of the "services" ACTS mapping, read by the k3/k4
# scripts
services <- c(
  "afs", "atp", "cmn", "cns", "crops", "dwe", "edu", "food", "hht", "ins",
  "livestock", "mnfcs", "obs", "ofi", "osg", "otp", "ros", "rsa", "trd",
  "whs", "wtp", "wtr"
)

for (model in names(model_format)) {
  model_dir <- file.path(write_dir, model)
  dir.create(model_dir)
  model_files <- ems_example(model, model_dir)
  scripts <- list.files(
    system.file("scripts", model, package = "teems"),
    pattern = "\\.R$"
  )

  for (db in names(db_inputs)) {
    inputs <- db_inputs[[db]]

    if (inputs$format != model_format[[model]]) {
      converted <- GTAP_convert(
        inputs$dat, inputs$par, inputs$set,
        target = model_format[[model]]
      )
      inputs$dat <- converted$dat
      inputs$par <- converted$par
      inputs$set <- converted$set
    }

    db_dir <- file.path(model_dir, db)
    dir.create(db_dir)

    for (script in scripts) {
      test_that(paste(model, db, tools::file_path_sans_ext(script)), {
        checks <- run_script(
          model = model,
          name = script,
          inputs = inputs,
          model_files = model_files,
          write_dir = db_dir,
          services = services
        )
        expect_true(length(checks) > 0L)
        expect_all_true(checks)
      })
    }
  }
}

unlink(write_dir, recursive = TRUE)
