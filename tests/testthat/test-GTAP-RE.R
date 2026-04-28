skip_on_cran()
ems_option_set(verbose = FALSE)

data_db <- c("v9", "v10", "v11", "v12")
db_inputs <- list(
  v9  = list(dat = Sys.getenv("GTAP9_dat"),   par = Sys.getenv("GTAP9_par"),   set = Sys.getenv("GTAP9_set")),
  v10 = list(dat = Sys.getenv("GTAP10A_dat"),  par = Sys.getenv("GTAP10A_par"),  set = Sys.getenv("GTAP10A_set")),
  v11 = list(dat = Sys.getenv("GTAP11c_dat"),  par = Sys.getenv("GTAP11c_par"),  set = Sys.getenv("GTAP11c_set")),
  v12 = list(dat = Sys.getenv("GTAP12_dat"),  par = Sys.getenv("GTAP12_par"),  set = Sys.getenv("GTAP12_set"))
)

model <- "GTAP-RE"
model_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"))
model_files <- ems_example(model, write_dir = model_dir)
model_files <- ems_example(model)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

for (db in data_db) {
  dat_input <- db_inputs[[db]]$dat
  par_input <- db_inputs[[db]]$par
  set_input <- db_inputs[[db]]$set

  if (db %in% c("v9", "v10")) {
    target_format <- "GTAPv7"
  } else {
    target_format <- NULL
  }

  year <- switch(db,
    "v9" = 2011,
    "v10" = 2014,
    "v11" = 2017,
    "v12" = 2023
  )

  write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), db, model)

  if (dir.exists(write_dir)) { 
    unlink(list.dirs(write_dir, recursive = FALSE), recursive = TRUE)
  } else {
    dir.create(write_dir, recursive = TRUE)
  }

  test_that(paste(db, paste(model, "null scenario")), {
    run_script(file.path(model, "null.R"))
    expect_true(check)
    expect_true(var_check)
    expect_true(coeff_check)
  })

  test_that(paste(db, paste(model, "numeraire")), {
    run_script(file.path(model, "numeraire.R"))
    expect_true(check)
  })

  test_that(paste(db, paste(model, "full uniform")), {
    run_script(file.path(model, "full_uniform.R"))
    expect_true(check)
  })

  test_that(paste(db, paste(model, "partial uniform")), {
    run_script(file.path(model, "part_uniform.R"))
    expect_true(check)
  })

  test_that(paste(db, paste(model, "partial uniform full swap")), {
    run_script(file.path(model, "part_uniform_full_swap.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform part swap")), {
    run_script(file.path(model, "part_uniform_part_swap.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform explicit year")), {
    run_script(file.path(model, "part_uniform_year.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform part swap mixed entry")), {
    run_script(file.path(model, "part_uniform_part_swap_mixed.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom partial all dim")), {
    run_script(file.path(model, "custom_partial.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom partial 2d 1fixed")), {
    run_script(file.path(model, "custom_partial_2d_1fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 3d 1fixed")), {
    run_script(file.path(model, "custom_partial_3d_1fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 3d 2fixed")), {
    run_script(file.path(model, "custom_partial_3d_2fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 4d 1fixed")), {
    run_script(file.path(model, "custom_partial_4d_1fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 4d 2fixed")), {
    run_script(file.path(model, "custom_partial_4d_2fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 4d 3fixed")), {
    run_script(file.path(model, "custom_partial_4d_3fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d 1fixed")), {
    run_script(file.path(model, "custom_partial_5d_1fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d 2fixed")), {
    run_script(file.path(model, "custom_partial_5d_2fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d 3fixed")), {
    run_script(file.path(model, "custom_partial_5d_3fixed.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d k4")), {
    services <- switch(db,
      "v9" = c(
        "atp", "food", "crops", "cmn", "cns", "mnfcs",
        "livestock", "dwe", "frs", "isr", "obs", "ofi",
        "osg", "otp", "ros", "trd", "wtp", "wtr"
      ),
      "v10" = c(
        "afs", "atp", "cmn", "cns", "crops", "dwe", "edu", "food", "hht", "ins",
        "livestock", "mnfcs", "obs", "ofi", "osg", "otp", "ros", "rsa", "trd",
        "whs", "wtp", "wtr"
      ),
      "v11" = c(
        "afs", "atp", "cmn", "cns", "crops", "dwe", "edu", "food", "hht", "ins",
        "livestock", "mnfcs", "obs", "ofi", "osg", "otp", "ros", "rsa", "trd",
        "whs", "wtp", "wtr"
      )
    )
    run_script(file.path(model, "custom_partial_5d_k4.R"))
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom full all dim")), {
    run_script(file.path(model, "custom_full.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom full csv all dim")), {
    run_script(file.path(model, "custom_full_csv.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom full all dim year")), {
    run_script(file.path(model, "custom_full_year.R"))
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "scenario")), {
    run_script(file.path(model, "scenario.R"))
    expect_true(check)
  })
}
