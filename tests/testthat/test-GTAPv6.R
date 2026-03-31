skip_on_cran()
ems_option_set(verbose = FALSE)

data_dir <- "~/src/teems/teems_dat"
data_prefix <- c("gd", "gsd", "gsdf")
data_db <- c("v9", "v10", "v11")

model <- "GTAPv6"
model_files <- ems_example(model)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

for (d in seq_along(data_prefix)) {
  prefix <- data_prefix[d]
  dat_input <- file.path(data_dir, paste0(prefix, "dat.har"))
  par_input <- file.path(data_dir, paste0(prefix, "par.har"))
  set_input <- file.path(data_dir, paste0(prefix, "set.har"))

  if (prefix == "gsdf") {
    target_format <- "GTAPv6"
  } else {
    target_format <- NULL
  }

  db <- data_db[d]

  year <- switch(db,
    "v9" = 2011,
    "v10" = 2014,
    "v11" = 2017
  )

  write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), db, model)

  unlink(file.path(write_dir, "*"), expand = TRUE)

  if (!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }

  test_that(paste(db, paste(model, "null")), {
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

  test_that(paste(db, paste(model, "custom partial 4d k3")), {
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
    run_script(file.path(model, "custom_partial_4d_k3.R"))
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

}
