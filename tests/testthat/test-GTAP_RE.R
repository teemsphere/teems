skip_on_cran()
ems_option_set(verbose = FALSE)

data_dir <- "~/src/teems/teems_dat"
data_prefix <- c("gd", "gsd", "gsdf")
data_db <- c("v9", "v10", "v11")

model <- "GTAP_RE"
model_input <- ems_example("GTAP_RE.tab", file.path("models", model))
closure_file <- ems_example("GTAP_RE.cls", file.path("models", model))

for (d in seq_along(data_prefix)) {
  prefix <- data_prefix[d]
  dat_input <- file.path(data_dir, paste0(prefix, "dat.har"))
  par_input <- file.path(data_dir, paste0(prefix, "par.har"))
  set_input <- file.path(data_dir, paste0(prefix, "set.har"))

  if (prefix %in% c("gd", "gsd")) {
    target_format <- "GTAPv7"
  } else {
    target_format <- NULL
  }

  db <- data_db[d]

  year <- switch(db,
    "v9" = 2011,
    "v10" = 2014,
    "v11" = 2017
  )

  write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), db)

  if (!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }

  test_that(paste(db, paste(model, "null scenario")), {
    run_script(file.path(model, "null.R"))
    expect_true(all(unlist(lapply(
      outputs[outputs$type == "variable", ]$dat,
      \(d) {
        all(d$Value == 0)
      }
    ))))
  })

  test_that(paste(db, paste(model, "numeraire")), {
    run_script(file.path(model, "numeraire.R"))
    expect_true(all(outputs$dat$pfactwld$Value == 1))
  })

  test_that(paste(db, paste(model, "full uniform")), {
    run_script(file.path(model, "full_uniform.R"))
    expect_true(all(outputs$dat$pop$Value == 1))
  })

  test_that(paste(db, paste(model, "partial uniform")), {
    run_script(file.path(model, "part_uniform.R"))
    expect_true(all(outputs$dat$aoall[REGr == "chn" & ACTSa == "crops"]$Value == -1))
  })

  test_that(paste(db, paste(model, "partial uniform full swap")), {
    run_script(file.path(model, "part_uniform_full_swap.R"))
    expect_true(all(
      outputs$dat$qfd[REGr == "usa" & ACTSa == "crops"]$Value == -1,
      outputs$dat$qfd[REGr != "usa" & ACTSa != "crops"]$Value == 0,
      outputs$dat$tfd$Value != 0
    ))
  })

  test_that(paste(db, paste(model, "partial uniform part swap")), {
    run_script(file.path(model, "part_uniform_part_swap.R"))
    expect_true(all(
      outputs$dat$qfd[REGr == "usa" & ACTSa == "crops"]$Value == -1,
      outputs$dat$qfd[REGr != "usa" & ACTSa != "crops"]$Value != 0,
      outputs$dat$tfd[REGr == "usa" & ACTSa == "crops"]$Value != 0,
      outputs$dat$tfd[REGr != "usa" & ACTSa != "crops"]$Value == 0
    ))
  })

  test_that(paste(db, paste(model, "partial uniform explicit year")), {
    run_script(file.path(model, "part_uniform_year.R"))
    expect_true(all(
      outputs$dat$aoall[REGr == "chn" & ACTSa == "crops" & Year == year]$Value == -1,
      outputs$dat$aoall[REGr != "chn" & ACTSa != "crops" & Year != year]$Value == 0
    ))
  })

  test_that(paste(db, paste(model, "partial uniform part swap mixed entry")), {
    run_script(file.path(model, "part_uniform_part_swap_mixed.R"))
    expect_true(all(
      outputs$dat$qfd[REGr == "usa" & ACTSa == "crops"]$Value == -1,
      outputs$dat$qfd[REGr != "usa" & ACTSa != "crops"]$Value != 0,
      outputs$dat$tfd[REGr == "usa" & ACTSa == "crops"]$Value != 0,
      outputs$dat$tfd[REGr == "usa" & ACTSa != "crops"]$Value == 0,
      outputs$dat$dppriv$Value != 0,
      dplyr::near(outputs$dat$yp$Value, 0.1)
    ))
  })

  test_that(paste(db, paste(model, "custom full all dim")), {
    run_script(file.path(model, "custom_full.R"))
    expect_true(all(
      all.equal(pop,
        outputs$dat$pop[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(aoall,
        outputs$dat$aoall[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(afeall,
        outputs$dat$afeall[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(atall,
        outputs$dat$atall[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      )
    ))
  })

  test_that(paste(db, paste(model, "custom full csv all dim")), {
    run_script(file.path(model, "custom_full_csv.R"))
    expect_true(all(
      all.equal(pop,
        outputs$dat$pop[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(aoall,
        outputs$dat$aoall[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(afeall,
        outputs$dat$afeall[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(atall,
        outputs$dat$atall[, !"Year"],
        check.attributes = FALSE,
        tolerance = 1e-6
      )
    ))
  })

  test_that(paste(db, paste(model, "custom full all dim year")), {
    run_script(file.path(model, "custom_full_year.R"))
    expect_true(all(
      all.equal(pop,
        outputs$dat$pop,
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(aoall,
        outputs$dat$aoall,
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(afeall,
        outputs$dat$afeall,
        check.attributes = FALSE,
        tolerance = 1e-6
      ),
      all.equal(atall,
        outputs$dat$atall,
        check.attributes = FALSE,
        tolerance = 1e-6
      )
    ))
  })

  test_that(paste(db, paste(model, "scenario")), {
    run_script(file.path(model, "scenario.R"))

    pop$REGr <- ifelse(pop$REGr == "chn",
      "chn",
      ifelse(pop$REGr == "usa",
        "usa",
        "row"
      )
    )

    pop <- aggregate(Value ~ REGr + Year, data = pop, FUN = sum)

    check <- merge(pop,
      outputs$dat$pop[, !"ALLTIMEt"],
      by = colnames(pop)[-ncol(pop)]
    )

    check$check <- ave(check$Value.x, check$REGr, FUN = function(x) {
      base <- x[1]
      ((x - base) / base) * 100
    })

    expect_true(all.equal(check$Value.y,
      check$check,
      tolerance = 1e-6
    ))
  })
}