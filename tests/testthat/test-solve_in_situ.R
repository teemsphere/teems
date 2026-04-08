skip_on_cran()
ems_option_set(verbose = FALSE)

data_db <- c("v9", "v10", "v11")
db_inputs <- list(
  v9  = list(dat = Sys.getenv("GTAP9_dat"),   par = Sys.getenv("GTAP9_par"),   set = Sys.getenv("GTAP9_set")),
  v10 = list(dat = Sys.getenv("GTAP10A_dat"),  par = Sys.getenv("GTAP10A_par"),  set = Sys.getenv("GTAP10A_set")),
  v11 = list(dat = Sys.getenv("GTAP11c_dat"),  par = Sys.getenv("GTAP11c_par"),  set = Sys.getenv("GTAP11c_set"))
)

model <- "GTAP-RE"
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
    "v11" = 2017
  )

  write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), db, model)

  unlink(file.path(write_dir, "*"), expand = TRUE)

  if (!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }


  test_that(paste(db, paste(model, "solve_in_situ")), {
    time_steps <- c(0, 1, 2, 3)

    # load GTAP HAR files, apply set mappings, and aggregate data
    .data <- ems_data(
      dat_input = dat_input,
      par_input = par_input,
      set_input = set_input,
      REG = "big3",
      COMM = "macro_sector",
      ACTS = "macro_sector",
      ENDW = "labor_agg",
      time_steps = time_steps,
      target_format = target_format
    )

    # parse the model Tablo file and load the closure
    model <- ems_model(
      model_file = model_file,
      closure_file = closure_file
    )

    REG <- c("chn", "usa", "row")
    ENDW <- c("labor", "capital", "natlres", "land")
    COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
    ACTS <- COMM
    MARG <- "svces"
    ALLTIME <- seq(0, length(time_steps) - 1)


    # 2D
    pop <- expand.grid(
      REGr = REG,
      ALLTIMEt = ALLTIME,
      stringsAsFactors = FALSE
    )

    pop <- pop[do.call(order, pop), ]
    pop$Value <- runif(nrow(pop))

    # 3D
    aoall <- expand.grid(
      ACTSa = ACTS,
      REGr = REG,
      ALLTIMEt = ALLTIME,
      stringsAsFactors = FALSE
    )

    aoall <- aoall[do.call(order, aoall), ]
    aoall$Value <- runif(nrow(aoall))

    # 4D
    afeall <- expand.grid(
      ENDWe = ENDW,
      ACTSa = ACTS,
      REGr = REG,
      ALLTIMEt = ALLTIME,
      stringsAsFactors = FALSE
    )

    afeall <- afeall[do.call(order, afeall), ]
    afeall$Value <- runif(nrow(afeall))

    # 5D
    atall <- expand.grid(
      MARGm = MARG,
      COMMc = COMM,
      REGs = REG,
      REGd = REG,
      ALLTIMEt = ALLTIME,
      stringsAsFactors = FALSE
    )

    atall <- atall[do.call(order, atall), ]
    atall$Value <- runif(nrow(atall))

    # define a custom percentage change shock over all pop elements
    pop_shk <- ems_custom_shock(
      var = "pop",
      input = pop
    )

    # define a custom percentage change shock over all aoall elements
    aoall_shk <- ems_custom_shock(
      var = "aoall",
      input = aoall
    )

    # define a custom percentage change shock over all afeall elements
    afeall_shk <- ems_custom_shock(
      var = "afeall",
      input = afeall
    )

    # define a custom percentage change shock over all atall elements
    atall_shk <- ems_custom_shock(
      var = "atall",
      input = atall
    )

    # set the output subdirectory name within write_dir
    write_sub_dir <- "solve_in_situ_WRITEOUT"
    ems_option_set(write_sub_dir = write_sub_dir)

    # validate inputs, write solver files, and return the CMF path
    cmf_path <- ems_deploy(
      write_dir = write_dir,
      .data = .data,
      model = model,
      shock = list(pop_shk, aoall_shk, afeall_shk, atall_shk)
    )

    GTAPDATA <- file.path(write_dir, write_sub_dir, "GTAPDATA.txt")
    GTAPINT <- file.path(write_dir, write_sub_dir, "GTAPINT.txt")
    GTAPPARM <- file.path(write_dir, write_sub_dir, "GTAPPARM.txt")
    GTAPSETS <- file.path(write_dir, write_sub_dir, "GTAPSETS.txt")
    shock_file <- list.files(file.path(write_dir, write_sub_dir), pattern = "shf", full.names = TRUE)

    outputs <- solve_in_situ(
      GTAPDATA = GTAPDATA,
      GTAPINT = GTAPINT,
      GTAPPARM = GTAPPARM,
      GTAPSETS = GTAPSETS,
      model_file = model_files[["model_file"]],
      closure_file = model_files[["closure_file"]],
      shock_file = shock_file,
      write_dir = "~/Documents/test/in_situ_test/",
      n_tasks = 1,
      n_subintervals = 1,
      matrix_method = "SBBD",
      solution_method = "mod_midpoint",
      writeout = TRUE
    )

    expect_s3_class(outputs, "tbl")

    # checks
    pop_check <- isTRUE(all.equal(pop,
      outputs$dat$pop,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    aoall_check <- isTRUE(all.equal(aoall,
      outputs$dat$aoall,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    afeall_check <- isTRUE(all.equal(afeall,
      outputs$dat$afeall,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    atall_check <- isTRUE(all.equal(atall,
      outputs$dat$atall,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    checks <- c(pop_check, aoall_check, afeall_check, atall_check)
    expect_all_true(checks)

    outputs <- solve_in_situ(
      GTAPDATA = GTAPDATA,
      GTAPINT = GTAPINT,
      GTAPPARM = GTAPPARM,
      GTAPSETS = GTAPSETS,
      model_file = model_files[["model_file"]],
      closure_file = model_files[["closure_file"]],
      shock_file = shock_file,
      write_dir = "~/Documents/test/in_situ_test/",
      n_tasks = 1,
      n_subintervals = 1,
      matrix_method = "SBBD",
      solution_method = "mod_midpoint",
      writeout = FALSE
    )

    expect_type(outputs, "list")

    # checks
    pop_check <- isTRUE(all.equal(pop,
      outputs$pop,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    aoall_check <- isTRUE(all.equal(aoall,
      outputs$aoall,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    afeall_check <- isTRUE(all.equal(afeall,
      outputs$afeall,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    atall_check <- isTRUE(all.equal(atall,
      outputs$atall,
      check.attributes = FALSE,
      tolerance = 1e-6
    ))

    checks <- c(pop_check, aoall_check, afeall_check, atall_check)
    expect_all_true(checks)
  })
}