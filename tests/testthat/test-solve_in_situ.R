skip_on_cran()
ems_option_set(verbose = FALSE)

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

year <- 2017

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "in_situ")

if (dir.exists(write_dir)) {
  unlink(list.dirs(write_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(write_dir, recursive = TRUE)
}

model <- "GTAP-RE"

model_files <- ems_example(model, write_dir = write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

time_steps <- c(0, 1, 2, 3)

dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps
)

model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

test_that("solve_in_situ solves", {
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

  pop_shk <- ems_custom_shock(
    var = "pop",
    input = pop
  )

  aoall_shk <- ems_custom_shock(
    var = "aoall",
    input = aoall
  )

  afeall_shk <- ems_custom_shock(
    var = "afeall",
    input = afeall
  )

  atall_shk <- ems_custom_shock(
    var = "atall",
    input = atall
  )

  write_sub_dir <- "custom_full"
  ems_option_set(write_sub_dir = write_sub_dir)
  cmf_path <- ems_deploy(
    write_dir = write_dir,
    .data = dat,
    model = model,
    shock = list(pop_shk, aoall_shk, afeall_shk, atall_shk)
  )

  insitu_dir <- file.path(write_dir, write_sub_dir)
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPPARM <- file.path(insitu_dir, "GTAPPARM.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)

  dir.create(file.path(insitu_dir, "writeout"))
  
  outputs <- solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPPARM = GTAPPARM,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    model_dir = file.path(insitu_dir, "writeout"),
    closure_file = model_files[["closure_file"]],
    shock_file = shock_file,
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

  dir.create(file.path(insitu_dir, "no_writeout"))
  outputs <- solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPPARM = GTAPPARM,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    model_dir = file.path(insitu_dir, "no_writeout"),
    closure_file = model_files[["closure_file"]],
    shock_file = shock_file,
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

test_that("solve_in_situ errors when model directory doesn't exist", {
  pop <- ems_uniform_shock(
    var = "pop",
    value = 1
  )

  write_sub_dir <- "solve_in_situ_missing_file"
  ems_option_set(write_sub_dir = write_sub_dir)

  cmf_path <- ems_deploy(
    write_dir = write_dir,
    .data = dat,
    model = model
  )

  insitu_dir <- file.path(write_dir, write_sub_dir)
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)

  expect_snapshot_error(solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    closure_file = model_files[["closure_file"]],
    model_dir = file.path(insitu_dir, "missing_file"),
    shock_file = shock_file,
    n_tasks = 1,
    n_subintervals = 1,
    matrix_method = "SBBD",
    solution_method = "mod_midpoint",
    writeout = TRUE
  ))
})

test_that("solve_in_situ errors when missing input file", {
  pop <- ems_uniform_shock(
    var = "pop",
    value = 1
  )
  
  write_sub_dir <- "solve_in_situ_missing_file"
  ems_option_set(write_sub_dir = write_sub_dir)
  
  cmf_path <- ems_deploy(
    write_dir = write_dir,
    .data = dat,
    model = model
  )
  
  insitu_dir <- file.path(write_dir, write_sub_dir)
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)
  
  dir.create(file.path(insitu_dir, "missing_file"))
  expect_snapshot_error(solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    closure_file = model_files[["closure_file"]],
    model_dir = file.path(insitu_dir, "missing_file"),
    shock_file = shock_file,
    n_tasks = 1,
    n_subintervals = 1,
    matrix_method = "SBBD",
    solution_method = "mod_midpoint",
    writeout = TRUE
  ))
})