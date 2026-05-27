skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "custom_shock")
temp_dir <- file.path(write_dir, "tmp")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(temp_dir, recursive = TRUE)
ems_option_set(
  verbose = FALSE,
  tempdir = write_dir
)
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
REGr <- c("asia", "eit", "lam")
ACTSa <- c("svces", "food", "crops")
df_input <- expand.grid(ACTSa = ACTSa, REGr = REGr, stringsAsFactors = FALSE)
df_input$Value <- runif(nrow(df_input))

csv_path <- file.path(temp_dir, "aoall_shock.csv")
write.csv(df_input, csv_path, row.names = FALSE)
test_that("ems_custom_shock errors when var is missing", {
  expect_snapshot_error(ems_custom_shock())
})

test_that("ems_custom_shock errors when input is missing", {
  expect_snapshot_error(ems_custom_shock("aoall"))
})

test_that("ems_custom_shock errors when var is not character", {
  expect_snapshot_error(ems_custom_shock(1, df_input))
})

test_that("ems_custom_shock errors when input is numeric", {
  expect_snapshot_error(ems_custom_shock("aoall", 1))
})

test_that("ems_custom_shock errors when input data frame lacks Value column", {
  no_val <- df_input
  no_val$Value <- NULL
  expect_snapshot_error(ems_custom_shock("aoall", no_val))
})

test_that("ems_custom_shock accepts data frame input", {
  result <- ems_custom_shock("aoall", df_input)
  expect_type(result, "list")
})

test_that("ems_custom_shock result inherits 'custom' class", {
  result <- ems_custom_shock("aoall", df_input)
  expect_true(inherits(result[[1]], "custom"))
})

test_that("ems_custom_shock result carries call attribute", {
  result <- ems_custom_shock("aoall", df_input)
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_custom_shock result carries var attribute", {
  result <- ems_custom_shock("aoall", df_input)
  expect_equal(result[[1]]$var, "aoall")
})

test_that("ems_custom_shock accepts CSV path input", {
  result <- ems_custom_shock("aoall", csv_path)
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "custom"))
})

test_that("ems_custom_shock errors when both year and int set are provided", {
  extra_col <- data.frame(REGr = "row", Value = 2, ALLTIMEt = 0, Year = 2017)
  extra_col <- ems_custom_shock("pop", extra_col)
  nest_temp("cust_extra_col", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, extra_col))
})

test_that("ems_custom_shock errors when invalid year is provided", {
  invalid_year <- data.frame(REGr = "row", Value = 2, Year = 2016)
  invalid_year <- ems_custom_shock("pop", invalid_year)
  nest_temp("cust_invalid_year", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_year))
})

test_that("ems_custom_shock errors when input is missing sets", {
  missing_set <- data.frame(REGr = "not_an_ele", Value = 2)
  missing_set <- ems_custom_shock("pop", missing_set)
  nest_temp("cust_missing_set", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, missing_set))
})

test_that("ems_custom_shock errors when invalid tuple is provided", {
  invalid_tup <- data.frame(REGr = "not_an_ele", ALLTIMEt = 0, Value = 2)
  invalid_tup <- ems_custom_shock("pop", invalid_tup)
  nest_temp("cust_invalid_tup", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_tup))
})

test_that("ems_custom_shock errors when some shock tuples are endogenous", {
  invalid_tup <- data.frame(ENDWMSe = "capital", REGr = "row", ALLTIMEt = c(0, 1), Value = 2)
  invalid_tup <- ems_custom_shock("qe", invalid_tup)
  nest_temp("cust_endo_tup", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, invalid_tup))
})

test_that("ems_custom_shock example runs", {
  model <- "GTAP-RE"
  model_files <- ems_example(write_dir, model)
  model_file <- model_files[["model_file"]]
  closure_file <- model_files[["closure_file"]]
  dat <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "AR5",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = 0:7
  )
  
  model <- ems_model(model_file, closure_file)
  
  sectors <- c("crops", "food", "livestock", "mnfcs", "svces")
  regions <- c("asia", "eit", "lam", "maf", "oecd")
  time_steps <- 0:7

  # Create data frame
  aoall_data <- expand.grid(ACTSa = sectors,
                       REGr = regions,
                       ALLTIMEt = time_steps)

  
  aoall_data$Value <- runif(nrow(aoall_data))
  head(aoall_data)

  # Assign values to a model variable
  aoall_shk <- ems_custom_shock("aoall", aoall_data)
  
  # In the case of temporally dynamic models, chronological
  # year(s) may be used instead of the the time set.
  
  pop_data <- data.frame(REGr = "asia",
                         Year = 2023:2030)

  pop_data$Value <- runif(nrow(pop_data))
  pop_data
  
  # Assign values to a model variable (pop)
  pop_shk <- ems_custom_shock("pop", pop_data)
  
  cmf_path <- ems_deploy(dat, model, shock = list(aoall_shk, pop_shk))
  outputs <- ems_solve(cmf_path)
  
  aoall_data <- aoall_data[do.call(order, aoall_data), ]
  aoall_data$ACTSa <- as.character(aoall_data$ACTSa)
  aoall_data$REGr <- as.character(aoall_data$REGr)
  pop_data <- pop_data[do.call(order, pop_data), ]
  pop_data$REGr <- as.character(pop_data$REGr)
  
  check1 <- all.equal(aoall_data,
                     outputs$dat$aoall[, !"Year"],
                     check.attributes = FALSE,
                     check.class = FALSE,
                     tolerance = 1e-7)
  
  pop_data <- pop_data[,match(colnames(outputs$dat$pop[, !"ALLTIMEt"]), colnames(pop_data))]
  check2 <- all.equal(pop_data,
                      outputs$dat$pop[REGr == "asia", !"ALLTIMEt"],
                      check.attributes = FALSE,
                      check.class = FALSE,
                      tolerance = 1e-7)

  expect_all_true(c(check1, check2))
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)