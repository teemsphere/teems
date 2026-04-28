skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset())

dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "uniform_shock")

if (dir.exists(write_dir)) { 
  unlink(list.dirs(write_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(write_dir, recursive = TRUE)
}

model <- "GTAP-RE"
model_files <- ems_example(model, write_dir = write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

# general test data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

# general test model
model <- ems_model(model_file = model_file, closure_file = closure_file)

temp_dir <- withr::local_tempdir()

# minimal data frame for scenario shock input (absolute values with Year column)
REGr <- c("usa", "chn", "row")
Year <- c(2017, 2018, 2019)
df_input <- expand.grid(REGr = REGr, Year = Year, stringsAsFactors = FALSE)
df_input$Value <- runif(nrow(df_input), min = 1e6, max = 1e9)

csv_path <- file.path(temp_dir, "pop_scenario.csv")
write.csv(df_input, csv_path, row.names = FALSE)

# --- error tests ---

test_that("ems_scenario_shock errors when var is missing", {
  expect_snapshot_error(ems_scenario_shock())
})

test_that("ems_scenario_shock errors when input is missing", {
  expect_snapshot_error(ems_scenario_shock(var = "pop"))
})

test_that("ems_scenario_shock errors when var is not character", {
  expect_snapshot_error(ems_scenario_shock(var = 1, input = df_input))
})

test_that("ems_scenario_shock errors when input is numeric", {
  expect_snapshot_error(ems_scenario_shock(var = "pop", input = 42))
})

test_that("ems_scenario_shock errors when input data frame lacks Value column", {
  no_val <- df_input
  no_val$Value <- NULL
  expect_snapshot_error(ems_scenario_shock(var = "pop", input = no_val))
})

# --- acceptance tests: data frame input ---

test_that("ems_scenario_shock accepts data frame input", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_type(result, "list")
})

test_that("ems_scenario_shock result inherits 'scenario' class", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_true(inherits(result[[1]], "scenario"))
})

test_that("ems_scenario_shock result carries call attribute", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_scenario_shock result carries var attribute", {
  result <- ems_scenario_shock(var = "pop", input = df_input)
  expect_equal(result[[1]]$var, "pop")
})

# --- acceptance tests: CSV input ---

test_that("ems_scenario_shock accepts CSV path input", {
  result <- ems_scenario_shock(var = "pop", input = csv_path)
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "scenario"))
})

test_that("ems_scenario_shock errors when no year df provided", {
  no_year <- data.frame(Set1 = "a", Value = 2)

  expect_snapshot_error(ems_scenario_shock(
    var = "pop",
    input = no_year
  ))
})

test_that("ems_scenario_shock errors when used with a static model", {
  static <- data.frame(Set1 = "a", Year = 2017, Value = 2)

  static <- ems_scenario_shock(
    var = "pop",
    input = static
  )

  ems_option_set(write_sub_dir = "scen_static")
  expect_snapshot_error(ems_deploy(
    model = model,
    .data = dat,
    shock = static,
    write_dir = write_dir
  ))
})

test_that("ems_scenario_shock errors when not all preaggregation tuples provided", {
  year <- 2017
  time_steps <- year + c(0, 1, 2)
  chron_data <- ems_data(
    dat_input = Sys.getenv("GTAP11c_dat"),
    par_input = Sys.getenv("GTAP11c_par"),
    set_input = Sys.getenv("GTAP11c_set"),
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = time_steps
  )

  pop <- ems_data(
    dat_input = Sys.getenv("GTAP11c_dat"),
    par_input = Sys.getenv("GTAP11c_par"),
    set_input = Sys.getenv("GTAP11c_set"),
    REG = "full",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
  )$POP

  pop$Year <- year
  regions <- unique(pop$REG)
  pop_traj <- expand.grid(
    REG = regions,
    Value = 0,
    Year = tail(time_steps, -1),
    stringsAsFactors = FALSE
  )

  pop <- rbind(pop, pop_traj)
  growth_rates <- data.frame(
    REG = regions,
    growth_rate = runif(length(regions), min = -0.01, max = 0.05)
  )

  pop <- merge(pop, growth_rates, by = "REG")
  base_values <- pop[pop$Year == year, c("REG", "Value")]
  names(base_values)[2] <- "base_value"
  pop <- merge(pop, base_values, by = "REG")

  pop$Value[pop$Year > year] <-
    pop$base_value[pop$Year > year] *
      (1 + pop$growth_rate[pop$Year > year])^(pop$Year[pop$Year > year] - year)

  pop$growth_rate <- NULL
  pop$base_value <- NULL
  pop <- pop[order(pop$REG, pop$Year), ]
  pop <- pop[, c("REG", "Year", "Value")]
  colnames(pop)[1] <- "REGr"

  pop <- tail(pop, -1)
  pop_trajectory <- ems_scenario_shock(
    var = "pop",
    input = pop
  )

  ems_option_set(write_sub_dir = "scen_missing_tup")
  expect_snapshot_error(ems_deploy(
    .data = chron_data,
    model = model,
    shock = pop_trajectory,
    write_dir = write_dir
  ))
})