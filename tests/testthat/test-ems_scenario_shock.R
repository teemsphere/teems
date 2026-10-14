skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "scenario_shock")
temp_dir <- file.path(write_dir, "tmp")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(temp_dir, recursive = TRUE)

ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model <- "GTAP-RE"
model_files <- ems_example(model, write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

model <- ems_model(model_file, closure_file)
REGr <- c("usa", "chn", "row")
Year <- c(2023, 2024, 2025)
df_input <- expand.grid(REGr = REGr, Year = Year, stringsAsFactors = FALSE)
df_input$Value <- runif(nrow(df_input), min = 1e6, max = 1e9)

csv_path <- file.path(temp_dir, "pop_scenario.csv")
write.csv(df_input, csv_path, row.names = FALSE)

test_that("ems_scenario_shock errors when var is missing", {
  expect_snapshot_error(ems_scenario_shock())
})

test_that("ems_scenario_shock errors when input is missing", {
  expect_snapshot_error(ems_scenario_shock("pop"))
})

test_that("ems_scenario_shock errors when var is not character", {
  expect_snapshot_error(ems_scenario_shock( 1,  df_input))
})

test_that("ems_scenario_shock errors when input is numeric", {
  expect_snapshot_error(ems_scenario_shock( "pop",  42))
})

test_that("ems_scenario_shock errors when input data frame lacks Value column", {
  no_val <- df_input
  no_val$Value <- NULL
  expect_snapshot_error(ems_scenario_shock("pop", no_val))
})

test_that("ems_scenario_shock accepts data frame input", {
  result <- ems_scenario_shock("pop", df_input)
  expect_type(result, "list")
})

test_that("ems_scenario_shock result inherits 'scenario' class", {
  result <- ems_scenario_shock("pop", df_input)
  expect_true(inherits(result[[1]], "scenario"))
})

test_that("ems_scenario_shock result carries call attribute", {
  result <- ems_scenario_shock("pop", df_input)
  expect_false(is.null(attr(result[[1]], "call")))
})

test_that("ems_scenario_shock result carries var attribute", {
  result <- ems_scenario_shock("pop", df_input)
  expect_equal(result[[1]]$var, "pop")
})

test_that("ems_scenario_shock accepts CSV path input", {
  result <- ems_scenario_shock("pop", csv_path)
  expect_type(result, "list")
  expect_true(inherits(result[[1]], "scenario"))
})

test_that("ems_scenario_shock errors when no year df provided", {
  no_year <- data.frame(Set1 = "a", Value = 2)
  expect_snapshot_error(ems_scenario_shock("pop", no_year))
})

test_that("ems_scenario_shock errors when used with a static model", {
  static_dat <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  static_model_files <- ems_example("GTAPv7", write_dir)
  static_model_file <- static_model_files[["model_file"]]
  static_closure_file <- static_model_files[["closure_file"]]
  static_model <- ems_model(static_model_file, static_closure_file)
  shock <- data.frame(REGr = "a", Year = 2017, Value = 2)
  shock <- ems_scenario_shock("pop", shock)
  nest_temp("scen_static", write_dir)
  expect_snapshot_error(ems_deploy(static_dat, static_model, shock))
})

test_that("ems_scenario_shock errors when not all preaggregation tuples provided", {
  year <- 2023
  time_steps <- year + c(0, 1, 2)
  dat <- ems_data(
    dat_input = Sys.getenv("GTAP12_dat"),
    par_input = Sys.getenv("GTAP12_par"),
    set_input = Sys.getenv("GTAP12_set"),
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = time_steps
  )

  pop <- ems_data(
    dat_input = Sys.getenv("GTAP12_dat"),
    par_input = Sys.getenv("GTAP12_par"),
    set_input = Sys.getenv("GTAP12_set"),
    REG = "full",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
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
  pop_trajectory <- ems_scenario_shock("pop", pop)
  nest_temp("scen_missing_tup", write_dir)
  expect_snapshot_error(ems_deploy(
    .data = dat,
    model = model,
    shock = pop_trajectory
  ))
})

test_that("a factor region column in a scenario shock deploys like the character column", {
  shf <- function(shk, name) {
    nest_temp(name, write_dir)
    cmf <- ems_deploy(dat, model, shk)
    readLines(list.files(dirname(cmf), pattern = "\\.shf$", full.names = TRUE))
  }
  regions <- rev(getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG$REG)
  chr <- expand.grid(REGr = regions, Year = c(2023, 2024, 2025), stringsAsFactors = FALSE)
  chr$Value <- 100 * (1 + match(chr$REGr, regions) / 1000)^(chr$Year - 2023)
  fac <- chr
  fac$REGr <- factor(fac$REGr, levels = regions)
  canon <- shf(ems_scenario_shock("pop", chr), "scen_factor_chr")
  expect_identical(shf(ems_scenario_shock("pop", fac), "scen_factor"), canon)
})

unlink(write_dir, recursive = TRUE)

test_that("a scenario shock with a zero base-year value aborts", {
  regions <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG$REG
  traj <- expand.grid(REGr = regions, Year = c(2023, 2024, 2025), stringsAsFactors = FALSE)
  traj$Value <- 100 * (1 + match(traj$REGr, regions) / 1000)^(traj$Year - 2023)
  traj$Value[traj$Year == 2023] <- 0
  zero_base <- ems_scenario_shock("pop", traj)
  dir.create(write_dir, showWarnings = FALSE, recursive = TRUE)
  nest_temp("scen_zero_base", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, zero_base))
})

test_that("ems_scenario_shock errors when partial is not a single TRUE or FALSE", {
  expect_snapshot_error(ems_scenario_shock("pop", df_input, partial = NA))
  expect_snapshot_error(ems_scenario_shock("pop", df_input, partial = "yes"))
})

partial_traj <- function(regions) {
  traj <- expand.grid(REGr = regions, Year = c(2023, 2024, 2025), stringsAsFactors = FALSE)
  traj$Value <- 100 * (1 + match(traj$REGr, regions) / 100)^(traj$Year - 2023)
  traj
}

test_that("a partial scenario shock covering whole aggregated tuples deploys", {
  dir.create(write_dir, showWarnings = FALSE, recursive = TRUE)
  traj <- partial_traj(c("usa", "chn"))
  nest_temp("scen_partial_none", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, ems_scenario_shock("pop", traj)))
  nest_temp("scen_partial", write_dir)
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_set(verbose = FALSE))
  msgs <- testthat::capture_messages(
    cmf <- ems_deploy(dat, model, ems_scenario_shock("pop", traj, partial = TRUE))
  )
  expect_true(any(grepl("covers 2 of 3 aggregated tuples", msgs, fixed = TRUE)))
  shf <- readLines(list.files(dirname(cmf), pattern = "\\.shf$", full.names = TRUE))
  shf <- paste(shf, collapse = "\n")
  expect_match(shf, "usa", fixed = TRUE)
  expect_match(shf, "chn", fixed = TRUE)
  expect_no_match(shf, "row", fixed = TRUE)
})

test_that("a partial scenario shock reproduces the full shock on the tuples it covers", {
  dir.create(write_dir, showWarnings = FALSE, recursive = TRUE)
  regions <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG$REG
  full <- partial_traj(regions)
  part <- full[full$REGr %in% c("usa", "chn"), ]
  shk_lines <- function(shk, name) {
    nest_temp(name, write_dir)
    cmf <- suppressMessages(ems_deploy(dat, model, shk))
    readLines(list.files(dirname(cmf), pattern = "\\.shf$", full.names = TRUE))
  }
  full_shf <- shk_lines(ems_scenario_shock("pop", full), "scen_full_ref")
  part_shf <- shk_lines(ems_scenario_shock("pop", part, partial = TRUE), "scen_part_ref")
  shk_values <- function(x) {
    v <- suppressWarnings(as.numeric(x))
    v[!is.na(v) & v != 0]
  }
  expect_length(shk_values(part_shf), 4L)
  expect_true(all(shk_values(part_shf) %in% shk_values(full_shf)))
})

test_that("a partial scenario shock covering part of an aggregated tuple aborts", {
  dir.create(write_dir, showWarnings = FALSE, recursive = TRUE)
  regions <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG$REG
  traj <- partial_traj(c("usa", setdiff(regions, c("usa", "chn"))[1:3]))
  nest_temp("scen_partial_part", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, ems_scenario_shock("pop", traj, partial = TRUE)))
})

test_that("a scenario shock tuple outside the pre-aggregation sets aborts", {
  dir.create(write_dir, showWarnings = FALSE, recursive = TRUE)
  regions <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG$REG
  traj <- partial_traj(c(regions, "zzz"))
  nest_temp("scen_extra", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, ems_scenario_shock("pop", traj)))
})

test_that("a scenario shock on a change variable deploys ordinary changes", {
  dir.create(write_dir, showWarnings = FALSE, recursive = TRUE)
  regions <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG$REG
  traj <- expand.grid(REGr = regions, Year = c(2023, 2024, 2025), stringsAsFactors = FALSE)
  traj$Value <- 1000 * match(traj$REGr, regions) + (traj$Year - 2023)
  nest_temp("scen_change_var", write_dir)
  cmf <- ems_deploy(dat, model, ems_scenario_shock("del_tbal", traj),
                    swap_in = "del_tbal", swap_out = "cgdslack")
  shf <- readLines(list.files(dirname(cmf), pattern = "\\.shf$", full.names = TRUE))
  v <- suppressWarnings(as.numeric(unlist(strsplit(shf, "[[:space:]]+"))))
  v <- v[!is.na(v) & v != 0]
  expect_equal(sum(v), 3 * length(regions))
})
