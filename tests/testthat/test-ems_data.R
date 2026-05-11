skip_on_cran()
ems_option_set(verbose = FALSE)

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")
REG <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG[, c(1, 2)]

test_that("ems_data requires dat_input argument", {
  expect_snapshot_error(ems_data())
})

test_that("ems_data requires par_input argument", {
  expect_snapshot_error(ems_data(dat_input))
})

test_that("ems_data requires set_input argument", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input
  ))
})

test_that("ems_data requires REG argument", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input
  ))
})

test_that("ems_data rejects non-character dat_input", {
  expect_snapshot_error(ems_data(
    dat_input = 1,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects non-character par_input", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input = 1,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects non-character set_input", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input = 1,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects non-character REG", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = 1,
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects invalid internal mapping name", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "not_an_internal_mapping"
  ))
})

test_that("ems_data rejects non-existent CSV file", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "not_a_file.csv"
  ))
})

test_that("ems_data rejects wrong file extension for mapping", {
  tmp_txt <- tempfile(fileext = ".txt")
  file.create(tmp_txt)
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = tmp_txt
  ))
})

test_that("ems_data rejects invalid mapping values in CSV", {
  REG[1, ] <- "invalid"
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = REG_csv
  ))
})

test_that("ems_data warns CSV with extra columns", {
  REG$extra_col <- NA
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_warning(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = REG_csv
  ))
})

test_that("ems_data rejects CSV with insufficient columns", {
  REG <- REG[, 1]
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = REG_csv
  ))
})

test_that("ems_data rejects unrecognized set arguments", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    not_a_set = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects unrecognized set arguments with CSV mapping", {
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    not_a_set = REG_csv,
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects duplicate time_steps", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 1)
  ))
})

test_that("ems_data warns wrong initial year", {
  expect_snapshot_warning(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(2014, 2015, 2016)
  ))
})

test_that("ems_data errors when dots passed without names", {
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    "big3",
    "macro_sector",
    "labor_agg",
    time_steps = c(2014, 2015, 2016)
  ))
})

test_that("ems_data examples work", {
  # Static model
  v7_data <- ems_data(dat_input,
    par_input,
    set_input,
    REG = "AR5",
    ACTS = "food",
    ENDW = "labor_diff"
  )

  check <- attr(v7_data, "metadata")$data_format == "GTAPv7"
  expect_true(check)
  # Intertemporal model (explicit time steps)
  int_data <- ems_data(Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set"),
    REG = "WB23",
    PROD_COMM = "services",
    ENDW_COMM = "labor_agg",
    time_steps = c(0, 1, 2, 4, 6, 8, 10, 15)
  )

  check <- attr(int_data, "metadata")$data_format == "GTAPv6"
  expect_true(check)
  # Intertemporal model (chronological time steps)
  int_data <- ems_data(dat_input,
    par_input,
    set_input,
    REG = "R32",
    ACTS = "medium",
    ENDW = "labor_diff",
    time_steps = c(2023, 2025, 2027, 2030, 2035)
  )
  check <- attr(int_data, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})