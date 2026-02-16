skip_on_cran()
ems_option_set(verbose = FALSE)

test_that("ems_data requires dat_input argument", {
  expect_snapshot_error(ems_data())
})

test_that("ems_data requires par_input argument", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  expect_snapshot_error(ems_data(dat_input = dat_input))
})

test_that("ems_data requires set_input argument", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input))
})

test_that("ems_data requires REG argument", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input))
})

test_that("ems_data rejects non-character dat_input", {
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = 1,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = "big3",
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg"))
})

test_that("ems_data rejects non-character par_input", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = 1,
                                 set_input = set_input,
                                 REG = "big3",
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg"))
})

test_that("ems_data rejects non-character set_input", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = 1,
                                 REG = "big3",
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg"))
})

test_that("ems_data rejects non-character aux_input", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 aux_input = 1,
                                 REG = "big3",
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg"))
})

test_that("ems_data rejects non-character REG", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = 1,
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg"))
})

test_that("ems_data rejects invalid internal mapping name", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = "not_an_internal_mapping"))
})

test_that("ems_data rejects non-existent CSV file", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = "not_a_file.csv"))
})

test_that("ems_data rejects wrong file extension for mapping", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  tmp_txt <- tempfile(fileext = ".txt")
  file.create(tmp_txt)
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = tmp_txt))
})

test_that("ems_data rejects invalid mapping values in CSV", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  REG <- mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  REG[1, ] <- "invalid"
  if (!dir.exists(R_user_dir("teems", "cache"))) {
    dir.create(R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = REG_csv))
})

test_that("ems_data rejects CSV with extra columns", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  REG <- mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  REG$extra_col <- NA
  if (!dir.exists(R_user_dir("teems", "cache"))) {
    dir.create(R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_warning(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = REG_csv))
})

test_that("ems_data rejects CSV with insufficient columns", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  REG <- mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  REG <- REG[, 1]
  if (!dir.exists(R_user_dir("teems", "cache"))) {
    dir.create(R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = REG_csv))
})

test_that("ems_data rejects invalid target_format", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = "big3",
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg",
                                 time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16),
                                 target_format = "not_a_format"))
})

test_that("ems_data unnecessary convert", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_warning(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = "big3",
                                 TRAD_COMM = "macro_sector",
                                 ENDW_COMM = "labor_agg",
                                 time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16),
                                 target_format = "GTAPv7"))
})

test_that("ems_data rejects unrecognized set arguments", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 not_a_set = "big3",
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg"))
})

test_that("ems_data rejects unrecognized set arguments with CSV mapping", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  REG <- mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  if (!dir.exists(R_user_dir("teems", "cache"))) {
    dir.create(R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 not_a_set = REG_csv,
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg"))
})

test_that("ems_data rejects duplicate time_steps", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_error(ems_data(dat_input = dat_input,
                                 par_input = par_input,
                                 set_input = set_input,
                                 REG = "big3",
                                 COMM = "macro_sector",
                                 ACTS = "macro_sector",
                                 ENDW = "labor_agg",
                                 time_steps = c(0, 1, 1)))
})

test_that("ems_data warns wrong initial year", {
  dat_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfdat.har"
  par_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfpar.har"
  set_input <- "~/dat/GTAP/v11c/flexAgg11c17/gsdfset.har"
  expect_snapshot_warning(ems_data(dat_input = dat_input,
                                   par_input = par_input,
                                   set_input = set_input,
                                   REG = "big3",
                                   COMM = "macro_sector",
                                   ACTS = "macro_sector",
                                   ENDW = "labor_agg",
                                   time_steps = c(2014, 2015, 2016)))
})