skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")
REG <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG[, c(1, 2)]

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "custom_shock")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(write_dir, recursive = TRUE)
ems_option_set(
  verbose = FALSE,
  tempdir = write_dir
)
withr::defer(ems_option_reset(), teardown_env())

REG_csv <- file.path(write_dir, "REG.csv")
tmp_txt <- file.path(write_dir, "wrong_ext.txt")
file.create(tmp_txt)

test_that("ems_data requires dat_input argument", {
  expect_snapshot_error(ems_data())
})

test_that("ems_data requires par_input argument", {
  expect_snapshot_error(ems_data(dat_input))
})

test_that("ems_data requires set_input argument", {
  expect_snapshot_error(ems_data(dat_input, par_input))
})

test_that("ems_data requires REG argument", {
  expect_snapshot_error(ems_data(dat_input, par_input, set_input))
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
  expect_snapshot_error(ems_data(
    dat_input,
    par_input,
    set_input,
    REG = tmp_txt
  ))
})

test_that("ems_data rejects invalid mapping values in CSV", {
  REG[1, ] <- "invalid"
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

full_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "full",
  ACTS = "full",
  ENDW = "full"
)

agg_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

test_that("aggregation conserves every data header total", {
  # value headers (class "dat") are sums over the aggregated elements,
  # so their totals are invariant to the mapping; parameters are
  # weighted and sets relabelled, neither is a sum
  dat_headers <- names(agg_data)[vapply(agg_data, inherits, TRUE, "dat")]
  expect_true(length(dat_headers) > 20L)
  expect_setequal(dat_headers, names(full_data)[vapply(full_data, inherits, TRUE, "dat")])
  # 1e-6: HAR values are single precision, so the two summation orders
  # differ by float32 accumulation (XTRV measures 2.4e-8)
  totals <- vapply(dat_headers, function(h) {
    isTRUE(all.equal(
      sum(full_data[[h]]$Value),
      sum(agg_data[[h]]$Value),
      tolerance = 1e-6
    ))
  }, TRUE)
  expect_all_true(totals)
})

test_that("aggregated headers carry the mapped elements and the raw ones dropped out", {
  vdfb <- agg_data[["VDFB"]]
  expect_setequal(unique(vdfb$REG), c("chn", "usa", "row"))
  expect_setequal(
    unique(vdfb$ACTS),
    c("crops", "livestock", "food", "mnfcs", "svces")
  )
  expect_false(any(unique(full_data[["VDFB"]]$REG) %in% c("row")))
  # the aggregated table is dense over its set columns
  expect_identical(nrow(vdfb), 5L * 5L * 3L)
  expect_false(anyNA(vdfb$Value))
})

test_that("a CSV mapping reproduces the internal mapping it was written from", {
  big3 <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$REG[, c("REG", "big3")]
  write.csv(big3, REG_csv, row.names = FALSE)
  csv_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = REG_csv,
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  expect_identical(names(csv_data), names(agg_data))
  same <- purrr::map2_lgl(csv_data, agg_data, function(a, b) {
    isTRUE(all.equal(a, b, check.attributes = FALSE))
  })
  expect_all_true(same)
})

test_that("subsetting an ems_data object keeps its class and metadata", {
  sub <- agg_data[c("VDFB", "EVFB")]
  expect_s3_class(sub, "ems_data")
  expect_identical(names(sub), c("VDFB", "EVFB"))
  expect_identical(attr(sub, "metadata"), attr(agg_data, "metadata"))
  expect_identical(sub[["VDFB"]], agg_data[["VDFB"]])
})

test_that("ems_data examples work", {
  # The following examples require input data. See
  # https://teemsphere.github.io/ to get started.

  # Data for a static model using internal mappings
  v7_data <- ems_data(Sys.getenv("GTAP12_dat"),
                      Sys.getenv("GTAP12_par"),
                      Sys.getenv("GTAP12_set"),
                      REG = "AR5",
                      ACTS = "food",
                      ENDW = "labor_diff")

  check <- attr(v7_data, "metadata")$data_format == "GTAPv7"
  expect_true(check)
  # Data for an intertemporal model with explicit time steps
  int_data <- ems_data(Sys.getenv("GTAP10A_dat"),
                       Sys.getenv("GTAP10A_par"),
                       Sys.getenv("GTAP10A_set"),
                       REG = "WB23",
                       PROD_COMM = "services",
                       ENDW_COMM = "labor_agg",
                       time_steps = c(0, 1, 2, 4, 6, 8, 10, 15))

  check <- attr(int_data, "metadata")$data_format == "GTAPv6"
  expect_true(check)
  # Data for an intertemporal model with chronological time
  # steps and a user-provided mapping for the ENDW set
  int_data <- ems_data(Sys.getenv("GTAP12_dat"),
                       Sys.getenv("GTAP12_par"),
                       Sys.getenv("GTAP12_set"),
                       REG = "R32",
                       ACTS = "medium",
                       ENDW = "labor_agg",
                       time_steps = c(2023, 2025, 2027, 2030, 2035))
  check <- attr(int_data, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})

test_that("ems_data prepares a GTAP-AEZ database in place", {
  skip_if(!nzchar(Sys.getenv("GTAP12AEZ_dat")), "GTAP12AEZ_* inputs not set")
  aez <- ems_data(
    dat_input = Sys.getenv("GTAP12AEZ_dat"),
    par_input = Sys.getenv("GTAP12AEZ_par"),
    set_input = Sys.getenv("GTAP12AEZ_set"),
    REG = "big3",
    ACTS = "macro_sector"
  )
  expect_true(isTRUE(attr(aez, "metadata")$aez))
  # the disaggregated activity set and its mapping header stay at
  # source resolution; the mapping composes onto ACTS at deploy
  expect_equal(nrow(aez$DACT), 65L)
  expect_true(all(aez$DACT$origin == aez$DACT$mapping))
  expect_equal(length(attr(aez, "set_raw")$MACT), 65L)
  # the renamed dimensions aggregate through the ACTS mapping
  expect_true("CROPACTS" %in% names(aez$AREA))
  expect_equal(unique(aez$AREA$CROPACTS), "crops")
  expect_true("LCOV" %in% names(aez$LCOV))
})

test_that("ems_data prepares a GTAP-E database in place", {
  skip_if(!nzchar(Sys.getenv("GTAP12E_dat")), "GTAP12E_* inputs not set")
  e <- ems_data(
    dat_input = Sys.getenv("GTAP12E_dat"),
    par_input = Sys.getenv("GTAP12E_par"),
    set_input = Sys.getenv("GTAP12E_set"),
    REG = "big3",
    ACTS = "energy"
  )
  md <- attr(e, "metadata")
  expect_true(isTRUE(md[["e"]]))
  expect_null(md[["ep"]])
  # the disaggregated commodity list and its mapping stay at source
  expect_equal(nrow(e$DCOM), 65L)
  expect_true(all(e$DCOM$origin == e$DCOM$mapping))
  expect_equal(length(attr(e, "set_raw")$MCOM), 65L)
  # the energy sets follow COMM; the energy mapping keeps them apart
  energy <- c("coa", "oil", "gas", "p_c", "ely", "gdt")
  expect_setequal(unique(e$EGY$mapping), energy)
  expect_setequal(unique(e$ENYP$mapping), energy)
  # the CDE parameters are read over TOPP: the energy node plus the
  # aggregated non-energy commodities, weighted through that recast
  expect_true("TOPP" %in% names(e$SUBP))
  expect_true("eny" %in% e$SUBP$TOPP)
  expect_false(any(energy %in% e$SUBP$TOPP))
  expect_false(any(c("SUBE", "INCE") %in% names(e)))
  expect_lte(max(e$SUBP$Value), 1)
})

test_that("ems_data prepares a GTAP-Power database in place", {
  skip_if(!nzchar(Sys.getenv("GTAP12P_dat")), "GTAP12P_* inputs not set")
  ep <- ems_data(
    dat_input = Sys.getenv("GTAP12P_dat"),
    par_input = Sys.getenv("GTAP12P_par"),
    set_input = Sys.getenv("GTAP12P_set"),
    REG = "big3",
    ACTS = "power"
  )
  md <- attr(ep, "metadata")
  expect_true(isTRUE(md[["ep"]]))
  expect_null(md[["e"]])
  expect_equal(nrow(ep$DCOM), 76L)
  # the power mapping (the layer's own bucket, not a core mapping) keeps
  # transmission, base load and peak load apart
  expect_false("power" %in% names(getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$ACTS))
  expect_equal(unique(ep$EBLF$mapping), "baseload")
  expect_equal(unique(ep$EPLF$mapping), "peakload")
  expect_setequal(unique(ep$ELYF$mapping), c("egen", "tnd"))
  expect_true("ely" %in% ep$ENYP$mapping)
  expect_true("tnd" %in% ep$EGY$mapping)
  expect_true("eny" %in% ep$SUBP$TOPP)
  expect_false("tnd" %in% ep$SUBP$TOPP)
})

unlink(write_dir, recursive = TRUE)
