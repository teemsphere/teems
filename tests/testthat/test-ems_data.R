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

test_that("single-file route needs both or neither of par_input and set_input", {
  expect_snapshot_error(ems_data(dat_input, par_input))
})

test_that("single-file route rejects set mappings", {
  expect_snapshot_error(ems_data(dat_input, REG = "big3"))
})

test_that("single-file route loads a non-GTAP HAR", {
  orani_har <- Sys.getenv("ORANIG_har")
  skip_if(!nzchar(orani_har), "ORANIG_har not set")
  orani <- ems_data(orani_har)
  expect_s3_class(orani, "ems_data")
  is_set <- vapply(orani, inherits, logical(1), "set")
  expect_setequal(names(orani)[is_set], c("COM", "IND", "OCC", "MAR", "REG"))
  expect_equal(names(orani[["1BAS"]]), c("COM", "SRC", "IND", "Value"))
  expect_equal(nrow(orani[["1BAS"]]), 37L * 2L * 35L)
  expect_equal(names(orani[["P021"]]), "Value")
  expect_false(any(c("XXCD", "XXHS", "MCM2", "MND2") %in% names(orani)))
  md <- attr(orani, "metadata")
  expect_true(isTRUE(md$generic))
  expect_equal(md$data_format, "generic")
  expect_true(is.na(md$reference_year))
  # sets read from the file keep the file's element order (GEMPACK order)
  expect_identical(orani[["COM"]]$origin[1:3], c("woolmutton", "grainshay", "beefcattle"))
  expect_true(is.unsorted(orani[["COM"]]$origin))
  expect_identical(orani[["COM"]]$origin, orani[["COM"]]$mapping)

  # an upper-case extension reads through the same route
  upper_har <- file.path(tempdir(), "BASEDATA.HAR")
  file.copy(orani_har, upper_har, overwrite = TRUE)
  withr::defer(unlink(upper_har))
  upper <- ems_data(upper_har)
  expect_identical(upper[["COM"]], orani[["COM"]])
})

test_that("pass-through sets keep file order and aggregated sets sort", {
  raw <- data.table::data.table(Value = c("c10", "c2", "c1"))
  class(raw) <- c("CNT", "CNT", "set", class(raw))
  ident <- list(CNT = data.table::data.table(CNT = c("c10", "c2", "c1"), mapping = c("c10", "c2", "c1")))
  kept <- .aggregate_data(data.table::copy(raw), sets = ident)
  expect_identical(kept$origin, c("c10", "c2", "c1"))
  expect_identical(kept$mapping, c("c10", "c2", "c1"))

  agg <- list(CNT = data.table::data.table(CNT = c("c10", "c2", "c1"), mapping = c("z", "a", "z")))
  sorted <- .aggregate_data(data.table::copy(raw), sets = agg)
  expect_identical(sorted$mapping, c("a", "z", "z"))
  expect_identical(sorted$origin, c("c2", "c1", "c10"))
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

test_that("ems_data folds an uppercase user mapping to lowercase", {
  REG_upper <- data.table::copy(REG)
  REG_upper[, names(REG_upper) := lapply(.SD, toupper)]
  write.csv(REG_upper, REG_csv, row.names = FALSE)
  upper <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = REG_csv,
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  expect_setequal(unique(upper$VDFB$REG), unique(tolower(REG[[2]])))
  expect_false(anyNA(upper$VDFB$REG))
})

test_that("ems_data accepts a lowercase user mapping against mixed-case data elements", {
  # the database spells Land, Capital and NatlRes in its ENDW header
  ENDW <- getFromNamespace("mappings", "teems")$GTAPv12$GTAPv7$ENDW[, c(1, 2)]
  ENDW_csv <- file.path(write_dir, "ENDW.csv")
  write.csv(ENDW, ENDW_csv, row.names = FALSE)
  folded <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = ENDW_csv
  )
  with_endw <- Filter(\(h) is.data.frame(h) && "ENDW" %in% colnames(h), folded)
  expect_gt(length(with_endw), 0L)
  expect_setequal(unique(with_endw[[1]]$ENDW), unique(ENDW[[2]]))
  expect_false(anyNA(with_endw[[1]]$ENDW))
})

test_that("ems_data rejects a data element its mapping does not cover", {
  expect_snapshot_error(
    getFromNamespace(".check_map_coverage", "teems")(
      set_mapping = data.table::data.table(
        ENDW = c("land", "capital", "natlres"),
        full = c("land", "capital", "natlres")
      ),
      data_ele = c("Land", "Capital", "NatRes"),
      map_name = "ENDW",
      call = NULL
    )
  )
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
  # DPSM is averaged over the regions an aggregate absorbs, not summed
  dat_headers <- setdiff(dat_headers, "DPSM")
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
  # loaded with verbose on: the layer announces itself
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_set(verbose = FALSE))
  msgs <- capture_messages(aez <- ems_data(
    dat_input = Sys.getenv("GTAP12AEZ_dat"),
    par_input = Sys.getenv("GTAP12AEZ_par"),
    set_input = Sys.getenv("GTAP12AEZ_set"),
    REG = "big3",
    ACTS = "macro_sector"
  ))
  expect_match(msgs, "GTAP-AEZ layer detected", fixed = TRUE, all = FALSE)
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
  # ESUBAEZ is weighted by land rents: every land-using aggregate keeps
  # the source value 20, however few of its members use land
  land_using <- aez$EAEZ[ACTS %in% c("crops", "livestock", "mnfcs")]
  expect_true(all(land_using$Value == 20))
})

test_that("ems_data prepares a GTAP-E database in place", {
  skip_if(!nzchar(Sys.getenv("GTAP12E_dat")), "GTAP12E_* inputs not set")
  # loaded with verbose on: the layer announces itself
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_set(verbose = FALSE))
  msgs <- capture_messages(e <- ems_data(
    dat_input = Sys.getenv("GTAP12E_dat"),
    par_input = Sys.getenv("GTAP12E_par"),
    set_input = Sys.getenv("GTAP12E_set"),
    REG = "big3",
    ACTS = "energy"
  ))
  expect_match(msgs, "GTAP-E layer detected", fixed = TRUE, all = FALSE)
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
  # GSHR is a share, averaged rather than summed over the aggregated regions
  expect_true(all(e$GSHR$Value == 1))
  # ELFVAEN as the FlexAgg GTAP-E program aggregates it: recalibrated
  # to the fossil supply elasticity for coal, oil and gas mining, 1 for
  # the other fuels, weighted by value added and energy elsewhere
  efve <- e$EFVE
  expect_equal(efve[ACTS == "gas" & REG == "chn", Value], 0.131541, tolerance = 1e-5)
  expect_equal(efve[ACTS == "gas" & REG == "usa", Value], 1.504764, tolerance = 1e-5)
  expect_equal(efve[ACTS == "coa" & REG == "usa", Value], 3.58521, tolerance = 1e-5)
  expect_true(all(efve[ACTS %in% c("p_c", "gdt"), Value] == 1))
})

test_that("ems_data prepares a GTAP-Power database in place", {
  skip_if(!nzchar(Sys.getenv("GTAP12P_dat")), "GTAP12P_* inputs not set")
  # loaded with verbose on: the layer announces itself
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_set(verbose = FALSE))
  msgs <- capture_messages(ep <- ems_data(
    dat_input = Sys.getenv("GTAP12P_dat"),
    par_input = Sys.getenv("GTAP12P_par"),
    set_input = Sys.getenv("GTAP12P_set"),
    REG = "big3",
    ACTS = "power"
  ))
  expect_match(msgs, "GTAP-Power layer detected", fixed = TRUE, all = FALSE)
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
  expect_true(all(ep$GSHR$Value == 1))
  # the power mapping renames the fuels: gas absorbs gdt and is
  # recalibrated as mining, oil_pcts holds p_c alone and takes 1; peak
  # load is weighted towards its near-zero gas and oil technologies
  efve <- ep$EFVE
  expect_equal(efve[ACTS == "gas" & REG == "usa", Value], 0.938472, tolerance = 1e-5)
  expect_true(all(efve[ACTS == "oil_pcts", Value] == 1))
  expect_lt(max(efve[ACTS == "peakload", Value]), 0.02)
})

test_that("ems_data averages DPSM over the regions it aggregates", {
  dpsm <- agg_data$DPSM
  expect_setequal(dpsm$REG, c("chn", "row", "usa"))
  expect_true(all(dpsm$Value == 1))
})

test_that("ems_data rejects a par_weights method it does not know", {
  expect_snapshot_error(ems_data(
    dat_input, par_input, set_input,
    REG = "big3", par_weights = "mean"
  ))
})

test_that("ems_data rejects more than one default par_weights method", {
  expect_snapshot_error(ems_data(
    dat_input, par_input, set_input,
    REG = "big3", par_weights = c("share", "value")
  ))
})

test_that("ems_data rejects a par_weights parameter the format does not weight", {
  expect_snapshot_error(ems_data(
    dat_input, par_input, set_input,
    REG = "big3", par_weights = c(ESBX = "value")
  ))
})

test_that("par_weights selects the share or value weights per parameter", {
  value_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    par_weights = "value"
  )
  mixed_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    par_weights = c(ESBM = "value")
  )
  share_w <- attr(agg_data, "metadata")$par_weights
  # the CDE parameters have no share nest and are recorded as value weighted
  expect_true(all(share_w[c("INCP", "SUBP")] == "value"))
  expect_true(all(share_w[setdiff(names(share_w), c("INCP", "SUBP"))] == "share"))
  expect_true(all(names(share_w) %in% names(agg_data)))
  expect_true(all(attr(value_data, "metadata")$par_weights == "value"))
  expect_equal(unname(attr(mixed_data, "metadata")$par_weights["ESBM"]), "value")
  # China's livestock imports are half wool (ESBM 12.9), nearly all from
  # one aggregated source: its sourcing can barely shift, so share
  # weights keep wool from dominating the aggregate. Value weights are
  # FlexAgg's: world import totals, one value for every region
  livestock <- \(d, h, r = "chn") d[[h]][COMM == "livestock" & REG == r, Value]
  expect_equal(livestock(agg_data, "ESBM"), 3.010618, tolerance = 1e-5)
  expect_equal(livestock(value_data, "ESBM"), 3.980415, tolerance = 1e-5)
  expect_equal(livestock(value_data, "ESBM", "usa"), 3.980415, tolerance = 1e-5)
  # ESBD pools one domestic/imported nest per agent
  expect_equal(livestock(agg_data, "ESBD"), 2.451707, tolerance = 1e-5)
  expect_equal(livestock(value_data, "ESBD"), 2.115725, tolerance = 1e-5)
  expect_equal(mixed_data$ESBM, value_data$ESBM)
  expect_equal(mixed_data$ESBD, agg_data$ESBD)
  # the CDE parameters are value weighted under either method
  expect_equal(agg_data$SUBP, value_data$SUBP)
})

unlink(write_dir, recursive = TRUE)
