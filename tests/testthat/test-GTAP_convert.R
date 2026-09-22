skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

test_that("GTAP_convert errors when invalid target", {
  expect_snapshot_error(GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv3"
  ))
})

test_that("GTAP_convert warns when v9 inputs are already in target format", {
  expect_snapshot_warning(GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv6"
  ))
})

test_that("GTAP_convert warns when v10 inputs are already in target format", {
  expect_snapshot_warning(GTAP_convert(
    Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set"),
    "GTAPv6"
  ))
})

test_that("GTAP_convert warns when v11 inputs are already in target format", {
  expect_snapshot_warning(GTAP_convert(
    Sys.getenv("GTAP11c_dat"),
    Sys.getenv("GTAP11c_par"),
    Sys.getenv("GTAP11c_set"),
    "GTAPv7"
  ))
})

test_that("GTAP_convert warns when v12 inputs are already in target format", {
  expect_snapshot_warning(GTAP_convert(
    Sys.getenv("GTAP12_dat"),
    Sys.getenv("GTAP12_par"),
    Sys.getenv("GTAP12_set"),
    "GTAPv7"
  ))
})

test_that("GTAP_convert GTAP9 format", {
  v7_9 <- GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv7"
  )
  check <- attr(v7_9$dat, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})

test_that("GTAP_convert GTAP10A format", {
  v7_10a <- GTAP_convert(
    Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set"),
    "GTAPv7"
  )
  check <- attr(v7_10a$dat, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})

test_that("GTAP_convert GTAP11c format", {
  v6_11c <- GTAP_convert(
    Sys.getenv("GTAP11c_dat"),
    Sys.getenv("GTAP11c_par"),
    Sys.getenv("GTAP11c_set"),
    "GTAPv6"
  )
  check <- attr(v6_11c$dat, "metadata")$data_format == "GTAPv6"
  expect_true(check)
})

test_that("GTAP_convert GTAP12 format", {
  v6_12 <- GTAP_convert(
    Sys.getenv("GTAP12_dat"),
    Sys.getenv("GTAP12_par"),
    Sys.getenv("GTAP12_set"),
    "GTAPv6"
  )
  check <- attr(v6_12$dat, "metadata")$data_format == "GTAPv6"
  expect_true(check)
})

test_that("GTAP_convert GTAP9", {
  v9 <- GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set")
  )
  check <- attr(v9$dat, "metadata")$data_format == "GTAPv6"
  expect_true(check)
})

test_that("GTAP_convert GTAP10A", {
  v10a <- GTAP_convert(
    Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set")
  )
  check <- attr(v10a$dat, "metadata")$data_format == "GTAPv6"
  expect_true(check)
})

test_that("GTAP_convert GTAP11c", {
  v11c <- GTAP_convert(
    Sys.getenv("GTAP11c_dat"),
    Sys.getenv("GTAP11c_par"),
    Sys.getenv("GTAP11c_set")
  )
  check <- attr(v11c$dat, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})

test_that("GTAP_convert GTAP12", {
  v12 <- GTAP_convert(
    Sys.getenv("GTAP12_dat"),
    Sys.getenv("GTAP12_par"),
    Sys.getenv("GTAP12_set")
  )
  check <- attr(v12$dat, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})

test_that("GTAP_convert from v7 to v6", {
  convertedv6 <- GTAP_convert(
    Sys.getenv("GTAP11c_dat"),
    Sys.getenv("GTAP11c_par"),
    Sys.getenv("GTAP11c_set"),
    "GTAPv6"
  )

  v6 <- ems_data(
    convertedv6$dat,
    convertedv6$par,
    convertedv6$set,
    REG = "full",
    PROD_COMM = "full",
    ENDW_COMM = "full"
  )

  # the GDYN 11c set file spells NatRes in its ENDW header where the
  # mappings say natlres: the spelling is mapped explicitly
  ENDW_COMM <- getFromNamespace("mappings", "teems")$GTAPv11$GTAPv6$ENDW_COMM[, c(1, 2)]
  ENDW_COMM <- rbind(ENDW_COMM, as.list(c("natres", "natlres")))
  ENDW_COMM_csv <- tempfile("ENDW_COMM_natres", fileext = ".csv")
  write.csv(ENDW_COMM, ENDW_COMM_csv, row.names = FALSE)
  gdyn <- ems_data(
    Sys.getenv("GDYN11c_dat"),
    Sys.getenv("GDYN11c_par"),
    Sys.getenv("GDYN11c_set"),
    REG = "full",
    PROD_COMM = "full",
    ENDW_COMM = ENDW_COMM_csv
  )

  gdyn <- lapply(gdyn, \(h) {
    nmes <- colnames(h)
    if ("ENDW_COMM" %in% nmes) {
      data.table::setorderv(h, setdiff(nmes, "Value"))
    }
    return(h)
  })

  gdyn$ETRE <- gdyn$ETRE[ENDW_COMM %in% c("land", "natlres")]
  colnames(gdyn$ETRE) <- c("ENDWS_COMM", "Value")
  common_headers <- intersect(names(gdyn), names(v6))
  common_headers <- setdiff(common_headers, "SAVE")
  gdyn <- gdyn[names(gdyn) %in% common_headers]
  v6 <- v6[names(v6) %in% common_headers]

  gdyn <- gdyn[match(names(v6), names(gdyn))]
  
  checks <- purrr::map2_lgl(
    gdyn,
    v6,
    all.equal,
    check.attributes = F,
    tolerance = 1e-4
  )

  expect_all_true(checks)
})

test_that("GTAP_convert from v6 to v7", {
  converted <- GTAP_convert(
    Sys.getenv("GDYN11c_dat"),
    Sys.getenv("GDYN11c_par"),
    Sys.getenv("GDYN11c_set"),
    "GTAPv7"
  )

  converted$set <- converted$set[!duplicated(names(converted$set))]

  # the GDYN 11c set file spells NatRes in its ENDW header where the
  # mappings say natlres: the spelling is mapped explicitly
  ENDW <- getFromNamespace("mappings", "teems")$GTAPv11$GTAPv7$ENDW[, c(1, 2)]
  ENDW <- rbind(ENDW, as.list(c("natres", "natlres")))
  ENDW_csv <- tempfile("ENDW_natres", fileext = ".csv")
  write.csv(ENDW, ENDW_csv, row.names = FALSE)
  gdyn <- ems_data(
    converted$dat,
    converted$par,
    converted$set,
    REG = "full",
    ACTS = "full",
    ENDW = ENDW_csv
  )

  gdyn <- gdyn[!duplicated(names(gdyn))]

  normalv7 <- ems_data(
    Sys.getenv("GTAP11c_dat"),
    Sys.getenv("GTAP11c_par"),
    Sys.getenv("GTAP11c_set"),
    REG = "full",
    ACTS = "full",
    ENDW = "full"
  )

  gdyn <- lapply(gdyn, \(h) {
    nmes <- colnames(h)
    if ("ENDW" %in% nmes) {
      data.table::setorderv(h, setdiff(nmes, "Value"))
    }
    return(h)
  })

  common_headers <- intersect(names(gdyn), names(normalv7))
  common_headers <- setdiff(common_headers, c("SAVE", "ENDF", "ENDL"))
  normalv7 <- normalv7[names(normalv7) %in% common_headers]
  gdyn <- gdyn[names(gdyn) %in% common_headers]
  gdyn <- gdyn[match(names(normalv7), names(gdyn))]
  gdyn$ENDW[which(gdyn$ENDW$origin == "natres"), ]$origin <- "natlres"
  gdyn$ENDW[which(gdyn$ENDW$origin == "natlres"), ]$mapping <- "natlres"
  data.table::setorder(gdyn$ENDW)

  gdyn$ETRE <- gdyn$ETRE[!duplicated(gdyn$ETRE)]

  checks <- purrr::map2_lgl(
    gdyn,
    normalv7,
    all.equal,
    check.attributes = F,
    tolerance = 1e-5
  )

  expect_all_true(checks)
})

test_that("GTAP_convert examples work", {
  # The following examples require input data. See
  # https://teemsphere.github.io/ to get started.

  # Convert HAR files to lists of arrays
  ls_arrays <- GTAP_convert(
    dat_har = Sys.getenv("GTAP12_dat"),
    par_har = Sys.getenv("GTAP12_par"),
    set_har = Sys.getenv("GTAP12_set"),
  )
  
  expect_type(ls_arrays, "list")
  expect_equal(attr(ls_arrays$dat, "metadata")$data_format, "GTAPv7")

  # Convert v7.0 files to v6.2 format
  converted2v6 <- GTAP_convert(
    dat_har = Sys.getenv("GTAP12_dat"),
    par_har = Sys.getenv("GTAP12_par"),
    set_har = Sys.getenv("GTAP12_set"),
    target    = "GTAPv6"
  )
  
  expect_type(converted2v6, "list")
  expect_equal(attr(converted2v6$dat, "metadata")$data_format, "GTAPv6")

  # Convert v6.2 files to v7.0 format
  converted2v7 <- GTAP_convert(
    Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set"),
    target    = "GTAPv7"
  )
  
  expect_type(converted2v7, "list")
  expect_equal(attr(converted2v7$dat, "metadata")$data_format, "GTAPv7")
})
test_that("GTAP-AEZ preparation on a synthetic layer", {
  fmt <- "GTAPv7"
  mk_set <- function(h, ele) structure(ele, class = c(h, h, "set", fmt, "character"))
  mk_arr <- function(h, kind, dims, value = 1) {
    a <- array(value, dim = lengths(dims), dimnames = dims)
    class(a) <- c(h, kind, fmt, class(a))
    a
  }
  reg <- c("chn", "row")
  acts <- c("pdr", "wht", "ctl", "frs", "mnfcs")
  i_data <- list(
    REG = mk_set("REG", reg),
    ACTS = mk_set("ACTS", acts),
    AEZS = mk_set("AEZS", c("aez1", "aez2")),
    COVS = mk_set("COVS", c("forestland", "cropland", "otherland")),
    CROP = mk_set("CROP", c("pdr", "wht")),
    LUSA = mk_set("LUSA", c("pdr", "wht", "ctl", "frs")),
    ESBV = mk_arr("ESBV", "par", list(ACTS = acts, REG = reg)),
    AREA = mk_arr("AREA", "dat", list(AEZS = c("aez1", "aez2"), CROP = c("pdr", "wht"), REG = reg)),
    TONS = mk_arr("TONS", "dat", list(AEZS = c("aez1", "aez2"), CROP = c("pdr", "wht"), REG = reg)),
    LCOV = mk_arr("LCOV", "dat", list(AEZS = c("aez1", "aez2"), COVS = c("forestland", "cropland", "otherland"), REG = reg))
  )
  attr(i_data, "metadata") <- list(data_format = fmt, database_version = "GTAPv12")
  class(i_data) <- c(fmt, "list")
  expect_true(.layer_detect(i_data, "aez"))
  out <- .prepare_aez(i_data, call = NULL)
  expect_true(isTRUE(attr(out, "metadata")$aez))
  expect_false(.layer_detect(out, "aez") && !isTRUE(attr(out, "metadata")$aez))
  # disaggregated sets at source resolution, never aggregated
  expect_equal(as.character(out$DACT), acts)
  expect_equal(as.character(out$MACT), acts)
  expect_equal(as.character(out$DCRP), c("pdr", "wht"))
  expect_equal(as.character(out$DFRS), "frs")
  expect_equal(as.character(out$DGRZ), "ctl")
  expect_equal(as.character(out$DLUA), c("pdr", "wht", "ctl", "frs"))
  for (h in c("DACT", "DCRP", "DFRS", "DGRZ", "DLUA", "MACT")) {
    expect_true(isTRUE(attr(out[[h]], "user_set")))
    expect_true(inherits(out[[h]], "set"))
  }
  # model-facing dimension and set names
  expect_equal(names(dimnames(out$AREA)), c("AEZS", "CROPACTS", "REG"))
  expect_equal(names(dimnames(out$TONS)), c("AEZS", "CROPACTS", "REG"))
  expect_equal(names(dimnames(out$LCOV)), c("AEZS", "LCOV", "REG"))
  expect_equal(class(out$CROP)[1:2], c("CROP", "CROPACTS"))
  expect_equal(class(out$COVS)[1:2], c("COVS", "LCOV"))
  # the flexagg parameter constants
  expect_equal(unname(out$EAEZ[, "chn"]), c(20, 20, 20, 20, 0))
  expect_equal(unname(out$YDON[, "row"]), c(1, 1, 0, 0, 0))
  expect_equal(unique(as.vector(out$ETAE)), 0.66)
  expect_equal(as.vector(out$YDRS), c(1, 1))
  expect_equal(as.vector(out$YDET), 0.25)
  expect_true(inherits(out$EAEZ, "par"))
  # grouping kept: sets, then parameters, then data
  kinds <- vapply(unclass(out), function(x) class(x)[3], character(1))
  kinds[vapply(unclass(out), inherits, logical(1), "par")] <- "par"
  kinds[vapply(unclass(out), inherits, logical(1), "dat")] <- "dat"
  expect_equal(rle(unname(kinds))$values, c("set", "par", "dat"))
  # an incomplete layer is named
  expect_snapshot_error(.prepare_aez(i_data[names(i_data) != "LUSA"], call = NULL))
})

test_that("GTAP_convert GTAP-AEZ target (v12a AEZ, v7 format)", {
  skip_if(!nzchar(Sys.getenv("GTAP12AEZ_dat")), "GTAP12AEZ_* inputs not set")
  aez <- GTAP_convert(
    Sys.getenv("GTAP12AEZ_dat"),
    Sys.getenv("GTAP12AEZ_par"),
    Sys.getenv("GTAP12AEZ_set"),
    "GTAP-AEZ"
  )
  md <- attr(aez$dat, "metadata")
  expect_equal(md$data_format, "GTAPv7")
  expect_true(isTRUE(md$aez))
  expect_true(all(c("DACT", "DCRP", "DFRS", "DGRZ", "DLUA", "MACT") %in% names(aez$set)))
  expect_equal(as.character(aez$set$DACT), tolower(as.character(aez$set$ACTS)))
  expect_equal(names(dimnames(aez$dat$AREA)), c("AEZS", "CROPACTS", "REG"))
  # the database ships the AEZ parameters: kept as they arrive
  expect_equal(range(aez$par$EAEZ), c(0, 20))
})

test_that("GTAP_convert GTAP-AEZ target rejects the v6-format layer (v10a AEZ)", {
  skip_if(!nzchar(Sys.getenv("GTAP10AEZ_dat")), "GTAP10AEZ_* inputs not set")
  expect_snapshot_error(suppressWarnings(GTAP_convert(
    Sys.getenv("GTAP10AEZ_dat"),
    Sys.getenv("GTAP10AEZ_par"),
    Sys.getenv("GTAP10AEZ_set"),
    "GTAP-AEZ"
  )))
})

test_that("GTAP_convert GTAP-E and GTAP-EP targets reject a v6-format database", {
  skip_if(!nzchar(Sys.getenv("GTAP10A_dat")), "GTAP10A_* inputs not set")
  expect_snapshot_error(suppressWarnings(GTAP_convert(
    Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set"),
    "GTAP-E"
  )))
  expect_snapshot_error(suppressWarnings(GTAP_convert(
    Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set"),
    "GTAP-EP"
  )))
})

test_that("GTAP_convert GTAP-E target (v12a E, v7 format)", {
  skip_if(!nzchar(Sys.getenv("GTAP12E_dat")), "GTAP12E_* inputs not set")
  e <- GTAP_convert(
    Sys.getenv("GTAP12E_dat"),
    Sys.getenv("GTAP12E_par"),
    Sys.getenv("GTAP12E_set"),
    "GTAP-E"
  )
  md <- attr(e$dat, "metadata")
  expect_true(isTRUE(md[["e"]]))
  expect_null(md[["ep"]])
  expect_equal(md$database_version, "GTAPv12")
  expect_true(all(c("DCOM", "MCOM", "DELY", "EGY", "ENYP", "ENYG", "ENYI", "TOPP") %in% names(e$set)))
  expect_equal(as.character(e$set$DCOM), tolower(as.character(e$set$COMM)))
  expect_equal(as.character(e$set$DELY), "ely")
  expect_equal(as.character(e$set$EGY), tolower(as.character(e$set$COME)))
  expect_true("eny" %in% as.character(e$set$TOPP))
  expect_equal(names(dimnames(e$par$SUBP))[[1]], "TOPP")
  expect_equal(names(dimnames(e$par$INCP))[[1]], "TOPP")
  expect_false(any(c("SUBE", "INCE") %in% names(e$par)))
  expect_lte(max(e$par$SUBP), 1)
  expect_true(inherits(e$set$TRBL, "set"))
})

test_that("GTAP_convert GTAP-EP target (v12a Power, v7 format)", {
  skip_if(!nzchar(Sys.getenv("GTAP12P_dat")), "GTAP12P_* inputs not set")
  ep <- GTAP_convert(
    Sys.getenv("GTAP12P_dat"),
    Sys.getenv("GTAP12P_par"),
    Sys.getenv("GTAP12P_set"),
    "GTAP-EP"
  )
  md <- attr(ep$dat, "metadata")
  expect_true(isTRUE(md[["ep"]]))
  expect_null(md[["e"]])
  expect_equal(md$full_database_version, "GTAPv12aPower")
  expect_equal(md$database_version, "GTAPv12")
  nests <- as.vector(outer(c("ELE", "ELY", "EGN", "EBL", "EPL"), c("F", "G", "I", "P"), paste0))
  expect_true(all(c("DCOM", "MCOM", "DELY", "EGY", "ENYP", "ENYG", "ENYI", "TOPP", nests) %in% names(ep$set)))
  expect_length(as.character(ep$set$DCOM), 76L)
  expect_equal(as.character(ep$set$ELYF), c("egen", "tnd"))
  expect_setequal(as.character(ep$set$EBLF), c("nuclearbl", "coalbl", "gasbl", "windbl", "hydrobl", "oilbl", "otherbl"))
  expect_setequal(as.character(ep$set$EPLF), c("gasp", "hydrop", "oilp", "solarp"))
  expect_true("tnd" %in% as.character(ep$set$EGY))
  expect_false("tnd" %in% as.character(ep$set$TOPP))
  expect_equal(names(dimnames(ep$par$SUBP))[[1]], "TOPP")
  expect_lte(max(ep$par$SUBP), 1)
})

test_that("GTAP_convert refuses the 11c energy releases by name", {
  skip_if(!nzchar(Sys.getenv("GTAP11cE_dat")), "GTAP11cE_* inputs not set")
  expect_error(
    GTAP_convert(
      Sys.getenv("GTAP11cE_dat"),
      Sys.getenv("GTAP11cE_par"),
      Sys.getenv("GTAP11cE_set"),
      "GTAP-E"
    ),
    "un-normalised"
  )
  skip_if(!nzchar(Sys.getenv("GTAP11cP_dat")), "GTAP11cP_* inputs not set")
  expect_error(
    GTAP_convert(
      Sys.getenv("GTAP11cP_dat"),
      Sys.getenv("GTAP11cP_par"),
      Sys.getenv("GTAP11cP_set"),
      "GTAP-EP"
    ),
    "un-normalised"
  )
})
