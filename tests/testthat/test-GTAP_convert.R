skip_on_cran()

ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

test_that("GTAP_convert errors when invalid origin", {
  expect_snapshot_error(GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv8",
    "GTAPv7"
  ))
})

test_that("GTAP_convert errors when invalid target", {
  expect_snapshot_error(GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv6",
    "GTAPv3"
  ))
})

test_that("GTAP_convert errors when incorrect origin", {
  expect_snapshot_error(GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv7",
    "GTAPv6"
  ))
})

test_that("GTAP_convert errors when origin equals target", {
  expect_snapshot_error(GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv6",
    "GTAPv6"
  ))
})

test_that("GTAP_convert GTAP9", {
  v7_9 <- GTAP_convert(
    Sys.getenv("GTAP9_dat"),
    Sys.getenv("GTAP9_par"),
    Sys.getenv("GTAP9_set"),
    "GTAPv6",
    "GTAPv7"
  )
  check <- attr(v7_9$dat, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})

test_that("GTAP_convert GTAP10A", {
  v7_10a <- GTAP_convert(
    Sys.getenv("GTAP10A_dat"),
    Sys.getenv("GTAP10A_par"),
    Sys.getenv("GTAP10A_set"),
    "GTAPv6",
    "GTAPv7"
  )
  check <- attr(v7_10a$dat, "metadata")$data_format == "GTAPv7"
  expect_true(check)
})

test_that("GTAP_convert GTAP11c", {
  v6_11c <- GTAP_convert(
    Sys.getenv("GTAP11c_dat"),
    Sys.getenv("GTAP11c_par"),
    Sys.getenv("GTAP11c_set"),
    "GTAPv7",
    "GTAPv6"
  )
  check <- attr(v6_11c$dat, "metadata")$data_format == "GTAPv6"
  expect_true(check)
})

test_that("GTAP_convert GTAP12", {
  v6_12 <- GTAP_convert(
    Sys.getenv("GTAP12_dat"),
    Sys.getenv("GTAP12_par"),
    Sys.getenv("GTAP12_set"),
    "GTAPv7",
    "GTAPv6"
  )
  check <- attr(v6_12$dat, "metadata")$data_format == "GTAPv6"
  expect_true(check)
})

test_that("GTAP_convert from v7 to v6", {
  convertedv6 <- GTAP_convert(
    Sys.getenv("GTAP11c_dat"),
    Sys.getenv("GTAP11c_par"),
    Sys.getenv("GTAP11c_set"),
    "GTAPv7",
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

  gdyn <- ems_data(
    Sys.getenv("GDYN11c_dat"),
    Sys.getenv("GDYN11c_par"),
    Sys.getenv("GDYN11c_set"),
    REG = "full",
    PROD_COMM = "full",
    ENDW_COMM = "full"
  )

  gdyn <- lapply(gdyn, \(h) {
    nmes <- colnames(h)
    if ("ENDW_COMM" %in% nmes) {
      h[is.na(ENDW_COMM), ENDW_COMM := "natlres"]
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
    "GTAPv6",
    "GTAPv7"
  )

  converted$set <- converted$set[!duplicated(names(converted$set))]

  gdyn <- ems_data(
    converted$dat,
    converted$par,
    converted$set,
    REG = "full",
    ACTS = "full",
    ENDW = "full"
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
      h[is.na(ENDW), ENDW := "natlres"]
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