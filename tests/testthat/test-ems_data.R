skip_on_cran()
ems_option_set(verbose = FALSE)

test_that("ems_data requires dat_input argument", {
  expect_snapshot_error(ems_data())
})

test_that("ems_data requires par_input argument", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  expect_snapshot_error(ems_data(dat_input = dat_input))
})

test_that("ems_data requires set_input argument", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input
  ))
})

test_that("ems_data requires REG argument", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input
  ))
})

test_that("ems_data rejects non-character dat_input", {
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = 1,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects non-character par_input", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = 1,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects non-character set_input", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = 1,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

# test_that("ems_data rejects non-character aux_input", {
#   dat_input <- Sys.getenv("GTAP11c_dat")
#   par_input <- Sys.getenv("GTAP11c_par")
#   set_input <- Sys.getenv("GTAP11c_set")
#   expect_snapshot_error(ems_data(
#     dat_input = dat_input,
#     par_input = par_input,
#     set_input = set_input,
#     aux_input = 1,
#     REG = "big3",
#     ACTS = "macro_sector",
#     ENDW = "labor_agg"
#   ))
# })

test_that("ems_data rejects non-character REG", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = 1,
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects invalid internal mapping name", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "not_an_internal_mapping"
  ))
})

test_that("ems_data rejects non-existent CSV file", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "not_a_file.csv"
  ))
})

test_that("ems_data rejects wrong file extension for mapping", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  tmp_txt <- tempfile(fileext = ".txt")
  file.create(tmp_txt)
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = tmp_txt
  ))
})

test_that("ems_data rejects invalid mapping values in CSV", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  REG <- teems:::mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  REG[1, ] <- "invalid"
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = REG_csv
  ))
})

test_that("ems_data rejects CSV with extra columns", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  REG <- teems:::mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  REG$extra_col <- NA
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_warning(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = REG_csv
  ))
})

test_that("ems_data rejects CSV with insufficient columns", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  REG <- teems:::mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  REG <- REG[, 1]
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = REG_csv
  ))
})

test_that("ems_data rejects invalid target_format", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16),
    target_format = "not_a_format"
  ))
})

test_that("ems_data unnecessary convert", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_warning(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16),
    target_format = "GTAPv7"
  ))
})

test_that("ems_data rejects unrecognized set arguments", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    not_a_set = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects unrecognized set arguments with CSV mapping", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  REG <- teems:::mappings$GTAPv11$GTAPv7$REG[, c(1, 2)]
  if (!dir.exists(tools::R_user_dir("teems", "cache"))) {
    dir.create(tools::R_user_dir("teems", "cache"))
  }
  REG_csv <- file.path(tools::R_user_dir("teems", "cache"), "REG.csv")
  write.csv(REG, REG_csv, row.names = FALSE)
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    not_a_set = REG_csv,
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
})

test_that("ems_data rejects duplicate time_steps", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_error(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 1)
  ))
})

test_that("ems_data warns wrong initial year", {
  dat_input <- Sys.getenv("GTAP11c_dat")
  par_input <- Sys.getenv("GTAP11c_par")
  set_input <- Sys.getenv("GTAP11c_set")
  expect_snapshot_warning(ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(2014, 2015, 2016)
  ))
})

test_that("ems_data converts from v7 to v6", {
  convertedv6 <- ems_data(
    dat_input = Sys.getenv("GTAP11c_dat"),
    par_input = Sys.getenv("GTAP11c_par"),
    set_input = Sys.getenv("GTAP11c_set"),
    REG = "full",
    PROD_COMM = "full",
    ENDW_COMM = "full",
    target_format = "GTAPv6"
  )
  
  gdyn <- ems_data(
    dat_input = Sys.getenv("GDYN11c_dat"),
    par_input = Sys.getenv("GDYN11c_par"),
    set_input = Sys.getenv("GDYN11c_set"),
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
  
  common_headers <- intersect(names(gdyn), names(convertedv6))
  common_headers <- setdiff(common_headers, "SAVE")
  gdyn <- gdyn[names(gdyn) %in% common_headers]
  convertedv6 <- convertedv6[names(convertedv6) %in% common_headers]
  
  gdyn <- gdyn[match(names(convertedv6), names(gdyn))]
  
  checks <- purrr::map2_lgl(
    gdyn,
    convertedv6,
    all.equal,
    check.attributes = F,
    tolerance = 1e-4
  )
  
  expect_all_true(checks)
})

test_that("ems_data converts from v6 to v7", {
  gdyn <- ems_data(
    dat_input = Sys.getenv("GDYN11c_dat"),
    par_input = Sys.getenv("GDYN11c_par"),
    set_input = Sys.getenv("GDYN11c_set"),
    REG = "full",
    ACTS = "full",
    ENDW = "full",
    target_format = "GTAPv7"
  )
  
  gdyn <- gdyn[!duplicated(names(gdyn))]
  
  normalv7 <- ems_data(
    dat_input = Sys.getenv("GTAP11c_dat"),
    par_input = Sys.getenv("GTAP11c_par"),
    set_input = Sys.getenv("GTAP11c_set"),
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
  gdyn$ENDW[which(gdyn$ENDW$origin == "natres"),]$origin <- "natlres"
  gdyn$ENDW[which(gdyn$ENDW$origin == "natlres"),]$mapping <- "natlres"
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