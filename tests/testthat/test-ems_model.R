ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

write_modified_model <- function(model_input, text, .fn = paste) {
  model_text <- readChar(model_input, file.info(model_input)[["size"]])
  modified <- .fn(model_text, text)
  out_path <- file.path(dirname(model_input), "error.tab")
  writeLines(modified, out_path)
  out_path
}

model_input <- ems_example("GTAPv6.tab", file.path("models", "GTAPv6"))
closure_file <- ems_example("GTAPv6.cls", file.path("models", "GTAPv6"))
test_that("ems_model requires both model_input and closure_file", {
  expect_snapshot_error(ems_model())
})

test_that("ems_model requires closure_file when only model_input provided", {
  expect_snapshot_error(ems_model(model_input = model_input))
})

test_that("ems_model requires model_input when only closure_file provided", {
  expect_snapshot_error(ems_model(closure_file = closure_file))
})

test_that("ems_model rejects non-character model_input", {
  expect_snapshot_error(ems_model(model_input = 1, closure_file = closure_file))
})

test_that("ems_model rejects non-character closure_file", {
  expect_snapshot_error(ems_model(model_input = model_input, closure_file = TRUE))
  expect_snapshot_error(ems_model(model_input = model_input, closure_file = 1))
})

test_that("ems_model rejects non-existent model_input file", {
  expect_snapshot_error(ems_model(model_input = "not_a_file", closure_file = closure_file))
})

test_that("ems_model rejects non-existent closure_file", {
  expect_snapshot_error(ems_model(model_input = model_input, closure_file = "not_a_file"))
})

test_that("ems_model rejects non-character var_omit", {
  expect_snapshot_error(ems_model(model_input = model_input, closure_file = closure_file, var_omit = 1))
})

test_that("ems_model rejects invalid variable names in var_omit", {
  expect_snapshot_error(ems_model(model_input = model_input, closure_file = closure_file, var_omit = "not_a_var"))
})

test_that("ems_model rejects invalid coefficient arguments", {
  expect_snapshot_error(ems_model(model_input = model_input, closure_file = closure_file, NOT_A_COEFF = 2))
})

test_that("ems_model returns valid tibble for GTAPv6", {
  model <- ems_model(model_input = model_input, closure_file = closure_file)
  expect_s3_class(model, "tbl_df")
  expect_true(all(c("tab", "type", "name", "header", "comp1", "comp2") %in% names(model)))
  expect_true(nrow(model) > 0)
  expect_true(!is.null(attr(model, "tab_file")))
})

model_input <- ems_example("GTAP_RE.tab", file.path("models", "GTAP_RE"))
closure_file <- ems_example("GTAP_RE.cls", file.path("models", "GTAP_RE"))
withr::defer(unlink(file.path(dirname(model_input), "error.tab")), teardown_env())

test_that("aggregated numeric to a formula", {
  model <- ems_model(
    model_input = model_input,
    closure_file = closure_file,
    KAPPA = 0.54321
  )

  expect_true(model[grepl("KAPPA", model$comp1), ]$comp2 == 0.54321)
  expect_true(grepl("0.54321", model[grepl("KAPPA", model$comp1), ]$tab))
})

test_that("invalid numeric to a formula", {
  expect_snapshot_error(ems_model(
    model_input = model_input,
    closure_file = closure_file,
    KAPPA = c(0, 1)
  ))
})

test_that("aggregated numeric to a read", {
  model <- ems_model(
    model_input = model_input,
    closure_file = closure_file,
    ETRAQ = -4.321
  )

  expect_true(model[grepl("ETRAQ", model$comp1), ]$comp2 == -4.321)
  expect_true(grepl("-4.321", model[grepl("ETRAQ", model$comp1), ]$tab))
})

test_that("aggregated data frame to a read", {
  set.seed(42)
  COMMc <- c("crops", "food", "livestock", "mnfcs", "svces")
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  SUBPAR <- expand.grid(
    COMMc = COMMc,
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))

  model <- ems_model(
    model_input = model_input,
    closure_file = closure_file,
    SUBPAR = SUBPAR
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(SUBPAR),
    attr(model, "SUBPAR")
  )) == 0)
})

test_that("aggregated csv to a read", {
  set.seed(42)
  temp_dir <- withr::local_tempdir()

  COMMc <- c("crops", "food", "livestock", "mnfcs", "svces")
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)

  SUBPAR <- expand.grid(
    COMMc = COMMc,
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))
  SUBPAR_csv <- file.path(temp_dir, "SUBPAR.csv")
  SUBPAR$Value <- round(SUBPAR$Value, 6)
  write.csv(SUBPAR, SUBPAR_csv, row.names = FALSE)
  model <- ems_model(
    model_input = model_input,
    closure_file = closure_file,
    SUBPAR = SUBPAR_csv
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(SUBPAR),
    attr(model, "SUBPAR")
  )) == 0)
})

test_that("aggregated data frame to a formula", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  CPHI <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )

  CPHI$Value <- runif(nrow(CPHI))
  model <- ems_model(
    model_input = model_input,
    closure_file = closure_file,
    CPHI = CPHI
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(CPHI),
    attr(model, "CPHI")
  )) == 0)
})

test_that("aggregated csv to a formula", {
  set.seed(42)
  temp_dir <- withr::local_tempdir()

  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  CPHI <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )

  CPHI$Value <- runif(nrow(CPHI))
  CPHI_csv <- file.path(temp_dir, "CPHI.csv")
  CPHI$Value <- round(CPHI$Value, 2)
  write.csv(CPHI, CPHI_csv, row.names = FALSE)
  model <- ems_model(
    model_input = model_input,
    closure_file = closure_file,
    CPHI = CPHI_csv
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(CPHI),
    attr(model, "CPHI")
  )) == 0)
})


test_that("ignored tab statement", {
  wrn_model <- write_modified_model(model_input, "POSTSIM (BEGIN) ;")
  expect_snapshot_warning(ems_model(
    model_input = wrn_model,
    closure_file = closure_file
  ))
})

test_that("invalid tab statement", {
  err_model <- write_modified_model(model_input, "OMIT  a1  a1oct  a1mar  a1_s  a2  a2mar  ;")
  expect_snapshot_error(ems_model(
    model_input = err_model,
    closure_file = closure_file
  ))
})

test_that("invalid intertemporal header", {
  err_model <- write_modified_model(model_input, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  expect_snapshot_error(ems_model(
    model_input = err_model,
    closure_file = closure_file
  ))
})

test_that("valid custom intertemporal header", {
  err_model <- write_modified_model(model_input, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  ems_option_set(timestep_header = "AYRS")
  withr::defer(ems_option_set(timestep_header = "YEAR"))
  expect_s3_class(
    ems_model(
      model_input = err_model,
      closure_file = closure_file
    ),
    "data.frame"
  )
})

test_that("invalid read statement", {
  err_model <- write_modified_model(model_input, "Read but no file;")
  expect_snapshot_error(ems_model(
    model_input = err_model,
    closure_file = closure_file
  ))
})

test_that("invalid binary set switch statement", {
  err_model <- write_modified_model(
    model_input,
    "Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);"
  )
  expect_snapshot_error(ems_model(
    model_input = err_model,
    closure_file = closure_file
  ))
})

test_that("identical set assignment", {
  err_model <- write_modified_model(model_input, "Set ENDWM2 # mobile endowments 2 # = ENDWM;")
  expect_snapshot_error(ems_model(
    model_input = err_model,
    closure_file = closure_file
  ))
})

test_that("invalid set qualifier", {
  err_model <- write_modified_model(
    model_input,
    "Set (static) ENDTIME # End time step # size 1 (P[NTSP-1]);"
  )
  expect_snapshot_error(ems_model(
    model_input = err_model,
    closure_file = closure_file
  ))
})

test_that("multiple set operators", {
  err_model <- write_modified_model(
    model_input,
    "Set ENDWCFS # multiple op # = ENDWC + ENDWF + ENDWS;"
  )
  expect_snapshot_error(ems_model(
    model_input = err_model,
    closure_file = closure_file
  ))
})

# to be determined
# test_that("unrecognized tab statement", {
#   model <- readChar(
#     model_input,
#     file.info(model_input)[["size"]]
#   )
#
#   invalid <- "Unrecognized statement;"
#   model <- paste(model, invalid)
#
#   temp_dir <- tools::R_user_dir("teems", "data")
#   if (!dir.exists(temp_dir)) {
#     dir.create(temp_dir)
#   }
#
#   err_model <- file.path(dirname(model_input), "error.tab")
#   writeLines(model, err_model)
#   expect_snapshot_error(ems_model(model_input = err_model,
#                                   closure_file = closure_file))
# })

# test_that("swap err", {
#   expect_snapshot_error(ems_model(model_input = "GTAPv6.2", swap_in = "not_a_var"))
#   wrong_set <- teems_swap(var = "pop", REG = "lam")
#   expect_snapshot_error(ems_model(model_input = "GTAPv6.2", swap_in = wrong_set))
#   swap <- teems_swap(var = "tfd")
#   expect_snapshot(ems_model(model_input = "GTAPv6.2", swap_in = swap))
#   expect_snapshot(ems_model(model_input = "GTAPv6.2", swap_in = "qgdp"))
#   expect_snapshot(ems_model(model_input = "GTAPv6.2", swap_in = list("qgdp", swap)))
# })

# test_that("cls err", {
#   cls_file <- readLines("data-raw/closures/GTAPv6.2/GTAPv6.2.cls")
#   cls_file[2] <- "not_a_var"
#   writeLines(cls_file, "/tmp/cls_file.cls")
#   ems_model(model_input = "GTAPv6.2", closure_file = "/tmp/cls_file.cls")
#   unlink("/tmp/cls_file.cls")
# })

# test_that("shk err", {
#   rndm_lst <- list(a = "rndm")
#   expect_snapshot_error(ems_model(model_input = "GTAPv6.2", shock = rndm_lst))
#   not_a_var <- teems_shock(var = "not_a_var", type = "uniform", input = 1)
#   expect_snapshot_error(ems_model(model_input = "GTAPv6.2", shock = not_a_var))
#   df <- data.frame(REGr = "lam", ALLTIMEt = 0, Year = 2014, Value = 1)
#   extra_col <- teems_shock(var = "pop", type = "custom", input = df)
#   expect_snapshot_error(ems_model(model_input = "GTAP-INTv1", shock = extra_col))
#   not_a_set_uni <- teems_shock(var = "afeall", type = "uniform", input = 1, REG = "lam", Year = 2014, PROD_COMMj = "zcgds")
#   expect_snapshot_error(ems_model(model_input = "GTAP-INTv1", shock = not_a_set_uni))
#   df <- data.frame(REG = "lam", PROD_COMMj = "zcgds", Year = 2014, Value = 1)
#   not_a_set_cust <- teems_shock(var = "afeall", type = "custom", input = df)
#   expect_snapshot_error(ems_model(model_input = "GTAP-INTv1", shock = not_a_set_cust))
#   uni_shk <- teems_shock(var = "afeall", type = "uniform", input = 1, REGr = "lam", PROD_COMMj = "zcgds")
# })
