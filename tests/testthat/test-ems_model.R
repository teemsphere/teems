ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

write_modified_model <- function(model_file, text, .fn = paste) {
  model_text <- readChar(model_file, file.info(model_file)[["size"]])
  modified <- .fn(model_text, text)
  out_path <- file.path(dirname(model_file), "error.tab")
  writeLines(modified, out_path)
  out_path
}

write_modified_closure <- function(closure_file, text, .fn = cat) {
  closure_text <- readLines(closure_file)
  modified <- capture.output(.fn(closure_text[[1]], text, tail(closure_text, -1), sep = "\n"))
  out_path <- file.path(dirname(closure_file), "error.cls")
  writeLines(modified, out_path)
  out_path
}

write_dir <- file.path(tools::R_user_dir(package = "teems", which = "data"), "model")

if (dir.exists(write_dir)) { 
  unlink(list.dirs(write_dir, recursive = FALSE), recursive = TRUE)
} else {
  dir.create(write_dir, recursive = TRUE)
}

model_file <- ems_example("GTAP-RE", write_dir = write_dir)[["model_file"]]
closure_file <- ems_example("GTAP-RE", write_dir = write_dir)[["closure_file"]]

# general data
dat_input <- Sys.getenv("GTAP11c_dat")
par_input <- Sys.getenv("GTAP11c_par")
set_input <- Sys.getenv("GTAP11c_set")

dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

test_that("ems_model requires both model_file and closure_file", {
  expect_snapshot_error(ems_model())
})

test_that("ems_model requires closure_file when only model_file provided", {
  expect_snapshot_error(ems_model(model_file = model_file))
})

test_that("ems_model requires model_file when only closure_file provided", {
  expect_snapshot_error(ems_model(closure_file = closure_file))
})

test_that("ems_model rejects non-character model_file", {
  expect_snapshot_error(ems_model(model_file = 1, closure_file = closure_file))
})

test_that("ems_model rejects non-character closure_file", {
  expect_snapshot_error(ems_model(model_file = model_file, closure_file = TRUE))
  expect_snapshot_error(ems_model(model_file = model_file, closure_file = 1))
})

test_that("ems_model rejects non-existent model_file file", {
  expect_snapshot_error(ems_model(model_file = "not_a_file", closure_file = closure_file))
})

test_that("ems_model rejects non-existent closure_file", {
  expect_snapshot_error(ems_model(model_file = model_file, closure_file = "not_a_file"))
})

test_that("ems_model rejects non-character var_omit", {
  expect_snapshot_error(ems_model(model_file = model_file, closure_file = closure_file, var_omit = 1))
})

test_that("ems_model rejects invalid variable names in var_omit", {
  expect_snapshot_error(ems_model(model_file = model_file, closure_file = closure_file, var_omit = "not_a_var"))
})

test_that("ems_model rejects invalid coefficient arguments", {
  expect_snapshot_error(ems_model(model_file = model_file, closure_file = closure_file, NOT_A_COEFF = 2))
})

test_that("ems_model returns valid tibble", {
  model <- ems_model(model_file = model_file, closure_file = closure_file)
  expect_s3_class(model, "tbl_df")
  expect_true(all(c("tab", "type", "name", "header", "comp1", "comp2") %in% names(model)))
  expect_true(nrow(model) > 0)
  expect_true(!is.null(attr(model, "tab_file")))
})

test_that("aggregated numeric to a formula", {
  model <- ems_model(
    model_file = model_file,
    closure_file = closure_file,
    KAPPA = 0.54321
  )

  expect_true(model[grepl("KAPPA", model$comp1), ]$comp2 == 0.54321)
  expect_true(grepl("0.54321", model[grepl("KAPPA", model$comp1), ]$tab))
})

test_that("invalid numeric to a formula", {
  expect_snapshot_error(ems_model(
    model_file = model_file,
    closure_file = closure_file,
    KAPPA = c(0, 1)
  ))
})

test_that("aggregated numeric to a read", {
  model <- ems_model(
    model_file = model_file,
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
    model_file = model_file,
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
    model_file = model_file,
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
    model_file = model_file,
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
    model_file = model_file,
    closure_file = closure_file,
    CPHI = CPHI_csv
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(CPHI),
    attr(model, "CPHI")
  )) == 0)
})


test_that("ignored tab statement", {
  wrn_model <- write_modified_model(model_file, "POSTSIM (BEGIN) ;")
  expect_snapshot_warning(ems_model(
    model_file = wrn_model,
    closure_file = closure_file
  ))
})

test_that("invalid tab statement", {
  err_model <- write_modified_model(model_file, "OMIT  a1  a1oct  a1mar  a1_s  a2  a2mar  ;")
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("invalid intertemporal header", {
  err_model <- write_modified_model(model_file, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("valid custom intertemporal header", {
  err_model <- write_modified_model(model_file, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  ems_option_set(timestep_header = "AYRS")
  withr::defer(ems_option_set(timestep_header = "YEAR"))
  expect_s3_class(
    ems_model(
      model_file = err_model,
      closure_file = closure_file
    ),
    "data.frame"
  )
})

test_that("invalid read statement", {
  err_model <- write_modified_model(model_file, "Read but no file;")
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("invalid binary set switch statement", {
  err_model <- write_modified_model(
    model_file,
    "Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);"
  )
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("identical set assignment", {
  err_model <- write_modified_model(model_file, "Set ENDWM2 # mobile endowments 2 # = ENDWM;")
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("invalid set qualifier", {
  err_model <- write_modified_model(
    model_file,
    "Set (static) ENDTIME # End time step # size 1 (P[NTSP-1]);"
  )
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("multiple set operators", {
  err_model <- write_modified_model(
    model_file,
    "Set ENDWCFS # multiple op # = ENDWC + ENDWF + ENDWS;"
  )
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("partial read statement", {
  err_model <- write_modified_model(
    model_file,
    'READ (all,i,COM) INTINP(i,"wool") FROM FILE params ; ! a partial read !'
  )
  expect_snapshot_error(ems_model(
    model_file = err_model,
    closure_file = closure_file
  ))
})

test_that("data frame input missing a set", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  SUBPAR <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))

  expect_snapshot_error(ems_model(
    model_file = model_file,
    closure_file = closure_file,
    SUBPAR = SUBPAR
  ))
})

test_that("invalid var in closure", {
  err_closure <- write_modified_closure(
    closure_file,
    "not_a_var"
  )

  expect_snapshot_error(ems_model(
    model_file = model_file,
    closure_file = err_closure
  ))
})

test_that("closure missing exo/endo spec", {
  err_closure <- readLines(closure_file)
  err_closure <- tail(err_closure, -1)
  err_file <- file.path(dirname(closure_file), "error.cls")
  writeLines(err_closure, err_file)
  
  expect_snapshot_error(ems_model(
    model_file = model_file,
    closure_file = err_file
  ))
})

test_that("ems_model errors when invalid closure mixed entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "pop"]
  append_cls <- c('exogenous', 'pop("row",ALLTIME)', 'pop("chn",ALLTIME)', 'pop("zzz",ALLTIME)') 
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)
  
  model <- ems_model(model_file = model_file, closure_file = invalid_cls)
  ems_option_set(write_sub_dir = "invalid_closure")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      write_dir = write_dir
    )
  )
})

test_that("ems_model errors when invalid closure subset entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "qe(ENDWC,REG,INITIME)"]
  append_cls <- c("exogenous", "qe(COMM,REG,INITIME)") 
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)
  
  model <- ems_model(model_file = model_file, closure_file = invalid_cls)
  ems_option_set(write_sub_dir = "invalid_closure")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      write_dir = write_dir
    )
  )
})

test_that("ems_model errors when invalid closure pure element entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "pop"]
  append_cls <- c("exogenous",
                  'pop("row","0")', 'pop("row","1")', 'pop("row","2")',
                  'pop("chn","0")', 'pop("chn","1")', 'pop("chn","2")',
                  'pop("usa","0")', 'pop("usa","1")', 'pop("zzz","2")')
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)

  model <- ems_model(model_file = model_file, closure_file = invalid_cls)
  ems_option_set(write_sub_dir = "invalid_closure")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      write_dir = write_dir
    )
  )
})

test_that("ems_model errors when duplicate closure entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  append_cls <- c("exogenous", "pop") 
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)
  
  model <- ems_model(model_file = model_file, closure_file = invalid_cls)
  ems_option_set(write_sub_dir = "invalid_closure")
  expect_snapshot_error(
    ems_deploy(
      .data = dat,
      model = model,
      write_dir = write_dir
    )
  )
})
