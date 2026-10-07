skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "deploy")
temp_dir <- file.path(write_dir, "tmp")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(temp_dir, recursive = TRUE)
ems_option_set(
  verbose = FALSE,
  tempdir = write_dir
)
withr::defer(ems_option_reset(), teardown_env())

model <- "GTAP-RE"
model_files <- ems_example(model, write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]
dat <- ems_data(
  dat_input,
  par_input,
  set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

model <- ems_model(model_file, closure_file, ignore_condense = TRUE)

test_that("ems_deploy errors when .data is missing", {
  expect_snapshot_error(ems_deploy())
})

test_that("ems_deploy errors when model is missing", {
  expect_snapshot_error(ems_deploy(dat))
})

test_that("ems_deploy returns character path to CMF file", {
  nest_temp("deploy_test", write_dir)
  cmf_path <- ems_deploy(dat, model)
  expect_type(cmf_path, "character")
})

test_that("ems_deploy returns path to an existing CMF file", {
  nest_temp("deploy_test2", write_dir)
  cmf_path <- ems_deploy(dat, model)
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts ems_swap variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  swap_in <- ems_swap("qfd")
  swap_out <- ems_swap("tfd")
  nest_temp("deploy_swap", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, swap_in, swap_out)
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts direct input full variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  nest_temp("deploy_swap2", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, "qfd", "tfd")
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap full variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  swap_in <- ems_swap("yp")
  swap_out <- ems_swap("dppriv")
  nest_temp("deploy_swap3", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, list(swap_in, "qfd"), list(swap_out, "tfd"))
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap partial variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  swap_in <- ems_swap("yp", REGr = "row")
  swap_out <- ems_swap("dppriv", REGr = "row")
  nest_temp("deploy_swap4", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, list(swap_in, "qfd"), list(swap_out, "tfd"))
  expect_true(file.exists(cmf_path))
})

test_that("successive partial swap-outs of one variable reduce cleanly", {
  shk <- ems_uniform_shock("qfd", 1)
  swap_in <- list(ems_swap("yp", REGr = "row"), ems_swap("yp", REGr = "chn"), "qfd")
  swap_out <- list(ems_swap("dppriv", REGr = "row"), ems_swap("dppriv", REGr = "chn"), "tfd")
  nest_temp("deploy_swap_chain", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, swap_in, swap_out)
  cls <- readLines(file.path(dirname(cmf_path), "GTAP-RE.cls"))
  expect_true(any(grepl("^dppriv\\(\"usa\"", cls)))
  expect_false(any(grepl("^dppriv\\(\"row\"|^dppriv\\(\"chn\"|^dppriv$", cls)))
})

test_that("ems_deploy folds element case in swaps", {
  shk <- ems_uniform_shock("qfd", 1)
  nest_temp("deploy_case_lower", write_dir)
  cmf_lower <- ems_deploy(
    dat, model, shk,
    list(ems_swap("yp", REGr = "row"), "qfd"),
    list(ems_swap("dppriv", REGr = "row"), "tfd")
  )
  nest_temp("deploy_case_upper", write_dir)
  cmf_upper <- ems_deploy(
    dat, model, shk,
    list(ems_swap("yp", REGr = "ROW"), "qfd"),
    list(ems_swap("dppriv", REGr = "Row"), "tfd")
  )
  expect_identical(
    readLines(file.path(dirname(cmf_upper), basename(closure_file))),
    readLines(file.path(dirname(cmf_lower), basename(closure_file)))
  )
})

test_that("ems_deploy folds element case in closure entries", {
  static_dat <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  v7 <- ems_example("GTAPv7", write_dir)
  cls <- readLines(v7[["closure_file"]])
  lower_cls <- file.path(write_dir, "lower.cls")
  upper_cls <- file.path(write_dir, "upper.cls")
  writeLines(
    sub("^pop$", 'pop("chn")\npop("usa")\npop("row")', cls),
    lower_cls
  )
  writeLines(
    sub("^pop$", 'pop("CHN")\npop("Usa")\npop("ROW")', cls),
    upper_cls
  )
  m_lower <- suppressMessages(suppressWarnings(
    ems_model(v7[["model_file"]], lower_cls, ignore_condense = TRUE)
  ))
  m_upper <- suppressMessages(suppressWarnings(
    ems_model(v7[["model_file"]], upper_cls, ignore_condense = TRUE)
  ))
  nest_temp("deploy_cls_lower", write_dir)
  cmf_lower <- ems_deploy(static_dat, m_lower)
  nest_temp("deploy_cls_upper", write_dir)
  cmf_upper <- ems_deploy(static_dat, m_upper)
  expect_identical(
    readLines(file.path(dirname(cmf_upper), "upper.cls")),
    readLines(file.path(dirname(cmf_lower), "lower.cls"))
  )
  expect_true(any(grepl('pop("usa")', readLines(file.path(dirname(cmf_upper), "upper.cls")), fixed = TRUE)))
})

test_that("ems_deploy errors when invalid variable provided for swap-in", {
  nest_temp("invalid_swap", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, swap_in = "not_a_var", swap_out = "tfd")
  )
})

test_that("ems_deploy errors when invalid variable provided for swap-out", {
  nest_temp("invalid_swap2", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, swap_in = "qfd", swap_out = "not_a_var")
  )
})

test_that("ems_deploy errors when shock_file and shock are both provided", {
  shk <- ems_uniform_shock("pop", 1)
  nest_temp("deploy_shk_file", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, shk, shock_file = "fake.shf")
  )
})

test_that("write_coefficients must be a logical scalar", {
  nest_temp("deploy_write_coefficients_arg", write_dir)
  expect_snapshot_error(ems_deploy(dat, model, write_coefficients = NA))
  expect_error(
    ems_deploy(dat, model, write_coefficients = c(TRUE, FALSE)),
    class = "rlang_error"
  )
  expect_error(
    ems_deploy(dat, model, write_coefficients = "yes"),
    class = "rlang_error"
  )
})

test_that("coefficient CSV Write pairs are opt-in (default off)", {
  nest_temp("deploy_write_coefficients_off", write_dir)
  cmf <- ems_deploy(dat, model)
  tab <- readLines(attr(cmf, "tab_path"))
  cmf_lines <- readLines(cmf)
  # PostSim coefficients get their CSV pair inside the PostSim section
  n_coeff <- sum(model$type == "Coefficient")
  # sets still get their Write pairs and outdata lines
  expect_true(any(grepl("^Write \\(set\\) REG to file REG ", tab)))
  expect_true(any(grepl("^outdata \"REG\"", cmf_lines)))
  # coefficients do not
  expect_false(any(grepl("^Write SAVE to file SAVE ", tab)))
  expect_false(any(grepl("out/coefficients/", cmf_lines, fixed = TRUE)))
  expect_lt(sum(grepl("^outdata ", cmf_lines)), n_coeff)

  nest_temp("deploy_write_coefficients_on", write_dir)
  cmf <- ems_deploy(dat, model, write_coefficients = TRUE)
  tab <- readLines(attr(cmf, "tab_path"))
  cmf_lines <- readLines(cmf)
  expect_true(any(grepl("^Write SAVE to file SAVE ", tab)))
  expect_equal(
    sum(grepl("out/coefficients/", cmf_lines, fixed = TRUE)),
    n_coeff
  )
})

test_that("ems_deploy errors when read-in headers not present in data", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data <- mod_data[!names(mod_data) %in% "SAVE"]
  nest_temp("deploy_missing_hdr", write_dir)
  expect_snapshot_error(
    ems_deploy(mod_data, model)
  )
})

test_that("ems_deploy errors when read-in headers are missing mapping", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data <- mod_data[!names(mod_data) %in% "REG"]
  nest_temp("deploy_missing_map", write_dir)
  expect_snapshot_error(ems_deploy(mod_data, model))
})

test_that("ems_deploy errors when timesteps provided to static model", {
  write_dir <- file.path(write_dir, "gtapv7")
  dir.create(write_dir, recursive = TRUE, showWarnings = FALSE)
  model_files <- ems_example("GTAPv7", write_dir)
  model_file <- model_files[["model_file"]]
  closure_file <- model_files[["closure_file"]]
  model <- ems_model(model_file, closure_file)
  nest_temp("deploy_static_ts", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_deploy errors when timesteps not provided to a dynamic model", {
  static_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  nest_temp("deploy_dynamic_no_ts", write_dir)
  expect_snapshot_error(ems_deploy(static_data, model))
})

test_that("ems_deploy errors when set-calculated number of entries does not match a finalized data header", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data$REG[.N, mapping := "row"]
  nest_temp("deploy_set_mismatch", write_dir)
  expect_snapshot_error(ems_deploy(mod_data, model))
})

test_that("an unlabelled header is dimensioned from its reading coefficient", {
  arr <- array(c(1L, 2L, 3L, 4L, 5L, 6L), dim = c(3L, 2L))
  class(arr) <- c("MEXP", "dat", "generic", class(arr))
  dt <- .array2DT(list(MEXP = arr))[[1]]
  expect_identical(attr(dt, "positional_dim"), c(3L, 2L))
  agg <- .aggregate_data(dt, sets = list(), ndigits = 6L)
  expect_identical(agg$Value, 1:6)

  unl_model <- tibble::tibble(
    type = c("Coefficient", "Read"), name = c("MEXP", "MEXP"),
    header = c("MEXP", "MEXP"), ls_upper_idx = list(c("COM", "EXP"), NA)
  )
  set_ele <- list(COM = c("a", "b", "c"), EXP = c("x", "y"), EXP2 = c("x", "y", "z"))
  out <- .dimension_positional(agg, unl_model, set_ele, call = NULL)
  expect_identical(colnames(out), c("COM", "EXP", "Value"))
  expect_identical(out$COM, rep(c("a", "b", "c"), 2L))
  expect_identical(out$EXP, rep(c("x", "y"), each = 3L))
  expect_identical(out$Value, 1:6)
  expect_identical(class(out)[1], "MEXP")

  unl_model$ls_upper_idx[[1]] <- c("COM", "EXP2")
  expect_snapshot_error(.dimension_positional(agg, unl_model, set_ele, call = NULL))
})

test_that("data headers match the TAB's header spelling case-insensitively", {
  d1 <- data.table::data.table(Value = 1)
  class(d1) <- c("P21h", "dat", class(d1))
  d2 <- data.table::data.table(Value = 2)
  class(d2) <- c("XPLH", "dat", class(d2))
  out <- .canonical_headers(list(P21h = d1, XPLH = d2), c("P21H", "XPLh", "OTHR"))
  expect_identical(names(out), c("P21H", "XPLh"))
  expect_identical(class(out$P21H)[1], "P21H")
  expect_identical(class(out$XPLh)[1], "XPLh")
})

test_that("set builders over Formula coefficients deploy with their indicators", {
  nest_temp("deploy_formula_builders", write_dir)
  fb_model <- ems_model(
    write_modified_model(
      model_file,
      paste(
        'Set CMPW # positive domestic value in chn # = (all,c,COMM: sum{t,ALLTIME, VDB(c,"chn",t)} > 0);',
        "Set CMPZ # after the second commodity # = (all,c,COMM: $pos(c) > 2 and not $pos(c) > 4);",
        sep = "\n"
      )
    ),
    closure_file,
    ignore_condense = TRUE
  )
  cmf <- ems_deploy(dat, fb_model)
  data_lines <- readLines(file.path(dirname(cmf), "GTAPDATA.txt"))
  block <- function(h) {
    at <- grep(sprintf('Header "%s"', h), data_lines, fixed = TRUE)
    n <- as.integer(strsplit(data_lines[at], " ")[[1]][1])
    as.numeric(data_lines[at + seq_len(n)])
  }
  comm <- unique(dat$COMM$mapping)
  expect_identical(block("SB02"), as.numeric(seq_along(comm) %in% 3:4))
  expect_true(sum(block("SB01")) > 0)
  tab <- readLines(file.path(dirname(cmf), basename(attr(fb_model, "tab_file"))))
  expect_true(any(grepl("= (all,c,COMM: SBI02(c) > 0.5);", tab, fixed = TRUE)))
})

test_that("ems_deploy errors when aggregated inputs are incomplete", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  SAVE <- mod_data$SAVE[!.N, ]
  SAVE$ALLTIMEt <- 0
  colnames(SAVE)[1] <- "REGr"
  model <- ems_model(model_file, closure_file, SAVE = SAVE)
  nest_temp("deploy_incomplete", write_dir)
  expect_snapshot_error(ems_deploy(mod_data, model))
})

test_that("ems_deploy without a shock announces the null shock", {
  nest_temp("null_shock", write_dir)
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_set(verbose = FALSE))
  msgs <- capture_messages(ems_deploy(dat, model))
  expect_match(msgs, "No shock has been provided", fixed = TRUE, all = FALSE)
})

test_that("ems_deploy accepts a shock file", {
  shock <- "Shock pfactwld(ALLTIME) = uniform 1;\n"
  temp <- tempfile(tmpdir = temp_dir, fileext = ".shf")
  cat(shock, file = temp)
  nest_temp("shock_file", write_dir)
  cmf_path <- ems_deploy(dat, model, shock_file = temp)
  outputs <- ems_solve(cmf_path)
  expect_all_true(abs(outputs$dat$pfactwld$Value - 1) < 1e-6)
})

test_that("ems_deploy examples work", {
  # The following examples require input data. See
  # https://teemsphere.github.io/ to get started.

  # Uniform shock applied to full variable with value 1
  shock <- ems_uniform_shock("qfd", value = 1)

  # Full variable swap, qfd for tfd
  cmf_path <- ems_deploy(
    .data = dat,
    model = model,
    shock = shock,
    swap_in = "qfd",
    swap_out = "tfd"
  )

  expect_true(is.character(cmf_path))
  # Partial variable swap (element "row" of set REG with index r)
  yp_row <- ems_swap("yp", REGr = "row")
  dppriv_row <- ems_swap("dppriv", REGr = "row")

  # Swap part of yp for part of dppriv; qfd for tfd
  subdir <- "examples"
  nest_temp(subdir, write_dir)
  cmf_path <- ems_deploy(
   .data = dat,
   model = model,
   shock = shock,
   swap_in = list(yp_row, "qfd"),
   swap_out = list(dppriv_row, "tfd")
  )
  expect_true(is.character(cmf_path))
})


unlink(write_dir, recursive = TRUE)
