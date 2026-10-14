skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "compose")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
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
n_var <- sum(model$type == "Variable")
# PostSim coefficients compose as type "postsim"
n_coeff <- sum(model$type == "Coefficient" & !model$postsim)
n_postsim <- sum(model$type == "Coefficient" & model$postsim)
cmf_path <- ems_deploy(dat, model)
ems_solve(cmf_path, suppress_outputs = TRUE)


test_that("ems_compose errors when cmf_path is missing", {
  expect_snapshot_error(ems_compose())
})

test_that("ems_compose errors when which is not character", {
  expect_snapshot_error(ems_compose(cmf_path, which = 1))
})

test_that("ems_compose errors when invalid which", {
  expect_snapshot_error(ems_compose(cmf_path, which = "not_a_var"))
})

test_that("ems_compose errors when passes is not logical", {
  expect_snapshot_error(ems_compose(cmf_path, passes = "yes"))
})

test_that("which matches variable and coefficient names case-insensitively and returns the canonical names (R6-B3)", {
  upper <- ems_compose(cmf_path, which = c("QGDP", "vdfb"))
  lower <- ems_compose(cmf_path, which = c("qgdp", "VDFB"))
  expect_identical(upper$name, lower$name)
  expect_setequal(upper$name, c("qgdp", "VDFB"))
})

test_that("which must name at least one output and passes must be a flag (R6-B8)", {
  expect_snapshot_error(ems_compose(cmf_path, which = character(0)))
  expect_snapshot_error(ems_compose(cmf_path, passes = NA))
  expect_snapshot_error(ems_compose(cmf_path, passes = c(TRUE, FALSE)))
})

test_that("passes = TRUE with only coefficients in which aborts (R6-B8)", {
  expect_snapshot_error(ems_compose(cmf_path, which = "VDFB", passes = TRUE))
})

test_that("ems_compose returns tibble for which = 'all'", {
  result <- ems_compose(cmf_path)
  expect_s3_class(result, "tbl")
})

test_that("ems_compose variable output contains expected elements", {
  result <- ems_compose(cmf_path)
  expect_equal(nrow(result), n_var + n_coeff + n_postsim)
})

test_that("ems_compose selects a variable", {
  result <- ems_compose(cmf_path, "qfd")
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose selects a coefficient", {
  result <- ems_compose(cmf_path, "SAVE")
  SAVE_out <- result$dat$SAVE[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(SAVE_out) <- colnames(dat$SAVE)
  expect_true(all.equal(SAVE_out, dat$SAVE, check.attributes = FALSE, tolerance = 1e-7))
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose selects multiple coefficients", {
  result <- ems_compose(cmf_path, c("SAVE", "VTMFSD"))
  SAVE_out <- result$dat$SAVE[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(SAVE_out) <- colnames(dat$SAVE)
  expect_true(all.equal(SAVE_out, dat$SAVE, check.attributes = FALSE, tolerance = 1e-7))
  VTWR_out <- result$dat$VTMFSD[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(VTWR_out) <- colnames(dat$VTWR)
  expect_true(all.equal(VTWR_out, dat$VTWR, check.attributes = FALSE, tolerance = 1e-7))
  expect_s3_class(result, "data.frame")
})

test_that("ems_compose selects variable/coefficient mix", {
  result <- ems_compose(cmf_path, c("qtmfsd", "SAVE"))
  SAVE_out <- result$dat$SAVE[ALLTIMEt == 0, !c("ALLTIMEt", "Year")]
  colnames(SAVE_out) <- colnames(dat$SAVE)
  expect_true(all.equal(SAVE_out, dat$SAVE, check.attributes = FALSE, tolerance = 1e-7))
  expect_all_true(result$dat$qtmfsd$Value == 0)
  expect_s3_class(result, "data.frame")
})

test_that("coefficients come from the binary dump; CSVs are opt-in", {
  bin_dir <- file.path(dirname(cmf_path), "out", "variables", "bin")
  expect_true(file.exists(file.path(bin_dir, "sol.cof")))
  expect_true(file.exists(file.path(bin_dir, "sol.cbin")))
  expect_length(
    list.files(file.path(dirname(cmf_path), "out", "coefficients")),
    0L
  )
  from_bin <- ems_compose(cmf_path)
  expect_equal(sum(from_bin$type == "coefficient"), n_coeff)
})

test_that("the solver lists its outputs and the pre-simulation coefficients read back", {
  bin_dir <- file.path(dirname(cmf_path), "out", "variables", "bin")
  sol_prefix <- file.path(bin_dir, "sol.")
  outputs <- jsonlite::fromJSON(paste0(sol_prefix, "outputs.json"))
  expect_true(outputs$complete)
  expect_true(all(c("sol.bin", "sol.var", "sol.set", "sol.sel", "sol.mds",
                    "sol.cof", "sol.cbin", "sol.cbin0", "sol.xac",
                    "sol.stats.json") %in% outputs$files$name))
  expect_true(.has_coefficient_dump(sol_prefix, presim = TRUE))
  post <- .parse_coefficient_bins(sol_prefix)
  pre <- .parse_coefficient_bins(sol_prefix, presim = TRUE)
  expect_identical(pre$cof_union, post$cof_union)
  expect_identical(nrow(pre$xc), nrow(post$xc))
  params <- post$cof_union$r_idx[post$cof_union$parameter &
    !post$cof_union$postsim]
  rows <- unlist(lapply(params + 1L, function(i) {
    post$cof_union$pack_begadd[i] + seq_len(post$cof_union$matsize[i])
  }))
  expect_gt(length(rows), 0L)
  expect_identical(pre$xc$Value[rows], post$xc$Value[rows])
  unlink(paste0(sol_prefix, "outputs.json"))
  expect_false(.has_coefficient_dump(sol_prefix, presim = TRUE))
})

test_that("ems_compose errors when cmf_path does not exist", {
  expect_snapshot_error(ems_compose(cmf_path = file.path("not_a_path")))
})

test_that("ems_compose examples run", {
  # The following examples require that a model run has taken
  # place. See https://teemsphere.github.io/ to get started.

  # Return specific variables and/or coefficients by name
  outputs <- ems_compose(cmf_path)
  
  # Specific variables and/or coefficients by name
  outputs <- ems_compose(cmf_path, c("qfd", "EVFP"))
  expect_equal(nrow(outputs), 2)
  expect_equal(ncol(outputs), 4)
  expect_equal(names(outputs), c("name", "label", "type", "dat"))
  expect_equal(names(outputs$dat), c("qfd", "EVFP"))
})

test_that("time steps read from the deploy metadata match the solver's timestep coefficient", {
  paths <- .get_output_paths(cmf_path = cmf_path)
  from_metadata <- .get_timesteps(
    paths = paths, cmf_path = cmf_path,
    timestep_header = .o_timestep_header(), call = NULL
  )
  # a deployment written before the metadata carried them falls back to
  # the coefficient the solver wrote out
  legacy <- readRDS(paths$metadata)
  legacy$time_steps <- NULL
  paths$metadata <- withr::local_tempfile(fileext = ".rds")
  saveRDS(legacy, paths$metadata)
  from_coefficient <- .get_timesteps(
    paths = paths, cmf_path = cmf_path,
    timestep_header = .o_timestep_header(), call = NULL
  )
  expect_equal(from_metadata, from_coefficient, ignore_attr = TRUE)
  expect_equal(from_metadata$CYRS - from_metadata$CYRS[1], c(0, 1, 2))
})

test_that("a run without a coefficient dump composes variables only", {
  # the run directory is not used again: the next test removes its outputs
  unlink(file.path(write_dir, "out", "variables", "bin", c("sol.cof", "sol.cbin")))
  expect_snapshot_error(ems_compose(cmf_path))
  expect_error(ems_compose(cmf_path, c("qfd", "SAVE")), "No coefficient dump", fixed = TRUE)
  result <- ems_compose(cmf_path, "qfd")
  expect_setequal(result$type, "variable")
  expect_true("Year" %in% names(result$dat$qfd))
})

test_that("ems_compose errors when model run has not taken place", {
  unlink(file.path(write_dir, "out"), recursive = TRUE)
  expect_snapshot(ems_compose(cmf_path),
                  error = TRUE, transform = scrub_paths)
})

test_that("variables with an ORIG_LEVEL carry their pre- and post-simulation levels", {
  nest_temp("compose_levels_null", write_dir)
  null_cmf <- ems_deploy(dat, model)
  out <- ems_solve(null_cmf, solution_method = "Johansen")
  vars <- out[out$type == "variable", ]
  qfd <- vars$dat[["qfd"]]
  expect_true(all(c("PreLevel", "PostLevel", "Change") %in% names(qfd)))
  vdfb <- out$dat[[which(out$name == "VDFB" & out$type == "coefficient")]]
  keys <- setdiff(names(vdfb), c("Value", "Year"))
  joined <- merge(qfd, vdfb, by = keys, suffixes = c("", ".coef"))
  expect_equal(nrow(joined), nrow(qfd))
  expect_equal(joined$PreLevel, joined$Value.coef, tolerance = 1e-6)
  expect_true(all(vars$dat[["pds"]]$PreLevel == 1))
  expect_false("PreLevel" %in% names(vars$dat[["del_taxrgc"]]))
})

test_that("levels follow the solution in a shocked run", {
  nest_temp("compose_levels_shock", write_dir)
  shocked <- ems_deploy(dat, model, ems_uniform_shock("aoall", 2))
  out <- ems_solve(shocked, solution_method = "Johansen")
  for (nm in c("qfd", "pds", "qgdp")) {
    d <- out$dat[[nm]]
    expect_equal(d$PostLevel, d$PreLevel * (1 + d$Value / 100), tolerance = 1e-9, label = nm)
    expect_equal(d$Change, d$PostLevel - d$PreLevel, tolerance = 1e-9, label = nm)
    expect_true(any(d$Value != 0), label = nm)
  }
})

test_that("passes = TRUE adds the three separate solutions, which extrapolate to Value", {
  nest_temp("compose_passes", write_dir)
  shocked <- ems_deploy(dat, model, ems_uniform_shock("aoall", 2))
  steps <- c(2L, 4L, 6L)
  ems_solve(shocked, solution_method = "Gragg", steps = steps, suppress_outputs = TRUE)
  out <- ems_compose(shocked, c("qgdp", "qfd"), passes = TRUE)
  x <- 1 / steps^2
  w <- vapply(1:3, \(i) prod(x[-i] / (x[-i] - x[i])), numeric(1))
  for (nm in c("qgdp", "qfd")) {
    d <- out$dat[[nm]]
    expect_true(all(c("Pass1", "Pass2", "Pass3", "Error", "Figures", "LevelsFigures", "Code", "Summarised") %in% names(d)), label = nm)
    expect_true(all(d$Code %in% c("XMA", "XRA", "C0", "FC0", "CX", "FCX", "MVC", "MN0", "OVC", "ON0", "MC?", "OC?", "MD?", "OD?", "MD!", "OD!")), label = nm)
    expect_true(all(d$Figures %in% c(0:6, NA)), label = nm)
    expect_true(all(d$LevelsFigures >= d$Figures | is.na(d$Figures)), label = nm)
    expect_true(all(d$Summarised), label = nm)
    expect_true(all(d$Error >= 0), label = nm)
    expect_true(any(d$Pass1 != d$Value), label = nm)
    extrapolated <- w[1] * d$Pass1 + w[2] * d$Pass2 + w[3] * d$Pass3
    expect_equal(extrapolated, d$Value, tolerance = 1e-9, label = nm)
  }
  acc <- attr(out, "accuracy")
  expect_setequal(acc$Variable, c("qgdp", "qfd"))
  expect_true(all(c("Elements", "Summarised", "MinFigures", "MinLevelsFigures", "ShareFigures",
    "ShareLevelsFigures", "WorstCode", "MaxError") %in% names(acc)))
  qfd <- out$dat[[match("qfd", out$name)]]
  row <- acc[acc$Variable == "qfd", ]
  expect_identical(row$Elements, nrow(qfd))
  expect_identical(row$MinLevelsFigures, min(qfd$LevelsFigures))
  expect_equal(row$MaxError, max(qfd$Error))
  expect_true(all(acc$ShareFigures >= 0 & acc$ShareFigures <= 1))
  plain <- ems_compose(shocked, "qgdp")
  expect_false(any(c("Pass1", "Figures") %in% names(plain$dat$qgdp)))
})

test_that("passes = TRUE on a run without separate solutions aborts", {
  nest_temp("compose_passes_johansen", write_dir)
  johansen <- ems_deploy(dat, model)
  ems_solve(johansen, solution_method = "Johansen", suppress_outputs = TRUE)
  expect_snapshot_error(ems_compose(johansen, "qgdp", passes = TRUE))
})

test_that("a model typed with all three VPQType forms deploys and solves", {
  nest_temp("compose_vpqtype", write_dir)
  text <- readLines(model_file)
  first_var <- grep("^Variable", text)[1]
  text <- c(
    text[seq_len(first_var - 1L)],
    "Variable (begins p default VPQType Price);",
    "Variable (begins q default VPQType Quantity);",
    "Variable (Name pop VPQType Quantity);",
    text[first_var:length(text)]
  )
  text <- sub("^Variable \\(orig_level=1.0\\)", "Variable (orig_level=1.0, VPQType=Price)", text)
  typed_file <- file.path(write_dir, "compose_vpqtype", "typed.tab")
  writeLines(text, typed_file)
  typed <- ems_model(typed_file, closure_file, ignore_condense = TRUE)
  vpq <- attr(typed, "vpqtype")
  expect_identical(unname(vpq[c("pds", "qfd", "pop")]), c("price", "quantity", "quantity"))
  out <- ems_solve(ems_deploy(dat, typed), solution_method = "Johansen")
  expect_s3_class(out, "tbl_df")
})

unlink(write_dir, recursive = TRUE)
