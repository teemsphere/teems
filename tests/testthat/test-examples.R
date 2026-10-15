skip_on_cran()

# Every \dontrun and \donttest example, run as written with real inputs
# in place of the placeholder paths and free variables (cmf_path, dat,
# model), so that a documented call that no longer works fails here.

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "examples")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

v7_subs <- c(
  "v7_data/gsdfdat.har" = dat_input,
  "v7_data/gsdfpar.har" = par_input,
  "v7_data/gsdfset.har" = set_input
)

skip_if_no_solver <- function() {
  skip_if(!nzchar(dat_input), "GTAP data not available")
  skip_if(!.docker_image_present(paste0("teems:", .o_docker_tag())),
          "teems image not available")
}

test_that("ems_option_set, ems_option_get and ems_option_reset examples run", {
  # the examples reset the options; the state this file runs under is put
  # back afterwards
  opts <- ems_option_get()
  withr::defer(do.call(ems_option_set, opts))
  env <- example_env()
  vals <- run_examples(example_exprs("ems_option_set"), env)
  expect_type(vals[["ems_option_get"]], "character")
  vals <- run_examples(example_exprs("ems_option_get"), env)
  expect_type(vals[[1]], "list")
  expect_type(vals[[2]], "integer")
  vals <- run_examples(example_exprs("ems_option_reset"), env)
  gets <- vals[names(vals) == "ems_option_get"]
  expect_length(gets, 2L)
  expect_false(gets[[1]])
  expect_true(gets[[2]])
})

test_that("ems_swap examples run", {
  vals <- run_examples(example_exprs("ems_swap"), example_env())
  expect_length(vals, 8L)
  breadth <- vapply(vals, function(s) class(s[[1]])[2], character(1))
  expect_identical(unname(breadth), rep(c("full", "partial"), c(2L, 6L)))
  depth <- vapply(vals, function(s) class(s[[1]])[1], character(1))
  expect_identical(unname(depth), rep(c("single", "multi", "single"), c(4L, 2L, 2L)))
})

test_that("ems_uniform_shock examples run", {
  vals <- run_examples(example_exprs("ems_uniform_shock"), example_env())
  expect_length(vals, 3L)
  for (s in vals) {
    expect_s3_class(s[[1]], "uniform")
  }
})

test_that("ems_custom_shock examples run", {
  env <- example_env()
  vals <- run_examples(example_exprs("ems_custom_shock"), env)
  shocks <- vals[names(vals) == "ems_custom_shock"]
  expect_length(shocks, 2L)
  for (s in shocks) {
    expect_s3_class(s[[1]], "custom")
  }
  expect_identical(nrow(env$aoall_data), 5L * 5L * 8L)
  expect_identical(env$pop_data$Year, 2023:2030)
})

test_that("ems_scenario_shock examples run", {
  env <- example_env()
  vals <- run_examples(example_exprs("ems_scenario_shock"), env)
  shocks <- vals[names(vals) == "ems_scenario_shock"]
  expect_length(shocks, 2L)
  for (s in shocks) {
    expect_s3_class(s[[1]], "scenario")
    expect_true(s[[1]]$partial)
    expect_identical(nrow(s[[1]]$input), 12L)
  }
  expect_equal(shocks[[1]][[1]]$input$Value, shocks[[2]][[1]]$input$Value)
})

test_that("ems_model examples run", {
  env <- example_env()
  vals <- run_examples(example_exprs("ems_model"), env)
  models <- vals[names(vals) == "ems_model"]
  expect_length(models, 2L)
  for (m in models) {
    expect_s3_class(m, "tbl_df")
    expect_gt(nrow(m), 0L)
  }
})

test_that("ems_example examples run", {
  skip_if(!nzchar(dat_input), "GTAP data not available")
  for (db in c("GTAP12AEZ", "GTAP12E", "GTAP12P")) {
    skip_if(!nzchar(Sys.getenv(paste0(db, "_dat"))),
            paste0(db, "_* inputs not set"))
  }
  script_dir <- file.path(write_dir, "example_scripts")
  dir.create(script_dir)
  layer_subs <- unlist(lapply(
    c(aez_data = "GTAP12AEZ", e_data = "GTAP12E", power_data = "GTAP12P"),
    function(db) {
      Sys.getenv(paste0(db, c("_dat", "_par", "_set")))
    }
  ))
  names(layer_subs) <- paste0(
    rep(c("aez_data", "e_data", "power_data"), each = 3L),
    c("/gsdfdat.har", "/gsdfpar.har", "/gsdfset.har")
  )
  subs <- c(v7_subs, layer_subs, "path/to/example/scripts" = script_dir)
  # the script examples share one directory and overwrite each other's
  # files, so each call's scripts are checked as soon as it returns
  expected_dat <- c(dat_input, dat_input, layer_subs[c(1L, 4L, 7L)], NA)
  env <- example_env()
  n_scripts <- 0L
  for (e in example_exprs("ems_example", subs)) {
    val <- run_examples(list(e), env)
    if (!identical(names(val), "ems_example")) {
      next
    }
    out <- val[[1]]
    expect_true(all(file.exists(out)))
    if (n_scripts == 0L) {
      expect_named(out, c("model_file", "closure_file"))
    } else {
      expect_type(out, "character")
      expect_gt(length(out), 0L)
      dat <- expected_dat[[n_scripts]]
      if (!is.na(dat)) {
        script <- readLines(out[[1]])
        expect_true(any(grepl(dat, script, fixed = TRUE)))
      }
    }
    n_scripts <- n_scripts + 1L
  }
  expect_identical(n_scripts, 7L)
  expect_identical(attr(env$converted$dat, "metadata")$data_format, "GTAPv6")
})

test_that("GTAP_convert examples run", {
  skip_if(!nzchar(dat_input), "GTAP data not available")
  skip_if(!nzchar(Sys.getenv("GTAP9_dat")), "GTAP9_* inputs not set")
  subs <- c(
    "path/to/gsdfdat.har" = dat_input,
    "path/to/gsdfpar.har" = par_input,
    "path/to/gsdfset.har" = set_input,
    v7_subs,
    "v6_data/gsddat.har" = Sys.getenv("GTAP9_dat"),
    "v6_data/gsdpar.har" = Sys.getenv("GTAP9_par"),
    "v6_data/gsdset.har" = Sys.getenv("GTAP9_set")
  )
  env <- example_env()
  run_examples(example_exprs("GTAP_convert", subs), env)
  for (obj in c("ls_arrays", "converted2v6", "converted2v7")) {
    expect_named(env[[obj]], c("dat", "par", "set"))
  }
  expect_true("VDFB" %in% names(env$ls_arrays$dat))
  expect_identical(attr(env$converted2v6$dat, "metadata")$data_format, "GTAPv6")
  expect_identical(attr(env$converted2v7$dat, "metadata")$data_format, "GTAPv7")
})

test_that("ems_data examples run", {
  skip_if(!nzchar(dat_input), "GTAP data not available")
  skip_if(!nzchar(Sys.getenv("ORANIG_har")), "ORANIG_har not set")
  endw_map <- file.path(write_dir, "endowment_mapping.csv")
  endw <- c("ag_othlowsk", "capital", "clerks", "land", "natlres",
            "off_mgr_pros", "service_shop", "tech_aspros")
  write.csv(
    data.frame(ENDW = endw,
               mapping = ifelse(endw %in% c("capital", "land", "natlres"),
                                endw, "labor")),
    endw_map,
    row.names = FALSE
  )
  subs <- c(
    v7_subs,
    "oranig/basedata.har" = Sys.getenv("ORANIG_har"),
    "user/endowment/mapping.csv" = endw_map
  )
  env <- example_env()
  vals <- run_examples(example_exprs("ems_data", subs), env)
  dats <- vals[names(vals) == "ems_data"]
  expect_length(dats, 5L)
  for (d in dats) {
    expect_type(d, "list")
    expect_gt(length(d), 0L)
  }
  expect_false(isTRUE(all.equal(env$v7_data$ESBM, env$v7_value$ESBM)))
  expect_identical(env$v7_data$VDFB, env$v7_value$VDFB)
})

skip_if_no_solver()

static_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)
static_files <- ems_example("GTAPv7", write_dir)
static_model <- suppressMessages(
  ems_model(static_files[["model_file"]], static_files[["closure_file"]])
)
nest_temp("static_deploy", write_dir)
static_cmf <- ems_deploy(static_data,
                         static_model,
                         shock = ems_uniform_shock("aoall", 1))

dynamic_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)
dynamic_files <- ems_example("GTAP-RE", write_dir)
dynamic_model <- suppressMessages(
  ems_model(dynamic_files[["model_file"]], dynamic_files[["closure_file"]])
)
nest_temp("dynamic_deploy", write_dir)
dynamic_cmf <- ems_deploy(dynamic_data,
                          dynamic_model,
                          shock = ems_uniform_shock("aoall", 1))

test_that("ems_deploy examples run", {
  nest_temp("deploy_examples", write_dir)
  env <- example_env(dat = static_data, model = static_model)
  run_examples(example_exprs("ems_deploy"), env)
  expect_true(file.exists(env$cmf_path))
  expect_match(env$cmf_path, "\\.cmf$")
  deploy_dir <- dirname(env$cmf_path)
  cls <- readLines(list.files(deploy_dir, "\\.cls$", full.names = TRUE))
  expect_true(all(c("qgdp", "yp(\"row\")") %in% cls))
  expect_false(any(c("aoreg", "dppriv") %in% cls))
  out <- suppressMessages(ems_solve(env$cmf_path))
  value <- function(v) out$dat[[which(out$name == v)]]
  expect_equal(value("qgdp")$Value, rep(1, 3L), tolerance = 1e-6)
  expect_true(all(value("aoreg")$Value > 0))
  yp <- value("yp")
  dppriv <- value("dppriv")
  expect_equal(yp$Value[yp$REGr == "row"], 0)
  expect_equal(dppriv$Value[dppriv$REGr != "row"], c(0, 0))
  expect_false(dppriv$Value[dppriv$REGr == "row"] == 0)
})

test_that("ems_solve examples run", {
  exprs <- example_exprs("ems_solve")
  is_solve <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("ems_solve"))
  }, logical(1))
  expect_identical(sum(is_solve), 3L)
  solves <- exprs[is_solve]
  static <- run_examples(solves[1:2], example_env(cmf_path = static_cmf))
  dynamic <- run_examples(solves[3], example_env(cmf_path = dynamic_cmf))
  for (out in c(static, dynamic)) {
    expect_s3_class(out, "tbl_df")
    expect_true(all(c("name", "type", "dat") %in% names(out)))
    expect_true("qgdp" %in% out$name)
  }
})

test_that("ems_RK examples run", {
  vals <- run_examples(example_exprs("ems_RK"),
                       example_env(cmf_path = static_cmf))
  expect_length(vals, 2L)
  for (out in vals) {
    expect_s3_class(out, "tbl_df")
    expect_true("qgdp" %in% out$name)
  }
  qgdp <- vals[[1]]$dat[[which(vals[[1]]$name == "qgdp")]]
  expect_true("error_estimate" %in% names(qgdp))
})

test_that("ems_compose examples run", {
  nest_temp("compose_examples", write_dir)
  solved <- ems_deploy(static_data,
                       static_model,
                       shock = ems_uniform_shock("aoall", 1))
  ems_solve(solved, suppress_outputs = TRUE)
  env <- example_env(cmf_path = solved)
  vals <- run_examples(example_exprs("ems_compose"), env)
  expect_length(vals, 3L)
  all_out <- vals[[1]]
  expect_s3_class(all_out, "tbl_df")
  expect_true(all(c("variable", "coefficient") %in% all_out$type))
  expect_setequal(vals[[2]]$name, c("qfd", "EVFP"))
  expect_identical(vals[[3]]$name, "qgdp")
  expect_false(is.null(attr(vals[[3]], "accuracy")))
})

test_that("ems_probe examples run", {
  withr::local_pdf(file.path(write_dir, "probe_plots.pdf"))
  env <- example_env(cmf_path = static_cmf)
  run_examples(example_exprs("ems_probe"), env)
  expect_s3_class(env$probe, "teems_probe")
  expect_true(env$probe$recommendation$status %in%
                c("ok", "singular", "no_host", "wont_fit"))
})

test_that("solve_in_situ examples run", {
  run_dir <- file.path(write_dir, "in_situ_example")
  dir.create(run_dir)
  deploy_dir <- dirname(static_cmf)
  subs <- c(
    "path/to/dat_file.txt" = file.path(deploy_dir, "GTAPDATA.txt"),
    "path/to/par_file.txt" = file.path(deploy_dir, "GTAPPARM.txt"),
    "path/to/sets_file.txt" = file.path(deploy_dir, "GTAPSETS.txt"),
    "existing/dir/for/model_run" = run_dir,
    "path/to/model_file.tab" = file.path(deploy_dir, "GTAPv7.tab"),
    "path/to/closure_file.cls" = file.path(deploy_dir, "GTAPv7.cls"),
    "path/to/shock_file.shf" = list.files(deploy_dir, "\\.shf$",
                                          full.names = TRUE)
  )
  vals <- run_examples(example_exprs("solve_in_situ", subs), example_env())
  out <- vals[["solve_in_situ"]]
  expect_s3_class(out, "tbl_df")
  expect_true("qgdp" %in% out$name)
})
