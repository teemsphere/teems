skip_on_cran()

# run-mode switches through the R API (solver c099d5f: -assertions /
# -range_test_initial / -range_test_updated / -postsim flags; the CMF
# statements are gone -- the CMF is a file manifest). Effective values
# are recorded in sol.stats.json and model_diagnostics.txt.

# the validators run after the cmf_path existence check and before
# anything reads the deployment, so an empty file is enough to reach them
stub_cmf <- function(env = parent.frame()) {
  cmf_path <- withr::local_tempfile(fileext = ".cmf", .local_envir = env)
  file.create(cmf_path)
  cmf_path
}

test_that("numeric-knob validation aborts", {
  cmf_path <- stub_cmf()
  expect_snapshot_error(ems_solve(cmf_path, n_threads = 0))
  expect_snapshot_error(ems_solve(cmf_path, max_retries = 0))
  expect_snapshot_error(ems_solve(cmf_path, retry_adjust = 1))
  expect_snapshot_error(ems_solve(cmf_path, n_tasks = c(1, 2)))
})

test_that("mode-switch validation aborts", {
  cmf_path <- stub_cmf()
  expect_snapshot_error(
    ems_solve(cmf_path, assertions = "warn"),
    class = "rlang_error"
  )
  expect_snapshot_error(
    ems_solve(cmf_path, postsim = "yes"),
    class = "rlang_error"
  )
})

test_that("a cmf_path that does not exist aborts", {
  expect_snapshot_error(ems_solve("nope.cmf"))
})

# --- e2e: switches reach the solver and the run records them --------

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")
skip_if(!nzchar(dat_input), "GTAP data not available")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "switches")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

solver_has_switches <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'assertions must be 0' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

solver_has_retry_record <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'max_retries' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

test_that("RK retry policy reaches the solver and the record (e2e)", {
  nest_temp("retry_e2e", write_dir)
  skip_if(
    !solver_has_retry_record(),
    "teems image absent or predates the retry-policy record"
  )
  conv <- GTAP_convert(dat_input, par_input, set_input)
  d <- suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = conv$set,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
  model_files <- ems_example("GTAPv7", write_dir)
  quiet_pivot(model <- ems_model(model_files[["model_file"]], model_files[["closure_file"]]))
  cmf_path <- ems_deploy(d, model)
  out <- suppressMessages(ems_solve(
    cmf_path,
    solution_method = "DoPri54",
    steps = 4L,
    adaptive = "yes",
    max_retries = 5,
    retry_adjust = 0.3,
    n_threads = 2
  ))
  expect_s3_class(out, "data.frame")
  stats <- jsonlite::read_json(
    file.path(dirname(cmf_path), "out", "variables", "bin", "sol.stats.json"),
    simplifyVector = TRUE
  )
  expect_identical(stats$options$max_retries, 5L)
  expect_equal(stats$options$retry_adjust, 0.3)
  expect_identical(stats$options$max_threads, 2L)
})

test_that("switches reach the solver and the run records them (e2e)", {
  nest_temp("switches_e2e", write_dir)
  skip_if(
    !solver_has_switches(),
    "teems image absent or predates the run-mode flags"
  )
  conv <- GTAP_convert(dat_input, par_input, set_input)
  d <- suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = conv$set,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
  model_files <- ems_example("GTAPv7", write_dir)
  tab <- file.path(write_dir, "switches.tab")
  base_txt <- readChar(model_files[["model_file"]],
    file.info(model_files[["model_file"]])$size
  )
  # a failing assertion: fatal under the solver default, downgraded here
  writeChar(paste0(base_txt, "\nAssertion # Never Holds # 1 < 0;\n"),
    tab,
    eos = NULL
  )
  quiet_pivot(model <- ems_model(tab, model_files[["closure_file"]]))
  cmf_path <- ems_deploy(d, model)
  ems_option_set(assertions = "warn", range_test_initial = "off")
  out <- suppressMessages(ems_solve(cmf_path, postsim = FALSE))
  ems_option_set(assertions = "fatal", range_test_initial = "warn")
  expect_s3_class(out, "data.frame")
  run_dir <- dirname(cmf_path)
  stats <- jsonlite::read_json(
    file.path(run_dir, "out", "variables", "bin", "sol.stats.json"),
    simplifyVector = TRUE
  )
  expect_identical(stats$options$assertions, "warn")
  expect_identical(stats$options$range_test_initial, "off")
  expect_identical(stats$options$range_test_updated, "warn")
  expect_false(stats$options$postsim)
  diag_txt <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl(
    "Modes: assertions warn; range test initial off, updated warn; postsim off",
    diag_txt,
    fixed = TRUE
  )))
})

solver_has_refine <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'Refinement step (DBBD)' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

test_that("the DBBD refinement option reaches the solver and the record (e2e)", {
  nest_temp("refine_e2e", write_dir)
  skip_if(
    !solver_has_refine(),
    "teems image absent or predates the DBBD refinement step"
  )
  conv <- GTAP_convert(dat_input, par_input, set_input)
  d <- suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = conv$set,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
  model_files <- ems_example("GTAPv7", write_dir)
  quiet_pivot(model <- ems_model(model_files[["model_file"]], model_files[["closure_file"]]))
  cmf_path <- ems_deploy(d, model, ems_uniform_shock("aoall", 5))
  run_dir <- dirname(cmf_path)
  read_stats <- function() {
    jsonlite::read_json(
      file.path(run_dir, "out", "variables", "bin", "sol.stats.json"),
      simplifyVector = TRUE
    )
  }
  on <- suppressMessages(ems_solve(cmf_path, matrix_method = "DBBD", n_tasks = 2L))
  stats <- read_stats()
  expect_true(stats$options$refine)
  expect_gt(stats$refine$solves, 0L)
  expect_lt(stats$refine$residual_ratio_after_max, 1e-12)
  diag_txt <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("refinement (DBBD, one step per solve): on", diag_txt, fixed = TRUE)))
  expect_true(any(grepl("^Refinement \\(DBBD\\): [0-9]+ solve\\(s\\) refined", diag_txt)))
  ems_option_set(refine = "off")
  off <- suppressMessages(ems_solve(cmf_path, matrix_method = "DBBD", n_tasks = 2L))
  ems_option_set(refine = "on")
  stats <- read_stats()
  expect_false(stats$options$refine)
  expect_null(stats$refine)
  diag_txt <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("refinement (DBBD, one step per solve): off (set by the refine option)", diag_txt, fixed = TRUE)))
  expect_true(any(grepl("Refinement (DBBD): off", diag_txt, fixed = TRUE)))
  lu <- suppressMessages(ems_solve(cmf_path, matrix_method = "LU"))
  stats <- read_stats()
  expect_false(stats$options$refine)
  expect_null(stats$refine)
  expect_s3_class(on, "data.frame")
  expect_s3_class(off, "data.frame")
  expect_s3_class(lu, "data.frame")
})

test_that("the intertemporal switch reads the (intertemporal) qualifier in any case, not (non_intertemporal)", {
  tab <- withr::local_tempfile(fileext = ".tab")
  cmf <- structure("unused.cmf", tab_path = tab)
  writeLines("Set (INTERTEMPORAL) TT (p0 - p3);", tab)
  expect_true(.solver_enable_time(list(cmf = cmf)))
  writeLines("Set (non_intertemporal) TT (p0 - p3);", tab)
  expect_false(.solver_enable_time(list(cmf = cmf)))
})
