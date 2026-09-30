skip_on_cran()

withr::defer(ems_option_reset(), teardown_env())

laptop <- list(cores = 8L, mem_gb = 12)

test_that("the memory model adds the refinement term to DBBD only", {
  th <- .auto_thresholds()
  base <- .auto_memory_gb("DBBD", 2L, 1.4e6, refine = FALSE)
  with <- .auto_memory_gb("DBBD", 2L, 1.4e6, refine = TRUE)
  expect_equal(with - base, th$mem_dbbd_refine * 1.4e6 / 1e6)
  expect_equal(
    .auto_memory_gb("DBBD", 2L, 1.4e6, condensed = TRUE, refine = TRUE) /
      .auto_memory_gb("DBBD", 2L, 1.4e6, refine = TRUE),
    th$mem_dbbd_condensed
  )
  expect_identical(.auto_memory_gb("LU", 1L, 1.4e6, refine = TRUE), .auto_memory_gb("LU", 1L, 1.4e6))
  expect_equal(.memory_fit_check("DBBD", 2L, 1.4e6, host = laptop, refine = TRUE)$est_gb, with)
})

test_that("refinement is on for DBBD by default, off only by the option", {
  expect_null(.refine_decide("LU", 1L, 1.4e6))
  expect_null(.refine_decide("NDBBD", 2L, 1.4e6, mode = "on"))
  d <- .refine_decide("DBBD", 2L, 1e9)
  expect_true(d$on)
  expect_identical(d$reason, "on")
  expect_equal(d$est_gb, .auto_memory_gb("DBBD", 2L, 1e9, refine = TRUE))
  d <- .refine_decide("DBBD", 2L, 1e5, mode = "off")
  expect_false(d$on)
  expect_identical(d$reason, "off")
})

test_that("the memory estimate counts refinement unless the option turns it off", {
  with <- .auto_memory_gb("DBBD", 2L, 1.4e6, refine = TRUE)
  expect_equal(.auto_memory_gb("DBBD", 2L, 1.4e6), with)
  expect_equal(.memory_fit_check("DBBD", 2L, 1.4e6, host = laptop)$est_gb, with)
  ems_option_set(refine = "off")
  withr::defer(ems_option_set(refine = "on"))
  expect_equal(.auto_memory_gb("DBBD", 2L, 1.4e6), .auto_memory_gb("DBBD", 2L, 1.4e6, refine = FALSE))
  expect_lt(.auto_memory_gb("DBBD", 2L, 1.4e6), with)
})

test_that("the refinement flag reaches the command and the record", {
  expect_null(.extra_cli_flags(list()))
  expect_identical(.extra_cli_flags(list(refine = TRUE)), "-refine 1")
  expect_identical(.extra_cli_flags(list(refine = FALSE)), "-refine 0")
  r <- list(method = "DBBD", n_tasks = 2L, n_threads = 1L, cores = 8L, mem_gb = 12)
  r$fit <- .memory_fit_check("DBBD", 2L, 1.4e6, host = laptop, refine = TRUE)
  r$refine <- .refine_decide("DBBD", 2L, 1.4e6)
  lines <- .resources_record_lines(r)
  expect_length(lines, 3L)
  expect_match(lines[3], "^  refinement \\(DBBD, one step per solve\\): on \\(the default\\)$")
  r$refine <- .refine_decide("DBBD", 2L, 1.4e6, mode = "off")
  expect_match(.resources_record_lines(r)[3], "^  refinement \\(DBBD, one step per solve\\): off \\(set by the refine option\\)$")
  r$refine <- NULL
  expect_length(.resources_record_lines(r), 2L)
})

test_that("the probe recommendation reports the refinement decision for DBBD", {
  r <- list(
    matrix_method = "DBBD", n_tasks = 2L, n_threads = 1L, method_johansen = "DBBD",
    evidence = "e", rationale = "r", decision = list(), host = list(cores = NULL),
    fit = NULL, tempdir = NULL, call = "ems_solve(cmf_path)",
    refine = list(on = TRUE, reason = "on", est_gb = 1.2)
  )
  expect_match(paste(cli::cli_fmt(.probe_print_recommendation(r)), collapse = "\n"), "refinement step per \"?DBBD\"? solve: on \\(about 1.2 GB with it\\)")
  r$refine <- list(on = FALSE, reason = "off", est_gb = 20)
  expect_match(paste(cli::cli_fmt(.probe_print_recommendation(r)), collapse = "\n"), "solve: off, set by the refine option")
})
