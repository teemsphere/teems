skip_on_cran()

withr::defer(ems_option_reset(), teardown_env())

test_that("ems_option_get errors on invalid name", {
  expect_snapshot_error(ems_option_get("not_an_option"))
})

test_that("ems_option_get returns list of all options when name is NULL", {
  opts <- ems_option_get()
  expect_type(opts, "list")
  expect_true(length(opts) > 0)
})

test_that("ems_option_get returns logical for verbose", {
  expect_type(ems_option_get("verbose"), "logical")
})

test_that("ems_option_get returns character for tempdir", {
  expect_type(ems_option_get("tempdir"), "character")
})

test_that("ems_option_get returns numeric for ndigits", {
  expect_type(ems_option_get("ndigits"), "integer")
})

test_that("ems_option_get returns logical for check_shock_status", {
  expect_type(ems_option_get("check_shock_status"), "logical")
})

test_that("ems_option_get returns character for timestep_header", {
  expect_type(ems_option_get("timestep_header"), "character")
})

test_that("ems_option_get returns character for n_timestep_header", {
  expect_type(ems_option_get("n_timestep_header"), "character")
})

test_that("ems_option_get returns character for docker_tag", {
  expect_type(ems_option_get("docker_tag"), "character")
})

test_that("ems_option_get returns numeric for accuracy_threshold", {
  expect_type(ems_option_get("accuracy_threshold"), "double")
})

test_that("ems_option_set sets verbose", {
  ems_option_set(verbose = FALSE)
  expect_false(ems_option_get("verbose"))
  ems_option_reset()
})

test_that("ems_option_set sets tempdir", {
  ems_option_set(tempdir = getwd())
  expect_equal(ems_option_get("tempdir"), getwd())
  ems_option_reset()
})

test_that("ems_option_set sets ndigits", {
  ems_option_set(ndigits = 8)
  expect_equal(ems_option_get("ndigits"), 8)
  ems_option_reset()
})

test_that("ems_option_set sets check_shock_status", {
  ems_option_set(check_shock_status = FALSE)
  expect_false(ems_option_get("check_shock_status"))
  ems_option_reset()
})

test_that("ems_option_set sets docker_tag", {
  ems_option_set(docker_tag = "v1.0")
  expect_equal(ems_option_get("docker_tag"), "v1.0")
  ems_option_reset()
})

test_that("ems_option_set sets accuracy_threshold", {
  ems_option_set(accuracy_threshold = 0.95)
  expect_equal(ems_option_get("accuracy_threshold"), 0.95)
  ems_option_reset()
})

test_that("ems_option_set sets timestep_header", {
  ems_option_set(timestep_header = "AYRS")
  expect_equal(ems_option_get("timestep_header"), "AYRS")
  ems_option_reset()
})

test_that("ems_option errors when write_dir does not exist", {
  expect_snapshot(
    ems_option_set(tempdir = file.path(basename(getwd()), "does_not_exist")),
    error = TRUE, transform = scrub_paths
  )
})

test_that("ems_option_set sets n_timestep_header", {
  ems_option_set(n_timestep_header = "NINTERVAL")
  expect_equal(ems_option_get("n_timestep_header"), "NINTERVAL")
  ems_option_reset()
})

test_that("ems_option_set returns NULL invisibly", {
  expect_null(ems_option_set(verbose = FALSE))
  ems_option_reset()
})

test_that("ems_option_reset restores default verbose", {
  ems_option_set(verbose = FALSE)
  ems_option_reset()
  expect_true(ems_option_get("verbose"))
})

test_that("zero_below is validated, stored, applied to written values and reset", {
  withr::defer(ems_option_reset())
  expect_identical(ems_option_get("zero_below"), 1e-6)
  ems_option_set(zero_below = 1e-3)
  expect_identical(ems_option_get("zero_below"), 1e-3)
  expect_identical(.typed_values(c(0.5, 2e-4, -2e-4, 0), "Real", 6L), c(0.5, 0, 0, 0))
  ems_option_set(zero_below = 0)
  expect_identical(.typed_values(c(0.5, 2e-4, 3.5e-7), "Real", 6L), c(0.5, 2e-4, 3.5e-7))
  ems_option_reset()
  expect_identical(.typed_values(c(0.5, 2e-4, 3.5e-7, -4e-45), "Real", 6L), c(0.5, 2e-4, 0, 0))
  expect_snapshot_error(ems_option_set(zero_below = -1))
  expect_snapshot_error(ems_option_set(zero_below = c(1, 2)))
})

test_that("ems_option_reset restores default ndigits", {
  ems_option_set(ndigits = 10)
  ems_option_reset()
  expect_equal(ems_option_get("ndigits"), 6)
})

test_that("ems_option_set rejects invalid values", {
  withr::defer(ems_option_reset())
  expect_snapshot_error(ems_option_set(verbose = "yes"))
  expect_snapshot_error(ems_option_set(ndigits = 2.5))
  expect_snapshot_error(ems_option_set(accuracy_threshold = 1.5))
  expect_snapshot_error(ems_option_set(check_shock_status = NA))
  expect_snapshot_error(ems_option_set(timestep_header = "year"))
  expect_snapshot_error(ems_option_set(n_timestep_header = "nyear"))
  expect_snapshot_error(ems_option_set(full_exclude = 1))
  expect_snapshot_error(ems_option_set(docker_tag = 1))
})

test_that("ems_option_set sets the solver run modes and rejects other values", {
  withr::defer(ems_option_reset())
  expect_identical(ems_option_get("assertions"), "fatal")
  expect_identical(ems_option_get("range_test_initial"), "fatal")
  expect_identical(ems_option_get("range_test_updated"), "warn")
  ems_option_set(assertions = "warn", range_test_initial = "warn", range_test_updated = "off")
  expect_identical(ems_option_get("assertions"), "warn")
  expect_identical(ems_option_get("range_test_initial"), "warn")
  expect_identical(ems_option_get("range_test_updated"), "off")
  expect_snapshot_error(ems_option_set(assertions = "maybe"))
  expect_snapshot_error(ems_option_set(range_test_initial = 2))
  expect_snapshot_error(ems_option_set(range_test_updated = c("warn", "off")))
  ems_option_reset()
  expect_identical(ems_option_get("assertions"), "fatal")
})

test_that("random_seed is validated, stored as an integer and reset", {
  withr::defer(ems_option_reset())
  expect_identical(ems_option_get("random_seed"), 1L)
  ems_option_set(random_seed = 42)
  expect_identical(ems_option_get("random_seed"), 42L)
  expect_identical(ems_option_get()$random_seed, 42L)
  expect_snapshot_error(ems_option_set(random_seed = -1))
  expect_error(ems_option_set(random_seed = 2.5), "random_seed")
  expect_error(ems_option_set(random_seed = c(1, 2)), "random_seed")
  expect_error(ems_option_set(random_seed = "7"), "random_seed")
  expect_error(ems_option_set(random_seed = 2^31), "random_seed")
  expect_identical(ems_option_get("random_seed"), 42L)
  ems_option_reset()
  expect_identical(ems_option_get("random_seed"), 1L)
})

test_that("the refine option is validated, stored and reset", {
  withr::defer(ems_option_reset())
  expect_true(ems_option_get("refine"))
  ems_option_set(refine = FALSE)
  expect_false(ems_option_get("refine"))
  expect_false(ems_option_get()$refine)
  expect_false(.o_refine())
  expect_snapshot_error(ems_option_set(refine = "off"))
  expect_error(ems_option_set(refine = c(TRUE, FALSE)), "refine")
  expect_error(ems_option_set(refine = NA), "refine")
  expect_false(ems_option_get("refine"))
  ems_option_reset()
  expect_true(ems_option_get("refine"))
})

test_that("the converge option is validated, stored and reset", {
  withr::defer(ems_option_reset())
  expect_false(ems_option_get("converge"))
  ems_option_set(converge = TRUE)
  expect_true(ems_option_get("converge"))
  expect_true(ems_option_get()$converge)
  expect_true(.o_converge())
  expect_snapshot_error(ems_option_set(converge = "on"))
  ems_option_reset()
  expect_false(ems_option_get("converge"))
})

test_that("the accuracy figures and criterion options are validated, stored and reset", {
  withr::defer(ems_option_reset())
  expect_identical(ems_option_get("accuracy_figures"), 4L)
  expect_identical(ems_option_get("accuracy_criterion"), "solution")
  ems_option_set(accuracy_figures = 5, accuracy_criterion = "both")
  expect_identical(ems_option_get("accuracy_figures"), 5L)
  expect_identical(ems_option_get()$accuracy_criterion, "both")
  expect_identical(.o_accuracy_figures(), 5L)
  expect_identical(.o_accuracy_criterion(), "both")
  expect_snapshot_error(ems_option_set(accuracy_figures = 7))
  expect_error(ems_option_set(accuracy_figures = 2.5), "accuracy_figures")
  expect_error(ems_option_set(accuracy_figures = c(3, 4)), "accuracy_figures")
  expect_snapshot_error(ems_option_set(accuracy_criterion = "levels"))
  expect_error(ems_option_set(accuracy_criterion = c("data", "both")), "accuracy_criterion")
  ems_option_reset()
  expect_identical(ems_option_get("accuracy_figures"), 4L)
  expect_identical(ems_option_get("accuracy_criterion"), "solution")
})

test_that("the required_figures option is validated, stored, reset and sent as -rqf", {
  withr::defer(ems_option_reset())
  expect_null(ems_option_get("required_figures"))
  ems_option_set(required_figures = 3)
  expect_identical(ems_option_get("required_figures"), 3L)
  expect_identical(.o_required_figures(), 3L)
  expect_snapshot_error(ems_option_set(required_figures = 0))
  expect_error(ems_option_set(required_figures = "3"), "required_figures")
  ems_option_reset()
  expect_null(ems_option_get("required_figures"))
})

test_that("ems_option_reset restores default docker_tag", {
  ems_option_set(docker_tag = "v1.0")
  ems_option_reset()
  expect_equal(ems_option_get("docker_tag"), "latest")
})

test_that("ems_option_reset returns NULL invisibly", {
  expect_null(ems_option_reset())
})

test_that("docker_tag defaults to latest and an explicit tag wins", {
  expect_identical(.o_docker_tag(), "latest")
  ems_option_set(docker_tag = "custom")
  expect_identical(.o_docker_tag(), "custom")
  ems_option_reset()
})
