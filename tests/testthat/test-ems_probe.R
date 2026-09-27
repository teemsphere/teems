fx <- test_path("fixtures", "probe")

healthy <- .probe_object(
  probe_path = file.path(fx, "healthy.probe.json"),
  stats_path = file.path(fx, "healthy.stats.json")
)
broken <- .probe_object(
  probe_path = file.path(fx, "broken.probe.json")
)

test_that("healthy probe report parses", {
  expect_s3_class(healthy, "teems_probe")
  expect_true(healthy$valid)
  expect_identical(healthy$version, 2L)
  expect_false(healthy$structural$defective)
  expect_false(healthy$realized$defective)
  expect_identical(healthy$structural$rank, healthy$vecsize)
  # the statement table tiles the condensed system exactly
  expect_identical(sum(healthy$statements$rows), healthy$vecsize)
  expect_gt(nrow(healthy$incidence), 0L)
  expect_identical(nrow(healthy$defects), 0L)
  # fine decomposition
  expect_identical(healthy$cores$largest, 4341L)
  expect_identical(healthy$cores$top$eqs[[1]]$name[[1]], "e_qfa")
  # stats.json companion
  expect_identical(healthy$structure$vecsize, healthy$vecsize)
})

test_that("broken probe report parses with named defects", {
  expect_s3_class(broken, "teems_probe")
  expect_false(broken$valid)
  expect_true(broken$structural$defective)
  expect_identical(broken$structural$rank, broken$vecsize - 3L)
  # exact named defects on both sides
  expect_identical(
    broken$structural$under_determined$name,
    rep("dprobeb", 3L)
  )
  expect_identical(
    broken$structural$over_constrained$name,
    rep("e_dprobe2", 3L)
  )
  # element tuples parsed from the solver-side labels
  expect_identical(broken$structural$under_determined$elements[[1]], "0")
  # aggregations
  expect_identical(broken$structural$under_by_var$name, "dprobeb")
  expect_identical(broken$structural$under_by_var$count, 3L)
  expect_identical(
    sort(broken$structural$dm_over_by_eq$name),
    c("e_dprobe1", "e_dprobe2")
  )
  # DM block sizes
  expect_identical(broken$structural$dm$m3, 6L)
  expect_identical(broken$structural$dm$n3, 3L)
  # combined defect tibble spans both patterns and sides
  expect_identical(nrow(broken$defects), 12L)
  expect_setequal(broken$defects$pattern, c("structural", "realized"))
})

test_that("probe print methods run", {
  expect_snapshot(print(healthy))
  expect_snapshot(print(broken))
})

test_that("probe plots render on a null device", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  expect_invisible(plot(healthy, type = "incidence"))
  expect_invisible(plot(healthy, type = "cores"))
  expect_invisible(plot(broken, type = "dm"))
  expect_invisible(plot(broken, type = "incidence"))
})

test_that("incidence plot errors on a probe without incidence data", {
  bare <- healthy
  bare$incidence <- NULL
  expect_snapshot_error(plot(bare, type = "incidence"))
})

test_that("dm plot errors on a structurally valid probe", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  expect_snapshot_error(plot(healthy, type = "dm"))
})

test_that("cores plot errors without fine data", {
  no_fine <- healthy
  no_fine$cores <- NULL
  expect_snapshot_error(plot(no_fine, type = "cores"))
})

test_that("probe report errors when the report is absent", {
  # the message embeds an absolute path, so no snapshot
  expect_error(
    .probe_object(probe_path = file.path(fx, "nonexistent.probe.json")),
    regexp = "No probe report was produced"
  )
})

test_that("ems_probe errors when cmf_path is missing", {
  expect_snapshot_error(ems_probe())
})

test_that("ems_probe errors when fine is not a logical scalar", {
  expect_snapshot_error(ems_probe(cmf_path = "some.cmf", fine = "yes"))
})

# probe-informed condensation advice (roadmap 6.2 via 6.10): the verdict
# reads the measured block structure, so it is exercised against stats
# variants of the healthy fixture
probe_stats_variant <- function(...) {
  stats <- jsonlite::fromJSON(file.path(fx, "healthy.stats.json"))
  stats <- utils::modifyList(stats, list(...))
  path <- file.path(tempfile(), "sol.stats.json")
  dir.create(dirname(path))
  writeLines(jsonlite::toJSON(stats, auto_unbox = TRUE, null = "null"), path)
  .probe_object(
    probe_path = file.path(fx, "healthy.probe.json"),
    stats_path = path
  )$condense
}

test_that("probe reports no condensation verdict for a small plain system", {
  cond <- probe_stats_variant()
  expect_false(cond$condensed)
  expect_false(cond$partitioned)
  expect_identical(cond$verdict, "none")
})

test_that("probe advises against condensation when a partition exists", {
  cond <- probe_stats_variant(
    nbacksolve = 68, nbselems = 2000,
    bordered = TRUE, ndblock = 35, netcut = 400, partition_set = "REG"
  )
  expect_true(cond$condensed)
  expect_true(cond$partitioned)
  expect_identical(cond$verdict, "hurts")
  expect_equal(cond$elimination_share, 2000 / 12524, tolerance = 1e-12)
  expect_equal(cond$border_share, 400 / 10524, tolerance = 1e-12)
})

test_that("probe confirms condensation on an LU-bound system", {
  cond <- probe_stats_variant(nbacksolve = 68, nbselems = 2000)
  expect_true(cond$condensed)
  expect_false(cond$partitioned)
  expect_identical(cond$verdict, "helps")
})

test_that("probe suggests condensation for a large LU-bound system", {
  expect_identical(probe_stats_variant(vecsize = 1.35e6)$verdict, "candidate")
  # too small for the measured gain to show
  expect_identical(probe_stats_variant(vecsize = 2e5)$verdict, "none")
  # a partitioned system of the same size is never a candidate
  expect_identical(
    probe_stats_variant(vecsize = 1.35e6, bordered = TRUE, ndblock = 35)$verdict,
    "none"
  )
})

test_that("probe prints each condensation verdict", {
  expect_snapshot({
    for (v in list(
      list(nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 35,
           netcut = 400, partition_set = "REG"),
      list(nbacksolve = 68, nbselems = 2000),
      list(vecsize = 1.35e6)
    )) {
      .probe_print_cndns(do.call(probe_stats_variant, v))
    }
  })
})

test_that("the probe recommends method and resources from the structure and a host", {
  laptop8 <- list(cores = 8L, mem_gb = 12, source = "given")
  box <- list(cores = 32L, mem_gb = 125, source = "given")
  # the healthy fixture has no usable partition: threaded LU
  r <- .probe_recommend(healthy, host = laptop8)
  expect_identical(r$matrix_method, "LU")
  expect_identical(c(r$n_tasks, r$n_threads), c(1L, 8L))
  # a small plain static system with a 3-block regional partition
  healthy <- .probe_object(
    probe_path = file.path(fx, "healthy.probe.json"),
    stats_path = file.path(fx, "structural_static.stats.json")
  )
  r <- .probe_recommend(healthy, host = laptop8)
  expect_identical(r$matrix_method, "DBBD")
  expect_identical(c(r$n_tasks, r$n_threads), c(2L, 4L))
  expect_identical(r$model_type, "static")
  expect_identical(r$fit$verdict, "fits")
  expect_match(r$call, "^ems_solve\\(cmf_path, matrix_method = \"DBBD\", n_tasks = 2, n_threads = 4\\)$")
  # the region count caps the box's rank count
  r <- .probe_recommend(healthy, host = box)
  expect_identical(c(r$n_tasks, r$n_threads), c(3L, 8L))
  # condensed below the crossover: threaded LU, Johansen named as the alternative
  meta <- list(system_size = 3485, n_reg = 3L, condense = list(n_backsolve = 60L, n_backsolve_ele = 60000))
  r <- .probe_recommend(healthy, metadata = meta, host = laptop8)
  expect_identical(r$matrix_method, "LU")
  expect_identical(c(r$n_tasks, r$n_threads), c(1L, 8L))
  expect_identical(r$method_johansen, "LU")
  expect_match(r$call, "^ems_solve\\(cmf_path, matrix_method = \"LU\", n_threads = 8\\)$")
  # no host known: one task, one thread, memory check not applied
  r <- .probe_recommend(healthy, host = NULL)
  expect_identical(c(r$n_tasks, r$n_threads), c(1L, 1L))
  expect_identical(r$fit$verdict, "unknown")
  # a system too big for the machine is reported, not refused
  big <- healthy
  big$vecsize <- 40e6
  r <- .probe_recommend(big, host = laptop8)
  expect_identical(r$fit$verdict, "exceeds")
})

test_that("the probe prints its recommendation", {
  probe <- .probe_object(
    probe_path = file.path(fx, "healthy.probe.json"),
    stats_path = file.path(fx, "structural_static.stats.json")
  )
  probe$recommendation <- .probe_recommend(probe, host = list(cores = 8L, mem_gb = 12, source = "given"))
  expect_snapshot(print(probe))
  probe$recommendation <- .probe_recommend(probe, host = NULL)
  expect_snapshot(print(probe))
})

test_that("ems_probe validates the cores and memory overrides", {
  expect_snapshot_error(ems_probe("x.cmf", cores = 0))
  expect_snapshot_error(ems_probe("x.cmf", memory = -1))
})

test_that("ems_probe announces a structurally singular system", {
  # the solver run and its collected report are stood in for by the
  # broken fixture; the announcement is gated on the verbose option
  cmf_path <- withr::local_tempfile(fileext = ".cmf")
  file.create(cmf_path)
  local_mocked_bindings(
    .check_docker = function(...) invisible(NULL),
    .run_solver_cmd = function(...) invisible(NULL),
    .solver_uses_random = function(...) FALSE,
    .collect_probe = function(...) broken,
    .deploy_metadata = function(...) NULL,
    .probe_recommend = function(...) NULL
  )
  ems_option_set(verbose = TRUE)
  withr::defer(ems_option_reset())
  expect_snapshot(res <- ems_probe(cmf_path, cores = 4, memory = 8))
  expect_false(res$valid)
  ems_option_set(verbose = FALSE)
  expect_no_message(ems_probe(cmf_path, cores = 4, memory = 8))
})
