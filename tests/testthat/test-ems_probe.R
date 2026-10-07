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
  expect_snapshot(summary(healthy))
  expect_snapshot(summary(broken))
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
    vecsize = 2e5, nbacksolve = 68, nbselems = 2000,
    bordered = TRUE, ndblock = 35, netcut = 400, partition_set = "REG"
  )
  expect_true(cond$condensed)
  expect_true(cond$partitioned)
  expect_identical(cond$verdict, "hurts")
  expect_equal(cond$elimination_share, 2000 / 202000, tolerance = 1e-12)
  expect_equal(cond$border_share, 400 / 2e5, tolerance = 1e-12)
  expect_equal(cond$uncondensed, 202000)
  # a time chain makes the bordered method the choice at any size
  # (GTAP-RE A/Bs, roadmap 6.2: SBBD +69% to +393% condensed)
  chained <- probe_stats_variant(
    nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 12,
    chain_set = "alltime", chain_source = "structural"
  )
  expect_identical(chained$verdict, "hurts")
})

test_that("probe keeps condensation below the condensed LU crossover", {
  cond <- probe_stats_variant(
    nbacksolve = 68, nbselems = 2000,
    bordered = TRUE, ndblock = 35, netcut = 400, partition_set = "REG"
  )
  expect_true(cond$partitioned)
  expect_identical(cond$verdict, "lu_fine")
  expect_identical(cond$lu_size, .auto_thresholds()$dbbd_condensed_size)
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
      list(vecsize = 2e5, nbacksolve = 68, nbselems = 2000, bordered = TRUE,
           ndblock = 35, netcut = 400, partition_set = "REG"),
      list(nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 35,
           netcut = 400, partition_set = "REG"),
      list(nbacksolve = 68, nbselems = 2000),
      list(nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 12,
           chain_set = "alltime", chain_source = "structural"),
      list(vecsize = 1.35e6)
    )) {
      .probe_print_cndns(do.call(probe_stats_variant, v))
    }
  })
})

test_that("the probe recommends method and resources from the structure and a host", {
  laptop8 <- list(cores = 8L, mem_gb = 12, source = "given")
  box <- list(cores = 32L, mem_gb = 125, source = "given")
  # below the smallest measured size every method is quick: plain LU on one thread
  r <- .probe_recommend(healthy, host = laptop8)
  expect_identical(r$status, "ok")
  expect_identical(r$matrix_method, "LU")
  expect_identical(c(r$n_tasks, r$n_threads), c(1L, 1L))
  expect_true(r$decision$small)
  expect_match(r$call, "^ems_solve\\(cmf_path, matrix_method = \"LU\"\\)$")
  # a large plain static system with a 3-block regional partition
  static <- .probe_object(
    probe_path = file.path(fx, "healthy.probe.json"),
    stats_path = file.path(fx, "structural_static.stats.json")
  )
  static$vecsize <- 1.5e6
  r <- .probe_recommend(static, host = laptop8)
  expect_identical(r$matrix_method, "DBBD")
  expect_identical(c(r$n_tasks, r$n_threads), c(2L, 4L))
  expect_identical(r$model_type, "static")
  expect_identical(r$fit$verdict, "fits")
  expect_match(r$evidence, "assessed at 2 tasks$")
  expect_match(r$call, "^ems_solve\\(cmf_path, matrix_method = \"DBBD\", n_tasks = 2, n_threads = 4\\)$")
  # the region count caps the box's rank count, and the evidence is
  # assessed at the capped count
  r <- .probe_recommend(static, host = box)
  expect_identical(c(r$n_tasks, r$n_threads), c(3L, 8L))
  expect_identical(r$decision$n_tasks, 3L)
  expect_match(r$evidence, "assessed at 3 tasks$")
  # a large plain system without a usable partition: LU on one thread
  big_lu <- healthy
  big_lu$vecsize <- 1.5e6
  r <- .probe_recommend(big_lu, host = laptop8)
  expect_identical(r$matrix_method, "LU")
  expect_identical(c(r$n_tasks, r$n_threads), c(1L, 1L))
  expect_false(r$decision$small)
  # condensed below the crossover: threaded LU, Johansen named as the alternative
  meta <- list(system_size = 1e5, n_reg = 3L, condense = list(n_backsolve = 60L, n_backsolve_ele = 2e6))
  cond <- static
  cond$vecsize <- 1e5
  r <- .probe_recommend(cond, metadata = meta, host = laptop8)
  expect_identical(r$matrix_method, "LU")
  expect_identical(c(r$n_tasks, r$n_threads), c(1L, 8L))
  expect_identical(r$method_johansen, "DBBD")
  expect_match(r$call, "^ems_solve\\(cmf_path, matrix_method = \"LU\", n_threads = 8\\)$")
  # no host known: no recommendation
  r <- .probe_recommend(static, host = NULL)
  expect_identical(r$status, "no_host")
  expect_true(is.na(r$matrix_method))
  # memory given without cores is still no host; cores without memory
  # recommends with the fit unchecked
  r <- .probe_recommend(static, host = list(mem_gb = 12, source = "memory_given"))
  expect_identical(r$status, "no_host")
  r <- .probe_recommend(static, host = list(cores = 8L, source = "cores_given"))
  expect_identical(r$status, "ok")
  expect_identical(r$fit$verdict, "unknown")
  # a system too big for the machine gets no call
  big <- static
  big$vecsize <- 40e6
  r <- .probe_recommend(big, host = laptop8)
  expect_identical(r$status, "wont_fit")
  expect_identical(r$fit$verdict, "exceeds")
  # a structurally singular system gets no call either
  r <- .probe_recommend(broken, host = laptop8)
  expect_identical(r$status, "singular")
})

# the fixture's system scaled to a size where the method rules bite
scale_probe <- function(probe, n) {
  probe$vecsize <- n
  for (pattern in c("structural", "realized")) {
    probe[[pattern]]$n <- n
    probe[[pattern]]$rank <- n
  }
  return(probe)
}

test_that("the probe prints its recommendation", {
  probe <- .probe_object(
    probe_path = file.path(fx, "healthy.probe.json"),
    stats_path = file.path(fx, "structural_static.stats.json")
  )
  probe <- scale_probe(probe, 1.5e6)
  laptop8 <- list(cores = 8L, mem_gb = 12, source = "given")
  probe$recommendation <- .probe_recommend(probe, host = laptop8)
  expect_snapshot(print(probe))
  expect_snapshot(summary(probe))
  probe$recommendation <- .probe_recommend(probe, host = NULL)
  expect_snapshot(print(probe))
  probe$recommendation <- .probe_recommend(probe, host = list(cores = 8L, source = "cores_given"))
  expect_snapshot(print(probe))
  # DBBD blocked by memory, tight LU, and nothing fits
  probe <- scale_probe(probe, 12.5e6)
  probe$recommendation <- .probe_recommend(probe, host = laptop8)
  expect_snapshot(print(probe))
  expect_snapshot(summary(probe))
  probe <- scale_probe(probe, 40e6)
  probe$recommendation <- .probe_recommend(probe, host = laptop8)
  expect_snapshot(print(probe))
  expect_snapshot(summary(probe))
  # a singular system
  singular <- broken
  singular$recommendation <- .probe_recommend(broken, host = laptop8)
  expect_snapshot(print(singular))
})

test_that("ems_probe validates the cores and memory overrides", {
  expect_snapshot_error(ems_probe("x.cmf", cores = 0))
  expect_snapshot_error(ems_probe("x.cmf", memory = -1))
})

test_that("ems_probe announces a structurally singular system", {
  # the solver run and its collected report are stood in for by the
  # broken fixture; the announcement is gated on the verbose option
  cmf_path <- file.path(withr::local_tempdir(), "model.cmf")
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

test_that("the probe runs against its own CMF copy and output stem", {
  run_dir <- withr::local_tempdir()
  cmf_path <- file.path(run_dir, "model.cmf")
  writeLines(c(
    'tabfile "/opt/teems/model.tab";',
    'soldata "SolFiles" "/opt/teems/out/variables/bin/sol";'
  ), cmf_path)
  stale <- file.path(run_dir, "out", "probe", "sol.probe.json")
  dir.create(dirname(stale), recursive = TRUE)
  file.create(stale)
  paths <- .probe_paths(.get_solver_paths(cmf_path, "x", call = NULL))
  expect_false(file.exists(stale))
  expect_identical(paths$docker_cmf, "/opt/teems/out/probe/model.cmf")
  expect_identical(paths$docker_diag_out, "/opt/teems/out/probe/solver_out_probe.txt")
  expect_identical(paths$diag_out, file.path(normalizePath(run_dir, "/"), "out", "probe", "solver_out_probe.txt"))
  local_mocked_bindings(.solver_uses_random = function(...) FALSE)
  cmd <- .construct_probe_cmd(paths, "x", fine = TRUE)
  expect_match(cmd, "tee /opt/teems/out/probe/solver_out_probe.txt", fixed = TRUE)
  expect_identical(paths$sol_prefix, file.path(normalizePath(run_dir, "/"), "out", "probe", "sol"))
  probe_cmf <- readLines(file.path(run_dir, "out", "probe", "model.cmf"))
  expect_identical(
    grep("soldata", probe_cmf, value = TRUE),
    'soldata "SolFiles" "/opt/teems/out/probe/sol";'
  )
  expect_true('tabfile "/opt/teems/model.tab";' %in% probe_cmf)
  expect_identical(
    grep("soldata", readLines(cmf_path), value = TRUE),
    'soldata "SolFiles" "/opt/teems/out/variables/bin/sol";'
  )
})

test_that("probing a solved model leaves its solution intact", {
  skip_on_cran()
  skip_if(!nzchar(Sys.getenv("GTAP12_dat")), "GTAP data not available")
  skip_if(!.docker_image_present(paste0("teems:", .resolve_docker_tag())),
    "teems image not available"
  )
  write_dir <- withr::local_tempdir()
  ems_option_set(verbose = FALSE, tempdir = write_dir)
  withr::defer(ems_option_reset())
  files <- ems_example("GTAPv7", write_dir)
  dat <- ems_data(
    dat_input = Sys.getenv("GTAP12_dat"),
    par_input = Sys.getenv("GTAP12_par"),
    set_input = Sys.getenv("GTAP12_set"),
    REG = "big3", ACTS = "macro_sector", ENDW = "labor_agg"
  )
  model <- ems_model(files[["model_file"]], files[["closure_file"]])
  cmf_path <- ems_deploy(dat, model)
  ems_solve(cmf_path, suppress_outputs = TRUE)
  sol_dir <- file.path(dirname(cmf_path), "out", "variables", "bin")
  before <- tools::md5sum(list.files(sol_dir, full.names = TRUE))
  composed <- ems_compose(cmf_path, which = c("qgdp", "VKB"))
  probe <- ems_probe(cmf_path)
  expect_true(probe$valid)
  expect_identical(tools::md5sum(list.files(sol_dir, full.names = TRUE)), before)
  expect_false(file.exists(file.path(sol_dir, "sol.probe.json")))
  expect_true(file.exists(file.path(dirname(cmf_path), "out", "probe", "sol.probe.json")))
  expect_identical(probe$paths$log, file.path(normalizePath(dirname(cmf_path), "/"), "out", "probe", "solver_out_probe.txt"))
  expect_true(file.exists(probe$paths$log))
  expect_false(any(grepl("solver_out_.*_probe", list.files(file.path(dirname(cmf_path), "out")))))
  expect_false(file.exists(file.path(dirname(cmf_path), "out", "probe", "sol.cof")))
  expect_identical(ems_compose(cmf_path, which = c("qgdp", "VKB")), composed)
})
