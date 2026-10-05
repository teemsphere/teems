test_that("Runge-Kutta subintervals pass only in a complementarity run with an accurate run", {
  run_dir <- withr::local_tempdir()
  cmf <- file.path(run_dir, "model.cmf")
  file.create(cmf)
  a <- list(
    solution_method = "DoPri54", steps = 4L, adaptive = "yes",
    n_subintervals = 3L, eps_tolerance = 1e-4, complementarity = NULL
  )
  paths <- list(cmf = cmf)
  saveRDS(list(n_comp_active = 0), file.path(run_dir, "metadata.rds"))
  expect_snapshot_error(.validate_solver_steps(a, paths = paths, call = NULL))
  saveRDS(list(n_comp_active = 14), file.path(run_dir, "metadata.rds"))
  expect_no_error(.validate_solver_steps(a, paths = paths, call = NULL))
  a$complementarity <- list(do_acc_run = FALSE)
  expect_error(.validate_solver_steps(a, paths = paths, call = NULL))
  a$complementarity <- NULL
  unlink(file.path(run_dir, "metadata.rds"))
  expect_error(.validate_solver_steps(a, paths = paths, call = NULL))
})
