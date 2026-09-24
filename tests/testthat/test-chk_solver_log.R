skip_on_cran()

local_solver_log <- function(lines, env = parent.frame()) {
  run_dir <- withr::local_tempdir(.local_envir = env)
  diag_out <- file.path(run_dir, "diag.txt")
  writeLines(lines, diag_out)
  list(diag_out = diag_out, run = run_dir)
}

check_log <- function(paths) {
  .check_solver_log(
    elapsed_time = proc.time(),
    solve_cmd = "cmd",
    paths = paths,
    call = NULL
  )
}

test_that("TAB errors map to the model-specification abort", {
  paths <- local_solver_log(c(
    "solver banner",
    "Error: name psave is declared as both a coefficient and a variable; names are case-insensitive and must be unique (manual 11.2.1)"
  ))
  expect_error(
    check_log(paths),
    "rejected the model specification with 1 error"
  )
  expect_error(check_log(paths), "11.2.1")
})

test_that("multiple TAB errors are previewed with their manual sections", {
  paths <- local_solver_log(c(
    "Error: coefficient dupx is declared more than once (manual 11.2.1)",
    "Error: unknown variable qualifier 'foo'"
  ))
  expect_error(check_log(paths), "2 errors")
  expect_error(check_log(paths), "unknown variable qualifier")
})

test_that("closure errors map to the closure abort", {
  paths <- local_solver_log(c(
    "Error: variable qgdp is not declared",
    "Error: element usa is not in set REG (in qxs)"
  ))
  expect_error(
    check_log(paths),
    "rejected the closure or shock inputs with 2 errors"
  )
})

test_that("shock-file errors map to the closure abort", {
  paths <- local_solver_log(
    "Error: qxs in the shock file is not a declared variable"
  )
  expect_error(check_log(paths), "closure or shock inputs")
})

test_that("C9 fail-fast reader wordings map to the closure abort", {
  # "cannot open closure file" must hit the closure row, not the
  # generic data-class "cannot open"
  paths <- local_solver_log(c(
    "Error: cannot open closure file /run/GTAPv7.cls",
    "Error: wrong number of arguments for variable afall (closure file)"
  ))
  expect_error(
    check_log(paths),
    "rejected the closure or shock inputs with 2 errors"
  )
  paths <- local_solver_log(
    "Error: shock statement for variable pop supplies fewer values than elements (3 expected) (shock file)"
  )
  expect_error(check_log(paths), "closure or shock inputs")
})

test_that("data errors map to the data abort", {
  paths <- local_solver_log(
    'Error: header "VKB" not found in the data file'
  )
  expect_error(check_log(paths), "could not read the model data")
})

test_that("runtime errors map to the numeric abort", {
  paths <- local_solver_log(
    "Error: division by zero in a formula; Zerodivide (nonzero_by_zero) is off (GEMPACK default) -- set a default or guard with ID01"
  )
  expect_error(check_log(paths), "runtime error")
  expect_error(check_log(paths), "10.11.1")
})

test_that("workspace exhaustion maps to the resource abort", {
  # the three solver-side workspace fatals (solve_drivers.c ma48_grow_la
  # ceiling, the growth loops, and a declined fast refactorize)
  paths <- local_solver_log(
    paste(
      "Error: factorizing condensed system needs a larger MA48 workspace",
      "than the 2147483647-element limit of the 32-bit HSL build (-laA is",
      "already at its 1545% ceiling); this system is too large for one",
      "sequential factorization -- solve it with a bordered matrix_method",
      '("SBBD" or "DBBD"), condense the model, or reduce its dimensions'
    )
  )
  expect_error(check_log(paths), "ran out of factorization workspace")
  expect_error(check_log(paths), "matrix_method")
  paths <- local_solver_log(
    paste(
      "Error: the MA48 workspace for DBBD diagonal block did not converge",
      "after 6 growth attempts; raise the initial workspace (laA/laD/laDi)",
      'or use a bordered matrix_method ("SBBD" or "DBBD")'
    )
  )
  expect_error(check_log(paths), "ran out of factorization workspace")
  paths <- local_solver_log(
    paste(
      "Error: MA48 could not factorize NDBBD diagonal block after repeated",
      "retries (the fast refactorization was declined each time); re-run",
      "with fastrefac = FALSE, or use a different matrix_method"
    )
  )
  expect_error(check_log(paths), "ran out of factorization workspace")
})

test_that("index ceiling maps to the size abort", {
  # solve_drivers.c jac_mat_prealloc: the one-rank Jacobian copy passes
  # the PetscInt total, and a PETSc preallocation failure after the check
  paths <- local_solver_log(
    paste(
      "Error: assembling the Jacobian for the linear system needs",
      "2493188469 nonzeros, above the 2147483647-nonzero ceiling of the",
      "32-bit PetscInt build; every HSL matrix_method (\"LU\", \"SBBD\")",
      "keeps the whole system on one rank, so more ranks do not help --",
      "condense the model, reduce its dimensions, or use the distributed",
      "matrix_method \"DBBD\""
    )
  )
  expect_error(check_log(paths), "too large for the solver")
  expect_error(check_log(paths), "DBBD")
  paths <- local_solver_log(
    "Error: PETSc could not preallocate the exogenous block for the linear system (PETSc error 63)"
  )
  expect_error(check_log(paths), "too large for the solver")
  # hsl_kernels.f90 hsl_i4: an INTEGER(8) count past HSL_MP48's INTEGER(4)
  paths <- local_solver_log(
    "Error: NE = 2147483648 exceeds the 32-bit HSL MP48 interface"
  )
  expect_error(check_log(paths), "too large for the solver")
})

test_that("TAB class takes priority over data class", {
  paths <- local_solver_log(c(
    "Error: cannot open file baddata.har",
    "Error: set marg is declared as both a coefficient and a set (manual 11.2.1)"
  ))
  expect_error(check_log(paths), "rejected the model specification")
})

test_that("unmapped Error lines fall back to the generic abort", {
  paths <- local_solver_log(
    "Error: some entirely novel condition"
  )
  expect_error(check_log(paths), "Errors detected during solution")
})

test_that("singularity without Error lines routes to the probe hint", {
  paths <- local_solver_log(
    "MA48: the matrix is singular at step 1"
  )
  expect_error(check_log(paths), "Singularity detected")
  expect_error(check_log(paths), "ems_probe")
  expect_error(check_log(paths), "structurally deficient closure")
})

test_that("singularity after updated range violations routes to the shock-size hint", {
  paths <- local_solver_log(c(
    "Warning: coefficient vxsb has a value below its declared lower bound 0.000000",
    "LU time 0.10",
    "Warning: coefficient vfob has an updated value below its declared lower bound 0.000000",
    "Warning: coefficient evfp has an updated value below its declared lower bound 0.000000",
    "MA48: the matrix is singular at step 52"
  ))
  expect_error(check_log(paths), "Singularity detected")
  expect_error(check_log(paths), "2 updated-value range warnings")
  expect_error(check_log(paths), "first: coefficient vfob has an updated value")
  expect_error(check_log(paths), "n_subintervals")
  expect_error(check_log(paths), "DoPri54")
  expect_error(check_log(paths), "ems_probe")
  expect_no_error(tryCatch(check_log(paths), error = \(e) {
    expect_false(grepl("structurally deficient", conditionMessage(e)))
    NULL
  }))
})

test_that("initial-data range warnings alone keep the structural singularity hint", {
  paths <- local_solver_log(c(
    "Warning: coefficient vxsb has a value below its declared lower bound 0.000000",
    "MA48: the matrix is singular at step 1"
  ))
  expect_error(check_log(paths), "structurally deficient closure")
})

test_that("cli braces in solver output do not break glue rendering", {
  paths <- local_solver_log(
    "Error: malformed formula {unbalanced} in TAB file"
  )
  expect_error(check_log(paths), "rejected the model specification")
})

test_that("a clean log passes and writes the exec record", {
  paths <- local_solver_log(c(
    "solver banner",
    "all steps complete"
  ))
  expect_no_error(suppressMessages(check_log(paths)))
  expect_true(file.exists(file.path(paths$run, "model_exec.txt")))
})

test_that(".map_solver_errors classifies representative catalog lines", {
  mapped <- .map_solver_errors(c(
    "coefficient name max is a reserved word (manual 11.2.1)",
    "duplicate lower bound on a coefficient declaration (one lower GE/GT and one upper LE/LT allowed)",
    "only (linear) or (levels) may qualify an equation under Equation (default=levels)",
    "PostSim Formula assigns variable psave; simulation results cannot be changed (manual 12.2.2)",
    "set nmrg references itself in a set expression",
    "the $POS function is not supported yet: $POS(r)",
    "Read without a header is not supported (use 'Read X from file <log> header \"H\"')",
    "variable qq is not declared",
    "zero divided by zero in a formula while Zerodivide (zero_by_zero) is off",
    "assertion failed (Assertions = warn/no in the CMF file suppresses/downgrades this abort)",
    "set product element a_very_long_element_b_very_long_element in the definition of AB exceeds 255 characters",
    "r is not a coefficient, variable or number and cannot be an arithmetic operand (an index or quoted element compares through $POS, manual 11.5.6/11.4.11)",
    "coefficient vxsb has an updated value at or below its declared strict lower bound 0.000000"
  ))
  expect_identical(
    mapped$class,
    c(
      "tab", "tab", "tab", "tab", "tab", "tab", "tab",
      "closure", "numeric", "numeric", "tab", "tab", "numeric"
    )
  )
  expect_identical(mapped$manual[13], "25.4.4")
  expect_identical(mapped$manual[1], "11.2.1")
  expect_identical(mapped$manual[9], "10.11.1")
})

test_that("condest diagnostic lines do not trip the generic scans", {
  paths <- local_solver_log(c(
    "solver banner",
    "condest: backward error omega1 0.00e+00 omega2 0.00e+00 (2 refinement passes), forward error bound 0.00e+00, kappa_w1 3.029e+04, kappa_w2 8.470e+08",
    "all steps complete"
  ))
  expect_no_error(suppressMessages(check_log(paths)))
})

test_that("memory record lines do not trip the generic scans", {
  paths <- local_solver_log(c(
    "solver banner",
    "memory: after matrix assembly, resident 1.23 GB max per rank, 4.56 GB over 4 rank(s); high-water 1.30 GB max, 4.80 GB sum",
    "Step time 0.01 s",
    "memory: after step, resident 1.40 GB max per rank, 5.10 GB over 4 rank(s); high-water 1.45 GB max, 5.30 GB sum",
    "all steps complete"
  ))
  expect_no_error(suppressMessages(check_log(paths)))
})

test_that("the solve record names the BLAS kernel family the run dispatched", {
  run_dir <- withr::local_tempdir()
  stats_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(stats_dir, recursive = TRUE)
  writeLines("diag", file.path(run_dir, "model_diagnostics.txt"))
  writeLines(
    paste0(
      '{"version": 2, "solver_version": "1.1.0-dev.4", ',
      '"solution_method": "Johansen", "matrix_method": "LU", ',
      '"mpi_size": 1, "vecsize": 10, "nexo": 3, ',
      '"options": {"subintervals": 1, "laA": 300, "laDi": 500, "laD": 200, ',
      '"fastrefac": false, "max_threads": 1, "blas_core": "Nehalem", ',
      '"assertions": "warn", "range_test_initial": "warn", ',
      '"range_test_updated": "warn", "postsim": false, "gpzerodivide": false}}'
    ),
    file.path(stats_dir, "sol.stats.json")
  )
  .solve_record_append(run_dir)
  rec <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("BLAS kernels: Nehalem", rec, fixed = TRUE)))
})

# an image predating the pin writes no blas_core: the line is dropped
# rather than rendered empty
test_that("the solve record omits the BLAS line when the solver did not record one", {
  run_dir <- withr::local_tempdir()
  stats_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(stats_dir, recursive = TRUE)
  writeLines("diag", file.path(run_dir, "model_diagnostics.txt"))
  writeLines(
    paste0(
      '{"version": 2, "solution_method": "Johansen", "matrix_method": "LU", ',
      '"mpi_size": 1, "vecsize": 10, "nexo": 3, ',
      '"options": {"subintervals": 1, "laA": 300, "laDi": 500, "laD": 200, ',
      '"fastrefac": false, "max_threads": 1, "assertions": "warn", ',
      '"range_test_initial": "warn", "range_test_updated": "warn", ',
      '"postsim": false, "gpzerodivide": false}}'
    ),
    file.path(stats_dir, "sol.stats.json")
  )
  .solve_record_append(run_dir)
  rec <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_false(any(grepl("BLAS kernels", rec, fixed = TRUE)))
})

test_that("the solve record renders the per-phase memory table", {
  run_dir <- withr::local_tempdir()
  stats_dir <- file.path(run_dir, "out", "variables", "bin")
  dir.create(stats_dir, recursive = TRUE)
  writeLines("diag", file.path(run_dir, "model_diagnostics.txt"))
  writeLines(
    paste0(
      '{"version": 2, "solution_method": "Johansen", "matrix_method": "LU", ',
      '"mpi_size": 2, "vecsize": 10, "nexo": 3, ',
      '"options": {"subintervals": 1, "laA": 300, "laDi": 500, "laD": 200, ',
      '"fastrefac": false, "max_threads": 1, "assertions": "warn", ',
      '"range_test_initial": "warn", "range_test_updated": "warn", ',
      '"postsim": false, "gpzerodivide": false},\n',
      '  "rss_gb": {\n',
      '    "variable_calculation": {"max": 0.512, "sum": 0.900, "probes": 1},\n',
      '    "step": {"max": 1.250, "sum": 2.100, "probes": 3},\n',
      '    "peak": {"max": 1.300, "sum": 2.200}\n  }\n}'
    ),
    file.path(stats_dir, "sol.stats.json")
  )
  .solve_record_append(run_dir)
  rec <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl(
    "Memory (resident GB, max per rank / sum over ranks): variable calculation 0.51/0.90; step 1.25/2.10",
    rec,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Memory high-water mark: 1.30 GB max per rank, 2.20 GB sum over ranks",
    rec,
    fixed = TRUE
  )))
})

test_that("condest near-singularity warns without aborting the run", {
  paths <- local_solver_log(c(
    "solver banner",
    "condest: backward error omega1 0.00e+00 omega2 0.00e+00 (0 refinement passes), forward error bound 0.00e+00, kappa_w1 6.179e-02, kappa_w2 1.753e+15",
    "condest: WARNING: the linear system is numerically near-singular at the current values (kappa_w2 1.8e+15): solutions are unreliable; the structural probe may pass (-solmed probe) -- look for near-zero data flows carried by the closure",
    "all steps complete"
  ))
  expect_warning(
    suppressMessages(check_log(paths)),
    "numerically near-singular"
  )
  # and the kappa value is surfaced in the warning text
  expect_warning(
    suppressMessages(check_log(paths)),
    "1.8e\\+15"
  )
})

test_that("a non-zero exit status aborts even with a clean log", {
  paths <- local_solver_log(c("solver banner", "Step time 0.01 s"))
  expect_error(
    .check_solver_log(
      elapsed_time = proc.time(),
      solve_cmd = "cmd",
      paths = paths,
      call = NULL,
      status = 139L
    ),
    "exited with status 139"
  )
  # a zero status with a clean log passes as before
  expect_no_error(check_log(paths))
})
