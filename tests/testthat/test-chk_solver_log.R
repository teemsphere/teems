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

test_that("braces in a solver error line are relayed as written", {
  paths <- local_solver_log(c(
    "Error: normal{factor_1{r}} is not a coefficient, variable or number and cannot be an arithmetic operand (an index or quoted element compares through $POS, manual 11.5.6/11.4.11)"
  ))
  err <- tryCatch(check_log(paths), error = function(e) conditionMessage(e))
  expect_match(err, "normal{factor_1{r}} is not a coefficient", fixed = TRUE)
  expect_no_match(err, "{{", fixed = TRUE)
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

test_that(".map_solver_errors classifies the Tier A solver fatals", {
  mapped <- .map_solver_errors(c(
    "Formula for c1 reads a later element of c1 along the loop (backward recursion; formulas run forwards through their loops and use the most recent values, manual 16.5(b)/(c) -- copy the coefficient first): (all,t,time2) c1(t) = c1(t+1) + d(t)",
    "index offset t+nlag is not an integer constant (manual 11.2.4: an offset is index + <integer> or index - <integer>)",
    "index offset t+1 in k(t+1) runs outside set TIME (at element t10 of TIME; manual 16.4)",
    "the left-hand side of Formula c1 carries 1 argument(s); c1 is declared with 2 (manual 10.8, 11.4.10): c1(t) = 1",
    "index offsets are not allowed on the left-hand side of a Formula(Initial) (a Read in later steps; manual 10.8, 11.11.4): c1(t+1) = 1",
    "the shock statement for pop names component pop(usa), which is endogenous; only exogenous components can be shocked (manual 24, 24.14.1; shock file)",
    "some components of pop have been specified more than once (pop(usa) is shocked by two statements; manual 68.1.1; shock file)",
    "initial closure check: 10 endogenous components is not equal to the number of equation rows (11); 20 variable components, 10 exogenous, 0 backsolved -- make 1 more component(s) endogenous (manual 23.2.7)",
    "LOOP statements are not supported (loops in TAB files, manual 11.18): loop (all,i,com)",
    "a strong comment opened with '![[!' in the TAB file is never closed by '!]]!' (1 still open at the end of the file; manual 11.1.5)",
    "product Update of vfm: the right-hand side must be a product of percentage-change variables v1*v2*...*vn (manual 11.12.4), and 2 is not one; write a (change) Update for any other form: (all,i,com) vfm(i) = 2*p(i)",
    "Formula for x gives a value that is not finite (NaN) at x(usa): a division, LOGE, SQRT or power left its domain or the value overflowed (arithmetic error, manual 34.3): x(r) = loge(y(r))",
    "the linear solve gave a value that is not finite (NaN) for qo(usa): the LHS matrix is singular or badly scaled at this step, or a coefficient overflowed (manual 34.1, 34.3)",
    "more than 100 equations were not satisfied very accurately (101 warnings); all files are written, but the solution may not be valid (manual 30.6.1)"
  ))
  expect_identical(
    mapped$class,
    c(rep("tab", 5), rep("closure", 3), rep("tab", 3), rep("numeric", 3))
  )
  expect_identical(
    mapped$manual,
    c(
      "16.5", "11.2.4", "16.4", "10.8", "11.11.4",
      "24.14.1", "68.1.1", "23.2.7",
      "11.18", "11.1.5", "11.12.4",
      "34.3", "34.1", "30.6.1"
    )
  )
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

test_that("an option or manifest statement the image does not know maps to the interface abort", {
  paths <- local_solver_log(c(
    "teems-solver 1.1.0-dev.4",
    "Error: unknown command-line option -subtotals: teems-solver 1.1.0-dev.4 does not read it (teems and its solver image are released together; use the image that matches this teems version, and see docs/solver-reference.md section 11 for the options)"
  ))
  expect_error(check_log(paths), "does not accept 1 input that teems sent")
  expect_error(check_log(paths), "unknown command-line option -subtotals")
  expect_error(check_log(paths), "released together")
  paths <- local_solver_log(
    "Error: unknown statement at line 272 of the manifest (.cmf) file (expected one of iodata, outdata, soldata, tabfile, closure, shock): subtotal \"/opt/teems/sub.sts\";"
  )
  expect_error(check_log(paths), "does not accept 1 input that teems sent")
  expect_error(check_log(paths), "subtotal", fixed = TRUE)
})

test_that("a misspelt TAB keyword maps to the model-specification abort", {
  paths <- local_solver_log(
    "Error: unknown statement keyword 'coeficient' (a statement without a keyword continues the previous coefficient statement, manual 11.1.1, and this one cannot): coeficient (all,r,reg) k8(r) ;"
  )
  expect_error(check_log(paths), "rejected the model specification")
  expect_error(check_log(paths), "11.1.1")
})

test_that("shock-group subtotal errors map to the subtotal abort", {
  paths <- local_solver_log(
    "Error: element mars is not in set reg (in pop; subtotal \"bad\"; subtotals file)"
  )
  expect_error(check_log(paths), "rejected the subtotal \\(shock-group\\) request with 1 error")
  expect_error(check_log(paths), "element mars is not in set reg", fixed = TRUE)
  paths <- local_solver_log(
    "Error: subtotals (manual 29) are not available with matrix_method NDBBD, which cannot keep its factorization for more solves yet; use matrix_method LU, SBBD or DBBD with the Johansen, Euler or Gragg method"
  )
  expect_error(check_log(paths), "rejected the subtotal")
  mapped <- .map_solver_errors(c(
    "subtotal \"Twice\" is defined twice (subtotals file)",
    "cannot open subtotals file /opt/teems/sub.sts: No such file or directory",
    "subtotals (manual 29) are not available in a model with complementarities yet: the approximate and accurate runs change the closure and the states between steps, so the step right-hand sides are not the shocks alone (manual 52)",
    "this run solves extra right-hand sides with each step's factorization (-fhtest), which matrix_method NDBBD cannot keep yet; use matrix_method LU, SBBD or DBBD",
    "the manifest (.cmf) file has more than one subtotals statement; put every subtotal in one file"
  ))
  expect_identical(mapped$class, c("subtotal", "subtotal", "subtotal", "subtotal", "interface"))
})
