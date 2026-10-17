# ems_solve errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# LU refuses more than one task and the bordered methods refuse more tasks than blocks (R5-B5, R5-B8)

    x `n_tasks` must be "1" with `matrix_method = "LU"`: the sequential LU path runs on one rank and every extra rank only replicates the model tables.
    i Use "SBBD", "DBBD" or "NDBBD" to spread the system over 2 tasks.

---

    x `n_tasks` (999) exceeds the 14 element of the largest set this deployment can split "DBBD" over.
    i Use at most 14 tasks, or another `matrix_method`.

# a docker tag that names no local image aborts with the full image name (R5-B2)

    x The "teems:nosuchtag" Docker image is not present.
    i For additional information see: teems-solver repository (<https://github.com/teemsphere/teems-solver>)

# ems_solve errors when n_tasks is not integerish

    x `n_tasks` must be integer-like.

# ems_solve errors when steps is not one to three whole numbers

    x `steps` must be one, two or three positive whole numbers.
    i One count (e.g. `steps = 8L`) is a single multi-step run without extrapolation; two or three increasing counts (e.g. `c(2L, 4L, 6L)`) are extrapolated, and three also give an accuracy estimate.

---

    x `steps` must be one, two or three positive whole numbers.
    i One count (e.g. `steps = 8L`) is a single multi-step run without extrapolation; two or three increasing counts (e.g. `c(2L, 4L, 6L)`) are extrapolated, and three also give an accuracy estimate.

# ems_solve errors when Gragg steps mix parity

    x `steps` must be all even or all odd when `solution_method` is "Gragg".
    i Richardson extrapolation of Gragg and midpoint solutions needs step counts of one parity (e.g. 2, 4, 6 or 3, 5, 7); even counts are recommended.

# the midpoint method solves and extrapolates

    x `steps` must be all even or all odd when `solution_method` is "Midpoint".
    i Richardson extrapolation of Gragg and midpoint solutions needs step counts of one parity (e.g. 2, 4, 6 or 3, 5, 7); even counts are recommended.

# accuracy_vars selects the variables of the accuracy summary

    x `accuracy_vars` must be a character vector of distinct variable names (letters, digits, `_` and `@`, starting with a letter).

---

    x `accuracy_vars` must be a character vector of distinct variable names (letters, digits, `_` and `@`, starting with a letter).

---

    x `accuracy_vars` selects the variables of the accuracy summary, which only a three-solution run of "Gragg", "Midpoint" or "Euler" has.
    i Give `steps` three step counts, or drop `accuracy_vars`.

# ems_solve errors when steps are not increasing

    x `steps` must be strictly increasing for `solution_method` "Gragg".
    i Richardson extrapolation combines solutions computed with distinct, increasing step counts.

---

    x `steps` must be strictly increasing for `solution_method` "Euler".
    i Richardson extrapolation combines solutions computed with distinct, increasing step counts.

# ems_solve errors on invalid Runge-Kutta arguments

    x `steps` must be a single positive integer when `solution_method` is "RK4".
    i Runge-Kutta methods take one step count (e.g. `steps = 8L`); they use no Richardson extrapolation, so no step-count triple is involved.

---

    x `steps` must be a single positive integer when `solution_method` is "DoPri54".
    i Runge-Kutta methods take one step count (e.g. `steps = 8L`); they use no Richardson extrapolation, so no step-count triple is involved.

---

    x `adaptive` "yes" requires an embedded Runge-Kutta `solution_method` ("BoSha32" or "DoPri54").
    i Only the embedded pairs provide the per-step error estimate the adaptive controller acts on.

---

    x `n_subintervals` must be 1 when `solution_method` is "BoSha32", except in a complementarity run with an accurate run.
    i Elsewhere subintervals restart the integrator and only benefit the extrapolating methods; increase `steps` (or use `adaptive`) instead. In a complementarity run each subinterval re-predicts the states (an approximate and an accurate run), with `steps` steps in each.

---

    x `eps_tolerance` must be a positive numeric of length 1.

# ems_solve errors when SBBD used with static model

    x `matrix_method` "SBBD" only applicable to intertemporal model runs.

# ems_solve errors when inmemory is not a logical scalar

    x `inmemory` must be a non-missing logical of length 1.

---

    x `inmemory` must be a non-missing logical of length 1.

# ems_solve errors when verbosity is invalid

    x `verbosity` must be integer-like.

---

    x `verbosity` must be 0, 1, or 2.

# ems_solve errors when solution errors detected

    Code
      ems_solve(cmf_path)
    Condition
      Error in `ems_solve()`:
      x The solver stopped on 1 runtime error while evaluating model values:
      i coefficient vdgb has an updated value below its declared lower bound 0
      i See GEMPACK manual section "25.4.4".
      i Full log: '<cache>/solve/solve_err_error/out/solver_out_HHMM.txt'.

# ems_deploy errors when the closure does not square the system

    Code
      ems_deploy(static_data, static_model, swap_out = "pop")
    Condition
      Error in `ems_deploy()`:
      x The closure does not square the system: 3497 endogenous variable elements against 3494 equation elements.
      i Arithmetic: 4478 variable elements - 981 exogenous elements (closure after swaps) = 3497 endogenous; the equation system determines exactly 3494, so 3 elements must still be exogenized.
      i Candidates: exogenizing 3 elements of one of pop, psave, qsave, pinv, kb closes the gap exactly.
      i If the counts look right but the partition is structurally deficient, run `teems::ems_probe()` on the deployed model for a named diagnosis.

# ems_solve warns when poor accuracy

    ! Only 33% of endogenous results (their levels values) accurate to at least 4 significant figures, below the 80% threshold.
    i Adjust with `accuracy_threshold`, `accuracy_figures` or `accuracy_criterion` in `teems::ems_option_set()`.

# ems_solve informs terminal run

    Code
      ems_solve(cmf_path, terminal_run = TRUE)
    Message
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --pull never --init -e TINI_KILL_PROCESS_GROUP=1 --name teems_ID --mount type=bind,src=<cache>/solve/solve_info_terminal,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; rm -f /opt/teems/out/variables/bin/sol{.bin,.est,.acc,.var,.sel,.set,.mds,.cof,.cbin,.stats.json,.cbin0,.xac,.cols,.cols.json,.jac,.jac.json,.outputs.json,.outputs.json.tmp,.probe.json,.probe.pattern} /opt/teems/out/{sets,coefficients}/*.csv; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0 -step1 2 -step2 4 -step3 6   -nsubints 1 -solmed Gragg -laA 300 -laDi 500 -laD 200 -verbosity 1 -maxthreads 1 -nox -assertions 2 -range_test_initial 2 -range_test_updated 1 -tempdir /tmp 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. To stop the run early, press Ctrl-C in that terminal, or run `docker kill
      teems_ID` from another one.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_info_terminal/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_info_terminal/GTAPv7.cmf")`

# a second solve in one deploy directory is refused by name

    Code
      ems_solve(cmf_path)
    Condition
      Error in `ems_solve()`:
      x A solve is already running in this deploy directory: run 010203_999 (pid 999) started 2026-01-01 01:02:03 UTC.
      i Concurrent runs need a deploy directory each: call `ems_option_set(tempdir = ...)` before `ems_deploy()` for every run. Two solves in one directory share the solver's scratch files and the outputs the CMF names, and would corrupt each other. A lock whose R process has ended is taken over automatically; one left by another machine on a shared folder must be removed by hand: '<cache>/solve/solve_lock/.solve_lock'.

# condensed deployments are warned against bordered methods (roadmap 6.2)

    Code
      ems_solve(cmf_path, matrix_method = "DBBD", n_tasks = 2L, terminal_run = TRUE)
    Condition
      Warning in `ems_solve()`:
      ! This deployment is condensed (2 backsolved variables, 0.9% of the uncondensed system) and "DBBD" is a bordered method.
      i Substitution densifies the diagonal blocks the bordered methods exploit: condensed deployments solve slower at every elimination share.
      i Condensation pays under "LU"; deploy without `backsolve` for bordered runs.
    Message
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --pull never --init -e TINI_KILL_PROCESS_GROUP=1 --name teems_ID --mount type=bind,src=<cache>/solve/solve_condense_advice,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; rm -f /opt/teems/out/variables/bin/sol{.bin,.est,.acc,.var,.sel,.set,.mds,.cof,.cbin,.stats.json,.cbin0,.xac,.cols,.cols.json,.jac,.jac.json,.outputs.json,.outputs.json.tmp,.probe.json,.probe.pattern} /opt/teems/out/{sets,coefficients}/*.csv; /opt/teems-solver/lib/mpi/bin/mpiexec -n 2 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 2 -step1 2 -step2 4 -step3 6   -nsubints 1 -solmed Gragg -laA 300 -laDi 500 -laD 200 -verbosity 1 -maxthreads 1 -nox -assertions 2 -range_test_initial 2 -range_test_updated 1 -refine 1 -tempdir /tmp 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. To stop the run early, press Ctrl-C in that terminal, or run `docker kill
      teems_ID` from another one.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_condense_advice/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_condense_advice/GTAPv7.cmf")`

# condensed intertemporal deployments are warned against (roadmap 6.2)

    Code
      ems_solve(cmf_path, solution_method = "Gragg", matrix_method = "SBBD",
        terminal_run = TRUE)
    Condition
      Warning in `ems_solve()`:
      ! This intertemporal deployment is condensed (2 backsolved variables, 0.9% of the uncondensed system).
      i Condensation is counterproductive on intertemporal models: bordered runs solve slower condensed, and a fully condensed "LU" run is slower still than plain "SBBD".
      i Deploy without `backsolve` and solve with "SBBD".
      i The memory estimate does not cover condensed "SBBD" runs: measured peaks reach several times the estimate, so the memory fit check cannot be relied on for this run.
    Message
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --pull never --init -e TINI_KILL_PROCESS_GROUP=1 --name teems_ID --mount type=bind,src=<cache>/solve/solve_condense_inter,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; rm -f /opt/teems/out/variables/bin/sol{.bin,.est,.acc,.var,.sel,.set,.mds,.cof,.cbin,.stats.json,.cbin0,.xac,.cols,.cols.json,.jac,.jac.json,.outputs.json,.outputs.json.tmp,.probe.json,.probe.pattern} /opt/teems/out/{sets,coefficients}/*.csv; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAP-RE.cmf -matsol 1 -step1 2 -step2 4 -step3 6   -nsubints 1 -solmed Gragg -laA 300 -laDi 500 -laD 200 -verbosity 1 -maxthreads 1 -nox -assertions 2 -range_test_initial 2 -range_test_updated 1 -tempdir /tmp 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. To stop the run early, press Ctrl-C in that terminal, or run `docker kill
      teems_ID` from another one.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_condense_inter/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_condense_inter/GTAP-RE.cmf")`

