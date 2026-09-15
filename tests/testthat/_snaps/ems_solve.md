# ems_solve errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# ems_solve errors when n_tasks is not integerish

    x `n_tasks` must be integer-like.

# ems_solve errors when steps is not length 3

    x `steps` must be a numeric vector of length 3.

# ems_solve errors when steps are not all even for Gragg

    x `steps` must be all even when `solution_method` is "Gragg".
    i Gragg's method guarantees its accuracy properties for even step counts only (Pearson 1991, Theorem 6.1).

---

    x `steps` must be all even when `solution_method` is "Gragg".
    i Gragg's method guarantees its accuracy properties for even step counts only (Pearson 1991, Theorem 6.1).

# ems_solve errors when steps are not increasing

    x `steps` must be strictly increasing for `solution_method` "Gragg".
    i Richardson extrapolation combines three solutions computed with distinct, increasing step counts.

---

    x `steps` must be strictly increasing for `solution_method` "Euler".
    i Richardson extrapolation combines three solutions computed with distinct, increasing step counts.

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

    x `n_subintervals` must be 1 when `solution_method` is "BoSha32".
    i Subintervals restart the integrator and only benefit the extrapolating methods; increase `steps` (or use `adaptive`) instead.

---

    x `eps_tolerance` must be a positive numeric of length 1.

# ems_solve errors when SBBD used with static model

    x `matrix_method` "SBBD" only applicable to intertemporal model runs.

# ems_solve errors when inmemory is not a logical scalar

    x `inmemory` must be a NULL or logical, not a string.

---

    x `inmemory` must be logical of length 1.

# ems_solve errors when verbosity is invalid

    x `verbosity` must be integer-like.

---

    x `verbosity` must be 0, 1, or 2.

# ems_solve errors when solution errors detected

    Code
      ems_solve(cmf_path, range_test_updated = "fatal")
    Message
      i `matrix_method` "auto": using "LU" for this static model.
    Condition
      Error in `ems_solve()`:
      x The solver stopped on 1 runtime error while evaluating model values:
      i coefficient vdgb has a value below its declared lower bound 0.000000
      i See GEMPACK manual section "25.4.4".
      i Full log: '<cache>/solve/solve_err_error/out/solver_out_HHMM.txt'.

# ems_deploy errors when the closure does not square the system

    Code
      ems_deploy(static_data, static_model, swap_out = "pop")
    Condition
      Error in `ems_deploy()`:
      x The closure does not square the system: 3497 endogenous variable elements against 3494 equation elements.
      i Arithmetic: 4478 variable elements - 981 exogenous elements (closure after swaps) = 3497 endogenous; the equation system determines exactly 3494, so 3 elements must still be exogenized.
      i Candidates: exogenizing 3 elements of one of psave, qsave, pinv, kb, qst closes the gap exactly.
      i If the counts look right but the partition is structurally deficient, run `teems::ems_probe()` on the deployed model for a named diagnosis.

# ems_solve warns when poor accuracy

    ! Only 45% of variables accurate to at least 4 digits, below the 80% threshold.
    i Adjust with `accuracy_threshold` in `teems::ems_option_set()`.

# ems_solve informs terminal run

    Code
      ems_solve(cmf_path, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_info_terminal,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_info_terminal/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_info_terminal/GTAPv7.cmf")`

# matrix_method auto resolves by model type

    Code
      ems_solve(cmf_path, matrix_method = "auto", terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_auto_static,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_auto_static/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_auto_static/GTAPv7.cmf")`

---

    Code
      ems_solve(cmf_path, solution_method = "Gragg", matrix_method = "auto",
        terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "SBBD" for this intertemporal model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_auto_dynamic,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAP-RE.cmf -matsol 1 -step1 2 -step2 4 -step3 8   -nsubints 1 -solmed Gragg -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_auto_dynamic/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_auto_dynamic/GTAP-RE.cmf")`

# matrix_method auto selects DBBD for static deployments from the probe

    Code
      ems_solve(cmf_path, n_tasks = 2L, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": probing the deployed system's structure (chain dimension, block partition) to choose the method.
      i `matrix_method` "auto": using "DBBD" for this static model.
      i Structural evidence: 900,000 equations, no chain, partition reg (3 blocks, border 6.5%), n_tasks 2.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_auto_dbbd,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 2 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 2    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_auto_dbbd/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_auto_dbbd/GTAPv7.cmf")`

---

    Code
      ems_solve(cmf_path, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
      i This static model favors "DBBD": `n_tasks = 2` (or `resources = "auto"`) with `matrix_method` "auto" may solve faster than single-task "LU".
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_auto_dbbd,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_auto_dbbd/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_auto_dbbd/GTAPv7.cmf")`

---

    Code
      ems_solve(cmf_path, n_tasks = 4L, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": probing the deployed system's structure (chain dimension, block partition) to choose the method.
      i `matrix_method` "auto": using "LU" for this static model.
      i Structural evidence: 900,000 equations, no chain, partition demd (9 blocks, border 15.6%), n_tasks 4.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_auto_dbbd,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 4 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_auto_dbbd/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_auto_dbbd/GTAPv7.cmf")`

# matrix_method auto skips the probe on a large plain static system and decides from the region count

    Code
      ems_solve(cmf_path, n_tasks = 2L, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "DBBD" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_auto_metadata,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 2 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 2    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_auto_metadata/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_auto_metadata/GTAPv7.cmf")`

---

    Code
      ems_solve(cmf_path, n_tasks = 4L, terminal_run = TRUE)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_auto_metadata,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 4 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 0    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_auto_metadata/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_auto_metadata/GTAPv7.cmf")`

# a second solve in one deploy directory is refused by name

    Code
      ems_solve(cmf_path)
    Message
      i `matrix_method` "auto": using "LU" for this static model.
    Condition
      Error in `ems_solve()`:
      x A solve is already running in this deploy directory: run 010203_999 (pid 999) started 2026-01-01 01:02:03 UTC.
      i Concurrent runs need a deploy directory each: call `ems_option_set(tempdir = ...)` before `ems_deploy()` for every run. Two solves in one directory share the solver's scratch files and the outputs the CMF names, and would corrupt each other. If a previous run was interrupted, remove '<cache>/solve/solve_lock/.solve_lock'.

# matrix_method auto probes the deployed structure and records the decision

    Code
      out <- ems_solve(cmf_path, n_tasks = 2L, pre_probe = TRUE)
    Message
      i `matrix_method` "auto": probing the deployed system's structure (chain dimension, block partition) to choose the method.
      i `matrix_method` "auto": using "DBBD" for this static model.
      i Structural evidence: 2,500,000 equations, no chain, partition reg (3 blocks, border 6.5%), n_tasks 2.
      i Structural probe: full structural rank 3494 of 3494; the closure is structurally valid.

# resources auto resolves tasks and threads from the container and records them

    `resources` must be one of "manual" or "auto", not "yes".

# condensed deployments are advised against bordered methods (roadmap 6.2)

    Code
      ems_solve(cmf_path, matrix_method = "DBBD", n_tasks = 2L, terminal_run = TRUE)
    Message
      i This deployment is condensed (2 backsolved variables, 0.9% of the uncondensed system) and "DBBD" is a bordered method.
      i Substitution densifies the diagonal blocks the bordered methods exploit: condensed deployments solve slower at every elimination share.
      i Condensation pays under "LU"; deploy without `backsolve` for bordered runs (`omit` is unaffected -- omission does not densify).
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_condense_advice,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 2 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAPv7.cmf -matsol 2    -nsubints 1 -solmed Johansen -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_condense_advice/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_condense_advice/GTAPv7.cmf")`

# condensed intertemporal deployments are advised against (roadmap 6.2)

    Code
      ems_solve(cmf_path, solution_method = "Gragg", matrix_method = "SBBD",
        terminal_run = TRUE)
    Message
      i This intertemporal deployment is condensed (2 backsolved variables, 0.9% of the uncondensed system).
      i Condensation is counterproductive on intertemporal models: bordered runs solve slower condensed, and a fully condensed "LU" run is slower still than plain "SBBD".
      i Deploy without `backsolve` and solve with "SBBD" (`omit` is unaffected -- omission does not densify).
      i `terminal_run` activated. To solve and compose outputs:
      docker run --rm --mount type=bind,src=<cache>/solve/solve_condense_inter,dst=/opt/teems teems:TAG /bin/bash -c "set -o pipefail; /opt/teems-solver/lib/mpi/bin/mpiexec -n 1 /opt/teems-solver/solver/teems-solver -cmdfile /opt/teems/GTAP-RE.cmf -matsol 1 -step1 2 -step2 4 -step3 8   -nsubints 1 -solmed Gragg -laA 300 -laDi 500 -laD 200   -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      
      1. Run the above command in your OS terminal.
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant teems-solver process early according to your
      OS-specific system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '<cache>/solve/solve_condense_inter/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("<cache>/solve/solve_condense_inter/GTAP-RE.cmf")`

