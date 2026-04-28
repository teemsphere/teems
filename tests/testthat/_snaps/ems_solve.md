# ems_solve errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# ems_solve errors when n_tasks is not integerish

    x `n_tasks` must be integer-like.

# ems_solve errors when steps is not length 3

    x `steps` must be a numeric vector of length 3.

# ems_solve errors when steps are mixed odd/even

    x `n_subintervals` must be all even or all odd.

# ems_solve errors when SBBD used with static model

    x `matrix_method` "SBBD" only applicable to intertemporal model runs.

# ems_solve errors when solution errors detected

    Code
      ems_solve(cmf_path = cmf_path)
    Condition
      Error in `ems_solve()`:
      x Errors detected during solution. See '/home/mpc/.local/share/R/teems/deploy/solve_err_error/out/solver_out_HHMM.txt'.

# ems_solve errors when solution singularity detected

    Code
      ems_solve(cmf_path = cmf_path)
    Condition
      Error in `ems_solve()`:
      x Singularity detected during solution. See '/home/mpc/.local/share/R/teems/deploy/solve_err_sing/out/solver_out_HHMM.txt'.

# ems_solve warns when poor accuracy

    ! Only 45% of variables accurate to at least 4 digits, below the 80% threshold.
    i Adjust with `accuracy_threshold` in `teems::ems_option_set()`.

# ems_solve informs terminal run

    Code
      ems_solve(cmf_path = cmf_path, terminal_run = TRUE)
    Message
      i `terminal_run` activated. To solve and compose outputs:
      1. Run the following command in your OS terminal: docker run --rm --mount
      type=bind,src=/home/mpc/.local/share/R/teems/deploy/solve_info_terminal,dst=/opt/teems
      teems:latest /bin/bash -c "/opt/teems-solver/lib/mpi/bin/mpiexec -n 1
      /opt/teems-solver/solver/hsl -cmdfile /opt/teems/GTAPv7.cmf -matsol 0 -regset
      REG -nsubints 1 -solmed Johansen -nesteddbbd 0 -presol 1 -laA 300 -laDi 500
      -laD 200 -maxthreads 1 -nox 2>&1 | tee /opt/teems/out/solver_out_HHMM.txt"
      2. If errors are present in the terminal output during an ongoing run, it is
      possible to stop the relevant hsl process early according to your OS-specific
      system activity monitor.
      3. Any error and/or singularity indicators will be present in the model
      diagnostic output:
      '/home/mpc/.local/share/R/teems/deploy/solve_info_terminal/out/solver_out_HHMM.txt'.
      4. If no errors or singularities are detected, use the following expression to
      structure solver binary outputs:
      `ems_compose("/home/mpc/.local/share/R/teems/deploy/solve_info_terminal/GTAPv7.cmf")`

