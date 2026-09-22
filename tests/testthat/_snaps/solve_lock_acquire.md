# a lock without an owner record is attributed to an earlier run

    Code
      .solve_lock_acquire(run_dir = run_dir, timeID = "010203_1", call = NULL)
    Condition
      Error in `.solve_lock_acquire()`:
      x A solve is already running in this deploy directory: an earlier run.
      i Concurrent runs need a deploy directory each: call `ems_option_set(tempdir = ...)` before `ems_deploy()` for every run. Two solves in one directory share the solver's scratch files and the outputs the CMF names, and would corrupt each other. If a previous run was interrupted, remove '<run_dir>/.solve_lock'.

