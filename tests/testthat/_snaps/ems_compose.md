# ems_compose errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# ems_compose errors when which is not character

    x `which` must be a character, not a number.

# ems_compose errors when invalid which

    x not_a_var is not present in output variables or coefficients.

# ems_compose errors when passes is not logical

    x `passes` must be a logical, not a string.

# ems_compose errors when cmf_path does not exist

    x Cannot open file 'not_a_path': No such file.

# a run without a coefficient dump or CSVs warns and returns variables only

    ! No coefficient outputs found for this run; only variables are returned.
    i The solver image predates the binary coefficient dump ('sol.cof'). Update the image, or deploy with `write_coefficients = TRUE` for the per-coefficient CSVs.

# ems_compose errors when model run has not taken place

    Code
      ems_compose(cmf_path)
    Condition
      Error in `ems_compose()`:
      x No solution files found at '<cache>/compose/GTAP-RE.cmf'; the path is wrong or the model has not been run.

# passes = TRUE on a run without separate solutions aborts

    x This run wrote no separate pass solutions, so `passes` cannot add them.
    i They are written for a three-pass "Gragg", "Midpoint" or "Euler" run (three step counts); a "Johansen" or Runge-Kutta run, or one with fewer passes, has none.

