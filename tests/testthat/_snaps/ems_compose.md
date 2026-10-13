# ems_compose errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# ems_compose errors when which is not character

    x `which` must be a character, not a number.

# ems_compose errors when invalid which

    x not_a_var is not present in output variables or coefficients.

# ems_compose errors when passes is not logical

    x `passes` must be a logical, not a string.

# which must name at least one output and passes must be a flag (R6-B8)

    x `which` must be "all" or a character vector of at least one variable or coefficient name.

---

    x `passes` must be "TRUE" or "FALSE".

---

    x `passes` must be "TRUE" or "FALSE".

# passes = TRUE with only coefficients in which aborts (R6-B8)

    x `passes = TRUE` adds the separate pass solutions of variables, but `which` names only coefficients.
    i Name a variable in `which`, or drop `passes`.

# ems_compose errors when cmf_path does not exist

    x Cannot open file 'not_a_path': No such file.

# a run without a coefficient dump composes variables only

    x No coefficient dump ('sol.cof'/'sol.cbin') found for this run, so coefficients cannot be returned.
    i The run did not finish or the solver image predates the dump; re-run with a current image, or name only variables in `which`.

# ems_compose errors when model run has not taken place

    Code
      ems_compose(cmf_path)
    Condition
      Error in `ems_compose()`:
      x No solution files found at '<cache>/compose/GTAP-RE.cmf'; the path is wrong or the model has not been run.

# passes = TRUE on a run without separate solutions aborts

    x This run wrote no separate pass solutions, so `passes` cannot add them.
    i They are written for a three-pass "Gragg", "Midpoint" or "Euler" run (three step counts); a "Johansen" or Runge-Kutta run, or one with fewer passes, has none.

