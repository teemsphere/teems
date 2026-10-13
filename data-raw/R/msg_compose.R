build_compose_err <- function() {
  list(
    # see compose_err$set_mismatch
    # test-compose_checks.R: "sets whose elements differ from the solver's are named before the abort"
    set_diff = "Sets whose parsed elements differ from the solver's: {.val {bad_sets}}.",
    # test-ems_compose.R: "ems_compose errors when model run has not taken place"
    no_sol = "No solution files found at {.path {cmf_path}}; the path is wrong or the model has not been run.",
    # test-compose_checks.R: "sets whose elements differ from the solver's are named before the abort"
    set_mismatch = "Tablo-parsed sets/elements do not match binary set outputs.",
    # test-compose_checks.R: "variable values that do not fill their index space abort"
    idx_mismatch = "Output variable index mismatch in {.fun teems::ems_compose}.",
    # test-compose_checks.R: "variable columns absent from the tab extract abort"
    lax_check = "Lax column check failed: one or more parsed variable column names are absent from the tab extract.",
    # test-compose_checks.R: "variable columns out of order against the tab extract abort"
    strict_check = "Strict column check failed: one or more column names are absent or out of order relative to the tab extract.",
    # test-compose_checks.R: "variable names that differ from the tab extract abort"
    var_check = "Parsed variable names do not match extract names.",
    # test-compose_checks.R: "sets absent from the binary outputs abort by name"
    missing_sets = "Set information missing from binary outputs: {.field {x_sets}}.",
    # test-ems_compose.R: "ems_compose errors when invalid which"
    invalid_name = "{.field {name}} is not present in output variables or coefficients.",
    # test-ems_compose.R: "which must name at least one output and passes must be a flag"
    which_empty = "{.arg which} must be {.val all} or a character vector of at least one variable or coefficient name.",
    # test-ems_compose.R: "which must name at least one output and passes must be a flag"
    passes_flag = "{.arg passes} must be {.val TRUE} or {.val FALSE}.",
    # test-ems_compose.R: "passes = TRUE with only coefficients in which aborts"
    passes_coefficients = c(
      "{.code passes = TRUE} adds the separate pass solutions of variables, but {.arg which} names only coefficients.",
      "Name a variable in {.arg which}, or drop {.arg passes}."
    ),
    # test-ems_compose.R: "passes = TRUE on a run without separate solutions aborts"
    no_passes = c(
      "This run wrote no separate pass solutions, so {.arg passes} cannot add them.",
      "They are written for a three-pass {.val Gragg}, {.val Midpoint} or {.val Euler} run (three step counts); a {.val Johansen} or Runge-Kutta run, or one with fewer passes, has none."
    ),
    # test-ems_compose.R: "a run without a coefficient dump composes variables only"
    no_coef_dump = c(
      "No coefficient dump ({.file sol.cof}/{.file sol.cbin}) found for this run, so coefficients cannot be returned.",
      "The run did not finish or the solver image predates the dump; re-run with a current image, or name only variables in {.arg which}."
    )
  )
}
