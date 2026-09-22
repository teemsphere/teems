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
    # test-compose_checks.R: "a coefficient file that carries another coefficient aborts"
    coeff_check = "One or more Tablo-identified coefficients absent from model output.",
    # test-compose_checks.R: "a coefficient dimensioned on an unknown set aborts"
    invalid_coeff_set = "Set not found; a space in a coefficient set declaration may be the cause, e.g. {.code (all, r, REG)} instead of {.code (all,r,REG)}.",
    # test-compose_checks.R: "sets absent from the binary outputs abort by name"
    missing_sets = "Set information missing from binary outputs: {.field {x_sets}}.",
    # test-ems_compose.R: "ems_compose errors when invalid which"
    invalid_name = "{.field {name}} is not present in output variables or coefficients.",
    # test-ems_compose.R: "a run without a coefficient dump or CSVs warns and returns variables only"
    no_coefficients = c(
      "No coefficient outputs found for this run; only variables are returned.",
      "The solver image predates the binary coefficient dump ({.file sol.cof}). Update the image, or deploy with {.code write_coefficients = TRUE} for the per-coefficient CSVs."
    )
  )
}
