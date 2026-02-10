build_compose_err <- function() {
  list(
    x_model_dir = "Model directory {.path {model_dir}} not found. This would indicate that the {.arg cmf_path} provided, {.path {cmf_path}}, is not correct.",
    x_var_out = "Model outputs not found. This would indicate that a model run at the {.arg cmf_path} provided, {.path {cmf_path}}, has not taken place.",
    set_mismatch = "Tablo-parsed sets and/or elements are not identical to post-model binary set outputs. If the Tablo file contains set writeout, this is likely an internal error and should be forwarded to the package maintainer. Otherwise, set {.arg post_set_check} to {.val FALSE} within {.fun teems::ems_option_set}.",
    idx_mismatch = "Index mismatch detected on output variables in {.fun teems::ems_compose}.",
    lax_check = "Lax column name check failed: One or more column names in parsed variable data.tables is not present in the tab extract.",
    strict_check = "Strict column name check failed: One or more column names is either not present or in a different order than that of the tab extract.",
    var_check = "Name discrepancy in parsed variables with respect to variable extract names.",
    coeff_check = "One or more coefficients identified from the Tablo extract was not found in the output csvs.",
    invalid_coeff_set = "It appears that a set isn't found which likely means a space around a coefficient set declaration (e.g., (all, r, REG) instead of (all,r,REG).",
    missing_sets = "Set information missing from binary outputs: {.field {x_sets}}."
  )
}
