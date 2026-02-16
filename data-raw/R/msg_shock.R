build_shk_err <- function() {
  list(
    cst_scen_val_file = "The last column in the loaded file {.file {input}} must be a {.field Value} column.",
    cst_scen_val_df = "{.obj_type_friendly {input}} supplied as a shock must have {.field Value} as the last column.",
    scen_year_file = "No {.field Year} column was found in the loaded file {.file {input}}.",
    scen_year_df = "{.obj_type_friendly {input}} supplied as a scenario shock must have a column designating {.field Year} consistent with selected time steps.",
    uni_named_lst = "Within subsetted uniform shocks, {.arg ...} must consist of named pairs in the format {.code SETi = \"set_element\"} or {.code SETi = c(\"set_element1\", \"set_element1\")}.",
    not_a_shk = "The value provided to {.arg shock} is not an object created with {.fun teems::emsshock}.",
    not_a_var = "The variable designated for a shock: {.val {var_name}}  was not found within the model Tablo file.",
    extra_col = "If {.field Year} is provided in lieu of the intertemporal set, the intertemporal set {.field {supplied_int_set}} is not necessary.",
    invalid_set = c(
      "{l_errant_set} set{?s} designated for an element-specific uniform shock: {.field {errant_set}} not applicable to the variable {.val {var_name}}.",
      "Set designations within {.pkg teems} comprise the variable-specific uppercase set name and lowercase index.",
      "For {.val {var_name}} these include: {.field {ls_mixed}}.",
      "In intertemporal models, {.field Year} may be provided in lieu of an intertemporal set."
    ),
    x_full_exo = "The variable {.field {raw_shock$var}} was assigned a shock over the entire variable yet is not fully exogenous.",
    x_full_exo_part = "The variable {.field {raw_shock$var}} was assigned a shock over part of the variable yet no components are exogenous.",
    uni_invalid_year = "The Year provided for a shock {.val {Year}} is not among years consistent with provided time steps {.field {CYRS}}.",
    uni_invalid_ele = c(
      "The element: {.val {ele}} is not found within the associated set: {.field {ele_set}}",
      "Valid elements with the current mapping include: {.val {recognized_ele}}."
    ),
    x_part_exo = "The following tuples have been allocated a shock but are not exogenous: {.field {errant_tup}}.",
    invalid_year = "{n_errant_year_tuples} tuple{?s} in the provided {class(shk)[[1]]} shock contain invalid {.field Year} specifications: {.field {errant_year_tuples}}.",
    cust_invalid_tup = "Some tuples provided to a {.arg custom} shock indicate elements used that do not belong to their respective sets: {.field {errant_tuples}}.",
    cust_endo_tup = "Some tuples designated for a shock do not have exogenous status: {.field {x_exo_parts}}.",
    scen_dynamic = "Shock type {.arg scenario} is only valid for temporally dynamic models.",
    scen_missing_tup = c(
      "{n_missing_tuples} tuple{?s} in the provided scenario shock file were missing: {.field {missing_tuples}}.",
      "Note that scenario shocks are subject to aggregation and must contain all unaggregated database- and variable-specific elements for associated sets."
    ),
    shk_file_shocks = "No additional shocks are accepted if a shock file is provided.",
    uni_named_lst = "Note that set names consist of the concatenation of the set name and variable-specific lowercase index."
  )
}