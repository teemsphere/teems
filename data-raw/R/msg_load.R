build_load_err <- function() {
  list(
    # nested_class = "Input data must be provided as a {.or {data_class}}, not {.obj_type_friendly {errant_class}}.",
    invalid_input = "The input header provided {.field {nme}} is not among loaded data headers: {.field {existing_headers}}.",
    no_val_col = "Input data for the header {.field {nme}} does contain {.val Value} as the final column.",
    agg_missing_tup = "{tup$n} tuple{?s} in the provided input file for {.val {nme}} were missing: {.field {tup$missing}}.",
    injection_missing_col = c(
      "Input data for the coefficient {.field {nme}} does not contain all required columns (sets).",
      "The required columns are {.field {req_col}}."
    ),
    missing_eqm_input = "If {.arg eqm_input} is not provided, both {.arg dat_input} and {.arg par_input} are required inputs.",
    missing_set_mappings = "Set mappings passed to {.arg ...} as a pairwise list are required.",
    invalid_internal_mapping = c(
      "The internal mapping selected: {.val {set_map}}, for set {.val {map_name}} does not exist.",
      "Available internal mappings for {.val {map_name}} include {.val {available_map_names}}"
    ),
    no_internal_mapping = "No internal mappings were found for the set {.field {map_name}}.",
    extra_input = "If {.arg eqm_input} is provided, {.arg dat_input}, {.arg par_input}, and {.arg set_input} arguments are unnecessary.",
    invalid_set_qual = "Invalid set qualifier detected: {.val {invalid_qual}}.",
    set_parse_fail = "Remnant set label detected during Tablo parsing.",
    set_op_fail = c(
      "Multiple {.val +} and/or {.val -} were detected within a single Tablo Set statement.",
      "For compatibility, split into multiple statements: Instead of A123=A1+A2+A3, A12=A1+A2 then A123=A12+A3"
    ),
    identical_set_fail = c(
      "It appears that one set has been defined as identical to a second set (e.g., Set SET_B # example set B # = SET_A;).",
      "If duplicate sets are desired, multiple Read statements should be implemented (e.g., Set SET_A # example set A # maximum size 5 read elements from file GTAPSETS header \"H2\";Set SET_B # example set B # maximum size 5 read elements from file GTAPSETS header \"H2\";)"
    )
  )
}
