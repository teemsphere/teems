build_gen_err <- function() {
  list(
    # test-ems_swap.R: "ems_swap errors when var is not character"
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when var is not character", "ems_uniform_shock errors when value is not numeric"
    # test-ems_custom_shock.R: "ems_custom_shock errors when var is not character", "ems_custom_shock errors when input is numeric"
    # test-ems_scenario_shock.R: "ems_scenario_shock errors when var is not character", "ems_scenario_shock errors when input is numeric"
    # test-ems_data.R: "ems_data rejects non-character dat_input", "ems_data rejects non-character par_input", "ems_data rejects non-character set_input", "ems_data rejects non-character aux_input", "ems_data rejects non-character REG"
    # test-ems_model.R: "ems_model rejects non-character model_file", "ems_model rejects non-character closure_file", "ems_model rejects non-character var_omit"
    # test-ems_compose.R: "ems_compose errors when name is not character"
    class = "{.arg {arg_name}} must be a {.or {check}}, not {.obj_type_friendly {arg}}.",
    # test-ems_data.R: "ems_data rejects non-existent CSV file"
    # test-ems_model.R: "ems_model rejects non-existent model_file file", "ems_model rejects non-existent closure_file"
    no_file = "Cannot open file {.file {file}}: No such file.",
    # test-ems_data.R: "ems_data rejects wrong file extension for mapping"
    invalid_file = "{.arg {arg}} must be a {.or {.val {valid_ext}}} file, not {?a/an} {.val {file_ext}} file."
  )
}
