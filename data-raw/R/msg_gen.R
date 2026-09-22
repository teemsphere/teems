build_gen_err <- function() {
  list(
    # test-ems_swap.R: "ems_swap errors when var is not character"
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when var is not character", "ems_uniform_shock errors when value is not numeric"
    # test-ems_custom_shock.R: "ems_custom_shock errors when var is not character", "ems_custom_shock errors when input is numeric"
    # test-ems_scenario_shock.R: "ems_scenario_shock errors when var is not character", "ems_scenario_shock errors when input is numeric"
    # test-ems_data.R: "ems_data rejects non-character dat_input", "ems_data rejects non-character par_input", "ems_data rejects non-character set_input" "ems_data rejects non-character REG"
    # test-ems_model.R: "ems_model rejects non-character model_file", "ems_model rejects non-character closure_file"
    # test-ems_compose.R: "ems_compose errors when which is not character"
    class = "{.arg {arg_name}} must be a {.or {check}}, not {.obj_type_friendly {arg}}.",
    # test-ems_data.R: "ems_data rejects non-existent CSV file"
    # test-ems_model.R: "ems_model rejects non-existent model_file file", "ems_model rejects non-existent closure_file"
    no_file = "Cannot open file {.file {file}}: No such file.",
    # test-ems_data.R: "ems_data rejects wrong file extension for mapping"
    invalid_file = "{.arg {arg}} must be a {.or {.val {valid_ext}}} file, not {?a/an} {.val {file_ext}} file.",
    # ems_option_set()/the R6 options class validators
    # test-ems_options.R: "ems_option_get errors on invalid name"
    opt_name = "{.arg name} must be one of {.val {valid}}, not {.val {name}}.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_verbose = "{.arg verbose} must be TRUE or FALSE.",
    # test-ems_options.R: "ems_option errors when write_dir does not exist"
    opt_tempdir = "{.path {tempdir}} does not exist.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_ndigits = "{.arg ndigits} must be an integer or coercible to one.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_accuracy_threshold = "{.arg accuracy_threshold} must be a numeric between 0 and 1.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_check_shock_status = "{.arg check_shock_status} must be TRUE or FALSE.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_timestep_header = "{.arg timestep_header} must be an upper case character vector.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_n_timestep_header = "{.arg n_timestep_header} must be an upper case character vector.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_full_exclude = "{.arg full_exclude} must be a character vector.",
    # test-ems_options.R: "ems_option_set rejects invalid values"
    opt_docker_tag = "{.arg docker_tag} must be a character vector.",
    # test-ems_options.R: "ems_option_set sets version_check and rejects other values"
    opt_version_check = "{.arg version_check} must be one of {.val abort}, {.val warn} or {.val off}.",
    # test-ems_options.R: "ems_option_set sets the solver run modes and rejects other values"
    opt_assertions = "{.arg assertions} must be one of {.val fatal}, {.val warn} or {.val off}.",
    # test-ems_options.R: "ems_option_set sets the solver run modes and rejects other values"
    opt_range_test_initial = "{.arg range_test_initial} must be one of {.val fatal}, {.val warn} or {.val off}.",
    # test-ems_options.R: "ems_option_set sets the solver run modes and rejects other values"
    opt_range_test_updated = "{.arg range_test_updated} must be one of {.val fatal}, {.val warn} or {.val off}.",
    # one-line internal aborts
    # test-ems_compose.R: "ems_compose errors when cmf_path is missing"
    missing_arg = "argument {.arg {arg}} is missing, with no default",
    # test-cli_cmds.R: "a message with a url closes on a named or a plain link"
    href_named = "For additional information see: {.href [{hyperlink}]({url})}",
    # test-cli_cmds.R: "a message with a url closes on a named or a plain link"
    href_plain = "For additional information see: {.href {url}}",
    # not in tests: internal assert
    ele_swap_internal = "Internal error on an ele to ele swap-out.",
    # test-chk_tab_preflight.R: "subsets by numbers abort"
    subset_by_numbers = "Subset '(by numbers)' argument not supported.",
    # test-solve_in_situ.R: "solve_in_situ errors when ignore_condense is not a single TRUE or FALSE"
    logical_flag = "{.arg {bad_arg}} must be {.val TRUE} or {.val FALSE}."
  )
}

build_gen_info <- function() {
  list()
}
