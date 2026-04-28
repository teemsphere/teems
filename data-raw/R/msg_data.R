build_data_err <- function() {
  list(
    # test-ems_data.R: "ems_data rejects invalid target_format"
    invalid_target = c("Invalid {.arg target_format}.",
    "Valid target formats: {.val {valid_formats}}."),
    # test-ems_data.R: "ems_data rejects duplicate time_steps"
    invalid_time_step = "One or more {.arg time_steps} does not progress into the future.",
    # test-ems_data.R: "ems_data rejects CSV with insufficient columns"
    invalid_user_input = "The {.field {map_name}} mapping requires both an origin and destination column.",
    # test-ems_data.R: "ems_data rejects unrecognized set arguments with CSV mapping"
    missing_data = "No loaded set data corresponds to the {.field {map_name}} mapping.",
    # test-ems_data.R: "ems_data rejects invalid mapping values in CSV"
    missing_ele_mapping = "The {.field {map_name}} mapping has no entries for {.val {missing_ele}}.",
    # test-ems_data.R: "ems_data rejects unrecognized set arguments"
    no_internal_mapping = "No internal mappings exist for set {.field {map_name}}.",
    # test-ems_data.R: "ems_data rejects invalid internal mapping name"
    invalid_internal_mapping = c(
      "Internal mapping {.val {set_map}} does not exist for set {.val {map_name}}.",
      "Available internal mappings for {.val {map_name}} include {.val {available_map_names}}"
    ),
    # test-ems_data.R: "ems_data requires REG argument"
    missing_set_mappings = "Set mappings are required as named arguments in {.arg ...}."
  )
}

build_data_wrn <- function() {
  list(
    # test-ems_data.R: "ems_data warns wrong initial year"
    time_steps = "Initial timestep is neither {.val 0} nor the {.field dat} reference year ({.val {t0}}).",
    # test-ems_data.R: "ems_data unnecessary convert"
    unnecessary_cvrt = "Data format already matches {.arg target_format}; no conversion applied.",
    # test-ems_data.R: "ems_data rejects CSV with extra columns"
    invalid_user_input = "The {.field {map_name}} mapping has more than 2 columns; only columns 1 (origin) and 2 (destination) will be used.",
    # no need to spoof this one
    db_version = c(
      "{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}.",
      "The {.fn teems::solve_in_situ} function can bypass the pipeline and be called on solver-ready input files."
    ),
    mapping_case = "Some {.field {map_name}} elements converted to lowercase for consistency."
  )
}

build_data_info <- function() {
  list(
    dat = c(
      "GTAP Data Base version: {.field {full_database_version}}",
      "Reference year: {.field {reference_year}}",
      "Data format: {.field {data_format}}"
    )
  )
}
