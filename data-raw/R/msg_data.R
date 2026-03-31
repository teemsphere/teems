build_data_err <- function() {
  list(
    # test-ems_data.R: "ems_data rejects invalid target_format"
    invalid_target = c("Invalid {.arg target_format}.",
    "Valid target formats currently include: {.val {valid_formats}}."),
    # test-ems_data.R: "ems_data rejects duplicate time_steps"
    invalid_time_step = "One or more {.arg time_steps} does not progress into the future.",
    # test-ems_data.R: "ems_data rejects CSV with insufficient columns"
    invalid_user_input = "The set mapping loaded for {.field {map_name}} does not contain both an origin element column and a mapping column.",
    # test-ems_data.R: "ems_data rejects unrecognized set arguments with CSV mapping"
    missing_data = "There is no loaded set data which corresponds to the loaded set map: {.field {map_name}}.",
    # test-ems_data.R: "ems_data rejects invalid mapping values in CSV"
    missing_ele_mapping = "The set mapping loaded for {.field {map_name}} is missing mappings for {.val {missing_ele}}.",
    # test-ems_data.R: "ems_data rejects unrecognized set arguments"
    no_internal_mapping = "No internal mappings were found for the set {.field {map_name}}.",
    # test-ems_data.R: "ems_data rejects invalid internal mapping name"
    invalid_internal_mapping = c(
      "The internal mapping selected: {.val {set_map}}, for set {.val {map_name}} does not exist.",
      "Available internal mappings for {.val {map_name}} include {.val {available_map_names}}"
    ),
    # test-ems_data.R: "ems_data requires REG argument"
    missing_set_mappings = "Set mappings passed to {.arg ...} as a pairwise list are required."
  )
}

build_data_wrn <- function() {
  list(
    # test-ems_data.R: "ems_data warns wrong initial year"
    time_steps = "The initial timestep provided is neither {.val {as.numeric(0)}} nor the reference year corresponding to the {.field dat} file loaded: {.val {t0}}.",
    # test-ems_data.R: "ems_data unnecessary convert"
    unnecessary_cvrt = "The retrieved data format is identical to the {.arg target_format} specified. No conversion has taken place.",
    # test-ems_data.R: "ems_data rejects CSV with extra columns"
    invalid_user_input = "The set mapping loaded for {.field {map_name}} contains more than 2 columns. Only the first (origin element) and second (mapped element) columns will be utilized.",
    # no need to spoof this one
    db_version = c(
      "{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}.",
      "The {.fn teems::solve_in_situ} function can bypass the pipeline and be called on solver-ready input files."
    )
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
