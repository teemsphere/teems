build_data_err <- function() {
  list(
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
    missing_set_mappings = "Set mappings are required as named arguments in {.arg ...}.",
    # test-GTAP_convert.R: "GTAP_convert GTAP-AEZ target (v10a AEZ, v6 format)"
    aez_v6_format = c(
      "The {.val GTAP-AEZ} target prepares the GTAPv7-format AEZ layer (GTAP 11/12 AEZ databases); the input is in the v6.2 format.",
      "The v6-format AEZ layer (GTAP 10a) carries different headers (ESBL, ETL1-3, ETA, YD01, YDEL over LAND_COMM/ENDWL_COMM/PROD_COMM) and is not supported."
    ),
    # test-GTAP_convert.R: "GTAP-AEZ preparation"
    aez_incomplete = c(
      "The GTAP-AEZ layer is incomplete: {cli::qty(missing_aez)}header{?s} {.val {missing_aez}} {?is/are} missing.",
      "GTAP-AEZ preparation needs the sets AEZS, COVS, CROP, LUSA, ACTS and REG and the data AREA, TONS and LCOV."
    ),
    no_name_mapping = "Set mappings must be passed as named pairs: {.code REG = \"mapping\"}"
  )
}

build_data_wrn <- function() {
  list(
    # test-ems_data.R: "ems_data warns wrong initial year"
    time_steps = "Initial timestep is neither {.val 0} nor the {.field dat} reference year ({.val {t0}}).",
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
    aez = "GTAP-AEZ layer detected: disaggregated activity sets and mapping (DACT, DCRP, DFRS, DGRZ, DLUA, MACT), CROPACTS/LCOV dimensions and AEZ parameters prepared.",
    dat = c(
      "GTAP Data Base version: {.field {full_database_version}}",
      "Reference year: {.field {reference_year}}",
      "Data format: {.field {data_format}}"
    )
  )
}
