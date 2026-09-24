build_data_err <- function(layer_spec) {
  # the header inventories come from layer_spec so the prose cannot
  # drift from what .prepare_*() actually requires
  layer_incomplete <- function(flag) {
    spec <- layer_spec[[flag]]
    needs <- c(
      if (length(spec$required$set) > 0L) {
        paste("the sets", layer_and(spec$required$set))
      },
      if (length(spec$required$par) > 0L) {
        paste("the parameters", layer_and(spec$required$par))
      },
      if (length(spec$required$dat) > 0L) {
        paste("the data", layer_and(spec$required$dat))
      }
    )
    c(
      paste0(
        "The ", spec$label, " layer is incomplete: ",
        "{cli::qty(missing_headers)}header{?s} {.val {missing_headers}} ",
        "{?is/are} missing."
      ),
      paste0(
        spec$label, " preparation needs ",
        paste(needs, collapse = " and "), "."
      )
    )
  }

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
    # test-ems_data.R: "single-file route rejects set mappings"
    generic_mappings = "Set mappings cannot be applied on the single-file route ({.arg par_input} and {.arg set_input} absent); the file is loaded at full resolution.",
    # test-ems_data.R: "single-file route needs both or neither of par_input and set_input"
    partial_inputs = "{.arg par_input} and {.arg set_input} must be given together or both left {.code NULL} (single-file route).",
    # test-GTAP_convert.R: "GTAP_convert GTAP-AEZ target rejects the v6-format layer (v10a AEZ)"
    aez_v6_format = c(
      "The {.val GTAP-AEZ} target prepares the GTAPv7-format AEZ layer (GTAP 11/12 AEZ databases); the input is in the v6.2 format.",
      "The v6-format AEZ layer (GTAP 10a) carries different headers (ESBL, ETL1-3, ETA, YD01, YDEL over LAND_COMM/ENDWL_COMM/PROD_COMM) and is not supported."
    ),
    # test-GTAP_convert.R: "GTAP-AEZ preparation"
    aez_incomplete = layer_incomplete("aez"),
    # test-GTAP_convert.R: "GTAP_convert GTAP-E and GTAP-EP targets reject a v6-format database"
    e_v6_format = c(
      "The {.val GTAP-E} target prepares the GTAPv7-format GTAP-E layer (GTAP 11c/12a E databases); the input is in the v6.2 format.",
      "The v6-format GTAP-E layer (GTAP 10a) carries different headers and is not supported."
    ),
    # test-layer_prepare.R: "GTAP-E preparation on a synthetic layer"
    e_incomplete = layer_incomplete("e"),
    # test-layer_prepare.R: "GTAP-E preparation on a synthetic layer"
    e_topp_dim = c(
      "GTAP-E parameter {.field {e_header}} is dimensioned on {.val {e_dim}}, not {.val TOPP}.",
      "{.field {e_header}} supplies the CDE parameters the model reads over {.val TOPP}; a different dimension would bind the wrong values."
    ),
    # test-GTAP_convert.R: "GTAP_convert GTAP-E and GTAP-EP targets reject a v6-format database"
    ep_v6_format = c(
      "The {.val GTAP-EP} target prepares the GTAPv7-format GTAP-Power layer (GTAP 11c/12a Power databases); the input is in the v6.2 format.",
      "The v6-format GTAP-Power layer (GTAP 10a) carries different headers and is not supported."
    ),
    # test-layer_prepare.R: "GTAP-EP preparation on a synthetic layer"
    ep_incomplete = layer_incomplete("ep"),
    # test-layer_prepare.R: "GTAP-EP preparation on a synthetic layer"
    ep_load_split = c(
      "The GTAP-Power generation technologies {.val {ep_techs}} do not split into base load and peak load.",
      "The split is read from the {.val BL} and {.val P} name suffixes; the long labels are inconsistent and cannot stand in for them."
    ),
    # test-layer_prepare.R: "GTAP-E preparation on a synthetic layer"
    cde_range = c(
      "The CDE substitution parameter {.field {e_header}} reaches {.val {e_max}}, so {.code ALPHA = 1 - SUBPAR} would be negative.",
      "The {.val eny} row of the 11c releases is an un-normalised sum over the energy commodities rather than a share-weighted parameter; the 12a releases carry it correctly."
    ),
    # test-layer_prepare.R: "a COMM weight is recast onto TOPP"
    e_topp_weight = c(
      "GTAP-E weight {.field {e_header}} cannot be recast onto {.val TOPP} from {.val {e_dim}}.",
      "The CDE parameters are aggregated with private consumption over {.val COMM}, which {.val TOPP} must cover with exactly one aggregate element."
    ),
    # test-ems_data.R: "ems_data errors when dots passed without names"
    no_name_mapping = "Set mappings must be passed as named pairs: {.code REG = \"mapping\"}"
  )
}

build_data_wrn <- function() {
  list(
    # test-ems_data.R: "ems_data warns wrong initial year"
    time_steps = "Initial timestep is neither {.val 0} nor the {.field dat} reference year ({.val {t0}}).",
    # test-ems_data.R: "ems_data warns CSV with extra columns"
    invalid_user_input = "The {.field {map_name}} mapping has more than 2 columns; only columns 1 (origin) and 2 (destination) will be used.",
    # not in tests: no need to spoof this one
    db_version = c(
      "{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}.",
      "The {.fn teems::solve_in_situ} function can bypass the pipeline and be called on solver-ready input files."
    )
  )
}

build_data_info <- function(layer_spec) {
  # the set names each layer reports come from layer_spec, so a change
  # to what .prepare_*() synthesises reaches the message for free
  cde <- function(spec) {
    paste0(
      "the CDE parameters ",
      paste(names(spec$cde$pairs), collapse = "/"), " bound to ",
      paste(vapply(spec$cde$pairs, function(p) p$to, character(1)),
        collapse = "/"
      )
    )
  }
  aez <- layer_spec$aez
  e <- layer_spec$e
  ep <- layer_spec$ep

  list(
    # test-ems_data.R: "ems_data prepares a GTAP-AEZ database in place"
    aez = paste0(
      "GTAP-AEZ layer detected: disaggregated activity sets and mapping (",
      layer_group(aez, "activity"), "), ",
      paste(aez$rename_set, collapse = "/"),
      " dimensions and AEZ parameters prepared."
    ),
    # test-ems_data.R: "ems_data prepares a GTAP-E database in place"
    e = paste0(
      "GTAP-E layer detected: disaggregated commodity set and mapping (",
      layer_group(e, "commodity"), "), aggregated energy sets (",
      layer_group(e, "energy"), "), ", layer_group(e, "top"), ", and ",
      cde(e), "."
    ),
    # test-ems_data.R: "ems_data prepares a GTAP-Power database in place"
    ep = paste0(
      "GTAP-Power layer detected: disaggregated commodity set and mapping (",
      layer_group(ep, "commodity"), "), aggregated energy sets (",
      layer_group(ep, "energy"), "), the electricity nest sets (",
      paste0(names(ep$nest$families), "*", collapse = ", "), "), ",
      layer_group(ep, "top"), ", and ", cde(ep), "."
    ),
    # test-ems_data.R: "single-file route loads a non-GTAP HAR"
    generic = c(
      "Single-file database: {n_sets} set{?s} ({.val {set_names}}) and {n_headers} data headers, loaded at full resolution.",
      "No database version is recorded and set mappings do not apply on this route."
    ),
    # test-inform_messages.R: "loaded data reports its version, reference year and format"
    dat = c(
      "GTAP Data Base version: {.field {full_database_version}}",
      "Reference year: {.field {reference_year}}",
      "Data format: {.field {data_format}}"
    )
  )
}
