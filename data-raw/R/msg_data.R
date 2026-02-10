build_data_err <- function() {
  list(
    invalid_target = c("Invalid {.arg target_format}.",
    "Valid target formats currently include: {.val {valid_formats}}."),
    invalid_convert = "The HAR file provided for {.arg dat_input} is already of the {.arg target_format} {.val {target_format}}.",
    # missing_tab = "If {.arg time_steps} is provided for a dynamic model, a Tablo file must be provided.",
    wrong_input = "It appears that a non-dat file has been provided as a  {.arg dat_input}.",
    invalid_dat_har = "The header array file provided for {.arg dat_input} appears to be of type {.val {inferred_type}}, not {.val dat}.",
    invalid_par_har = "The header array file provided for {.arg par_input} appears to be of type {.val {inferred_type}}, not {.val par}.",
    invalid_set_har = "The header array file provided for {.arg set_input} appears to be of type {.val {inferred_type}}, not {.val set}.",
    invalid_time_step = "One or more {.arg time_steps} does not progress into the future.",
    data_set_mismatch = "The expected number of data entries on {.field {class(dt)[1]}} ({.val {expected}}) is not equal to the number found ({.val {nrow(dt)}}).",
    missing_tsteps = "{.arg time_steps} have not been provided to an intertemporal model. See {.fun teems::ems_data}.",
    nonreq_tsteps = "{.arg time_steps} have been provided yet no intertemporal sets have been detected in the provided model. See {.fun teems::ems_data}.",
    missing_mapping = "Some model sets that are read as headers are missing mappings: {.field {m_map}}.",
    missing_header = "The following headers are designated as read-in but are missing from the loaded data: {.val {missing_headers}}."
  )
}

build_data_wrn <- function() {
  list(
    time_steps = "The initial timestep provided is neither {.val {as.numeric(0)}} nor the reference year corresponding to the {.field dat} file loaded: {.val {t0}}.",
    unnecessary_cvrt = "The retrieved data format is identical to the {.arg target_format} specified. No conversion will take place."
  )
}
