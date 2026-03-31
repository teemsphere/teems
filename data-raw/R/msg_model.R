build_model_err <- function() {
  list(
    # test-ems_model.R: "ems_model rejects invalid variable names in var_omit"
    invalid_var_omit = "{.val {invalid_var}} designated for variable omission not found within the provided model input.",
    # test-ems_model.R: "ems_model rejects invalid coefficient arguments"
    invalid_coeff = "The aggregated data coefficient {.arg {nme}} is not declared within the provided model input.",
    # test-ems_model.R: "partial read statement"
    invalid_read = "Partial {.field Read} statements are not supported.",
    # leaving this in but there is not possible?
    invalid_mod = "{.arg nme} is neither read in nor appearing on the LHS of a formula.",
    # test-ems_model.R: "invalid numeric to a formula"
    invalid_numeric = c("Numeric value directly assigned must be length one.",
                        "Use a {.code data.frame} with the appropriate sets to assign multiple heterogeneous numeric values to a coefficient."),
    # test-ems_model.R: "invalid tab statement"
    invalid_state = c("teems {version} does not support {.field {inv_state}} statements.",
                      "Supported statements include: {.field {supported_state}}."),
    # probably a redundant check but if a weird unrecognized statement is found then there needs to be a way of distinguishing between an implied statement and an unrecognized statement
    unsupported_tab = "Unsupported Tablo declarations detected: {.field {unsupported}}.",
    # test-ems_model.R: "invalid intertemporal header"
    invalid_int_header = c(
      "The intertemporal {header_descr} required is currently {.val {timestep_header}} but this header is not loaded.",
      "See {.fun teems::ems_option_set} {.arg {arg_name}} for setting a custom {header_descr}."
    ),
    # test-ems_model.R: "invalid read statement"
    missing_file = "Read statements missing \"from file\" detected.",
    # test-ems_model.R: "invalid binary set switch statement"
    binary_switch = c("A colon was detected within a {.field Set} definition indicating the use of an unsupported binary switch.",
                      "Declare sets explicitly within the Tablo file or using {.arg ...} within {.fun teems::ems_model}.",
                      "For example, {.field Set ENDWM # mobile endowment # (capital,unsklab,sklab);} {.emph not} {.field Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);}."),
    # test-ems_model.R: "identical set assignment"
    identical_set_fail = c(
      "It appears that one set has been defined as identical to a second set: {.field Set SET_B # example Set B # = SET_A;}.",
      "If duplicate sets are desired, multiple Read statements should be implemented.",
      "For example {.field Set SET_A # example set A # maximum size 5 read elements from file GTAPSETS header \"H2\";} {.emph and} {.field Set SET_B # example set B # maximum size 5 read elements from file GTAPSETS header \"H2\";)}"
    ),
    # test-ems_model.R: "invalid set qualifier"
    invalid_set_qual = "Invalid set qualifier detected: {.field {invalid_qual}}.",
    # not in tests
    set_parse_fail = "Remnant set label detected during Tablo parsing.",
    # test-ems_model.R: "multiple set operators"
    set_op_fail = c(
      "Multiple {.val +} and/or {.val -} were detected within a single Tablo Set statement.",
      "For compatibility, split into multiple statements.",
      'Instead of {.field Set ENDWCFS # multiple op # = ENDWC + ENDWF + ENDWS;}, {.field Set ENDWCF # one op # = ENDWC + ENDWF;} {.emph and} {.field Set ENDWCFS # second op # = ENDWCF + ENDWS;}.'
    ),
    # test-ems_model.R: "data frame input missing a set"
    injection_missing_col = c(
      "Input data for the coefficient {.field {nme}} does not contain all required columns (sets).",
      "The required columns are {.field {req_col}}."
    ),
    # test-ems_model.R: "invalid var in closure"
    no_var = "The closure provided contains variables not present in the model: {.val {var_discrepancy}}.",
    # the following error should never be issued (full will be assigned)
    entry_type = "The following closure entries have not been classified properly: {invalid_entry}.",
    missing_specification = "The closure provided must contain both an {.val Exogenous} entry and a {.val Rest Endogenous} entry. Note that the inverse approach is not currently supported."
    )
}

build_model_wrn <- function() {
  list(
    # test-ems_model.R: "ignored tab statement"
    ignored_state = "The following model statements are unsupported and will be ignored: {.field {ign_state}}."
  )
}
