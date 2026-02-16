build_model_err <- function() {
  list(
    invalid_var_omit = "{.val {invalid_var}} designated for variable omission not found within the provided model input.",
    invalid_coeff = "The aggregated data coefficient {.arg {nme}} is not declared within the provided model input.",
    invalid_read = "Partial Read statements are not supported.",
    # leaving this in but there is not possible?
    invalid_mod = "{.arg nme} is neither read in nor appearing on the LHS of a formula.",
    invalid_numeric = c("Numeric value directly assigned must be length one.",
                        "Use a {.code data.frame} with the appropriate sets to assign multiple heterogeneous numeric values to a coefficient."),
    invalid_state = c("The current version of teems does not support {.field {inv_state}}.",
                      "Supported statements include: {.field {supported_state}}."),
    # probably a redundant check but if a weird unrecognized statement is found then there needs to be a way of distinuishing between an implied statement and an unrecognized statement
    unsupported_tab = "Unsupported Tablo declarations detected: {.field {unsupported}}.",
    invalid_int_header = c(
      "The intertemporal {header_descr} required is currently {.val {timestep_header}} but this header is not loaded.",
      "See {.fun teems::ems_option_set} {.arg {arg_name}} for setting a custom {header_descr}."
    ),
    missing_file = "Read statements missing \"from file\" detected.",
    binary_switch = c("A colon was detected within a {.field Set} definition indicating the use of an unsupported binary switch.",
                      "Declare sets explicitly within the Tablo file or using {.arg ...} within {.fun teems::ems_model}.",
                      "For example, {.field Set ENDWM # mobile endowment # (capital,unsklab,sklab);} {.emph not} {.field Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);}."),
    identical_set_fail = c(
      "It appears that one set has been defined as identical to a second set: {.field Set SET_B # example Set B # = SET_A;}.",
      "If duplicate sets are desired, multiple Read statements should be implemented.",
      "For example {.field Set SET_A # example set A # maximum size 5 read elements from file GTAPSETS header \"H2\";} {.emph and} {.field Set SET_B # example set B # maximum size 5 read elements from file GTAPSETS header \"H2\";)}"
    ),
    invalid_set_qual = "Invalid set qualifier detected: {.val {invalid_qual}}.",
    # not in tests
    set_parse_fail = "Remnant set label detected during Tablo parsing.",
    set_op_fail = c(
      "Multiple {.val +} and/or {.val -} were detected within a single Tablo Set statement.",
      "For compatibility, split into multiple statements.", 
      'Instead of {.field Set ENDWCFS # multiple op # = ENDWC + ENDWF + ENDWS;}, {.field Set ENDWCF # one op # = ENDWC + ENDWF;} {.emph and} {.field Set ENDWCFS # second op # = ENDWCF + ENDWS;}.'
    ),
    injection_missing_col = c(
      "Input data for the coefficient {.field {nme}} does not contain all required columns (sets).",
      "The required columns are {.field {req_col}}."
    )
  )
}

build_model_wrn <- function() {
  list(
    ignored_state = "The following model statements {.field {ign_state}} are unsupported and will be ignored."
  )
}
