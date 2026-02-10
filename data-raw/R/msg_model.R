build_model_err <- function() {
  list(
    invalid_internal = c(
      "The internal Tablo file specified: {.val {file}} is not supported.",
      "Currently supported internal Tablo files include: {.field {valid_internal_files}}.",
      "Alternatively, path to a user-provided Tablo file can be supplied (e.g., \"/my/{file_type}/path.tab\").",
      "Note that user-provided model files may need to be modified for compatibility with various {.pkg teems} functions."
    ),
    invalid_coeff = "The data input provided {.arg {nme}} is not declared as a coefficient.",
    invalid_read = "Partial Read statements are not supported.",
    invalid_mod = "{.arg nme} is neither read in nor appearing on the LHS of a formula.",
    invalid_numeric = c("Numeric value directly assigned must be length one.",
                        "Use a {.code data.frame} with the appropriate sets to assign multiple heterogeneous numeric values to a coefficient."),
    unsupported_tab = "Unsupported Tablo declarations detected: {.val {unsupported}}.",
    invalid_int_header = c(
      "The {header_descr} required is currently set as {.val {timestep_header}} but this header is not loaded within the Tablo file.",
      "See {.fun teems::ems_option_set} {.arg {arg_name}} for setting the {header_descr}."
    ),
    missing_file = "Read statements missing \"from file\" detected",
    binary_switch = "A colon was detected within a {.field Set} definition indicating the use of an unsupported binary switch. Declare sets explicitly within the Tablo file (e.g.,  ENDWM # mobile endowment # (capital,unsklab,sklab); {.emph not} ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);"
  )
}

build_model_wrn <- function() {
  list(
    ETRE = "The {.arg ETREtoENDW} value for {.fun teems::ems_option_get()} is set to `TRUE` however no dedicated sluggish endowment set has been detected for {.field ETRE}."
  )
}
