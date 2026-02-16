build_aux_err <- function() {
  list(invalid_input = "{.arg {header}} must be a {.or {check}}, not {.obj_type_friendly {input}}.",
       no_inputs = "No inputs have been provided for any data type.",
       missing_col = "The data provided to {.arg {header}} does not contain a {.val Value} column.",
       x_dim = c("The dimensions of replacement data for {.field {input_name}}, {.val {dim}}, are not identical to the data being replaced: {.val {chk_dim}}.",
                 "For loading aggregated data, use the {.arg ...} argument of {.fun teems::ems_model}."),
       missing_dimname = c("The {.field {input_name}} element names provided for {.file {name}} do not contain all elements within the data being replaced.",
                           "Missing elements include {.val {missing_nme}}."),
       missing_dimname2 = c("The {.field {input_name}} element names provided for {.file {input_name}} do not contain all elements associated with the set {.field {s}}.",
                            "Missing elements include {.val {missing_nme}}."),
       invalid_dimname = c("The {.field {input_name}} set {.field {s}} is not found among loaded data, neither in terms of its name or constituitive elements."),
       beta_set = "Loading of full sets is a feature currently under development.")
}

build_aux_wrn <- function() {
  list(
    nme_dimnames = c("An inconsistent set name ({.field {odd_name}}) was identified when compared to the header to be replaced.",
                     "All associated set elements are present so this is likely due to a set used in multiple positions and will be presumed to be equivalent to {.field {presumed_name}}."),
    mod_dimname = "The set {.field {pre_s}} within {.field {input_name}} has been renamed to {.field {s}} due to identical elements."
  )
}
