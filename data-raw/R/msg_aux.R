build_aux_err <- function() {
  list(
    # test-ems_aux.R: "ems_aux errors when input is numeric", "ems_aux errors when input is logical"
    invalid_input = "{.arg {header}} must be a {.or {check}}, not {.obj_type_friendly {input}}.",
    # test-ems_aux.R: "ems_aux errors when header is missing for data frame input", "ems_aux errors when header is missing for CSV input"
    missing_header = "{.arg header} must be provided if {.arg input} is a data frame or path to a CSV file.",
    # test-ems_aux.R: "ems_aux errors when data frame lacks Value column"
    missing_col = "The data provided to {.arg {header}} does not contain a {.val Value} column.",
    # test-ems_aux.R: "ems_data errors when aux data has wrong dimensions"
    x_dim = c("The dimensions of replacement data for {.field {input_name}}, {.val {dim}}, are not identical to the data being replaced: {.val {chk_dim}}.",
              "For loading aggregated data, use the {.arg ...} argument of {.fun teems::ems_model}."),
    # test-ems_aux.R: "ems_data errors when aux data is missing elements for replacement header"
    missing_dimname = c("{.field {input_name}} element names provided for {.field {name}} do not contain all elements within the data being replaced.",
                        "Missing elements include {.val {missing_nme}}."),
    # test-ems_aux.R: "ems_data errors when aux data is missing elements for new header"
    missing_dimname2 = c("Element names provided for {.field {input_name}} do not contain all elements associated with the set {.field {s}}.",
                         "Missing elements include {.val {missing_nme}}."),
    # test-ems_aux.R: "ems_data errors when unrecognized set and incomplete ele", "ems_data errors when aux data has unrecognizable set"
    invalid_dimname = c("The {.field {input_name}} set {.field {s}} is not found among loaded data, neither in terms of its name or constituitive elements."))
}

build_aux_wrn <- function() {
  list(
    # test-ems_aux.R: "ems_data issues warning when changing name on replacement header with recognized ele"
    nme_dimnames = c("An inconsistent set name ({.field {odd_name}}) was identified when compared to the header to be replaced.",
                     "All associated set elements are present so this is likely due to a set used in multiple positions and will be presumed to be equivalent to {.field {presumed_name}}."),
    # test-ems_aux.R: "ems_data issues warning when renaming aux data set with recognized ele", "ems_data issues warning when changing name on new header with recognized ele"
    mod_dimname = "The set {.field {pre_s}} within {.field {input_name}} has been renamed to {.field {s}} due to identical elements.",
    # test-ems_aux.R: "ems_data issues warning when type is different for replacement header"
    diff_data_type = "The {.arg type} provided to {.field {input_name}} differs from that of the data it replaces: {.val {classes[[2]]}}."
  )
}
