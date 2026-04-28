build_exp_err <- function() {
  list(
    # test-ems_example.R: "ems_example errors when write_dir does not exist"
    invalid_write_dir = "The path provided for {.arg write_dir}, {.path {write_dir}}, does not exist.",
    # test-ems_example.R: "ems_example errors when type is scripts and dat_input is missing"
    # test-ems_example.R: "ems_example errors when type is scripts and par_input is missing"
    # test-ems_example.R: "ems_example errors when type is scripts and set_input is missing"
    missing_input = "{.arg dat_input}, {.arg par_input}, and {.arg set_input} must be provided when {.arg type} is {.val scripts}."
  )
}