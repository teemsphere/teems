build_exp_err <- function() {
  list(
    # test-ems_example.R: "ems_example errors when path does not exist"
    invalid_path = "{.path {path}} does not exist or is not writable.",
    # test-ems_example.R: "ems_example errors when type is scripts and an input is missing"
    missing_input = "{.arg dat_input}, {.arg par_input}, and {.arg set_input} must be provided when {.arg type} is {.val scripts}."
  )
}