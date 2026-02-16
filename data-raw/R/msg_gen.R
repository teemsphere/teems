build_gen_err <- function() {
  list(
    class = "{.arg {arg_name}} must be a {.or {check}}, not {.obj_type_friendly {arg}}.",
    dir_not_file = "A filepath is expected, not the directory {.file {file}}.",
    no_file = "Cannot open file {.file {file}}: No such file.",
    invalid_file = "{.arg {arg}} must be a {.or {.val {valid_ext}}} file, not {?a/an} {.val {file_ext}} file."
  )
}