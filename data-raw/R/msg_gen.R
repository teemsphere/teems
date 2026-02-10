build_gen_err <- function() {
  list(
    class = "{.arg {arg_name}} must be a {.or {check}}, not {.obj_type_friendly {arg}}.",
    dir_not_file = "A filepath is expected, not the directory {.file {file}}.",
    no_file = "Cannot open file {.file {file}}: No such file.",
    invalid_file = "{.arg {arg}} must be a {.or {.val {valid_ext}}} file, not {?a/an} {.val {file_ext}} file."
  )
}

build_gen_wrn <- function() {
  list(
    db_version = c(
      "{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}.",
      "The {.fn teems::emssolve} function can bypass the pipeline and be called on solver-ready input files."
    )
  )
}

build_gen_info <- function() {
  list(
    dat = c(
      "GTAP Data Base version: {.field {full_database_version}}",
      "Reference year: {.field {reference_year}}",
      "Data format: {.field {data_format}}"
    )
  )
}

build_gen_url <- function() {
  list(internal_files = NULL)
}
