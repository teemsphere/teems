#' @keywords internal
#' @noRd
.validate_compose_args <- function(a,
                                   call) {
  a$cmf_path <- .check_input(
    file = a$cmf_path,
    valid_ext = "cmf",
    call = call
  )

  checklist <- list(
    cmf_path = "character",
    which    = "character",
    passes   = "logical"
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  if (length(a$which) == 0L || anyNA(a$which) || !all(nzchar(a$which))) {
    .cli_action(compose_err$which_empty,
      action = "abort",
      call = call
    )
  }
  if (length(a$passes) != 1L || is.na(a$passes)) {
    .cli_action(compose_err$passes_flag,
      action = "abort",
      call = call
    )
  }

  a$sol_prefix <- file.path(dirname(a$cmf_path), "out", "variables", "bin", "sol.")

  if (list.files(dirname(a$sol_prefix)) %=% character(0)) {
    cmf_path <- normalizePath(a$cmf_path, "/")
    .cli_action(compose_err$no_sol,
      action = "abort",
      call = call
    )
  }

  return(a)
}
