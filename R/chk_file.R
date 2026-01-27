#' @keywords internal
#' @noRd
.check_file <- function(file,
                        valid_ext,
                        call,
                        ...) {
  UseMethod(".check_file")
}

#' @keywords internal
#' @noRd
#' @method .check_file internal
#' @export
.check_file.internal <- function(file,
                                 valid_ext,
                                 call,
                                 ...) {
  valid_internal_files <- names(get(paste("internal", valid_ext, sep = "_")))
  if (!file %in% valid_internal_files) {
    .cli_action(model_err$invalid_internal,
      action = "abort",
      call = call
    )
  }

  class(file) <- c(valid_ext, class(file))
  return(file)
}

#' @keywords internal
#' @noRd
#' @method .check_file user
#' @export
.check_file.user <- function(file,
                             valid_ext,
                             call,
                             file_ext,
                             ...) {
  if (dir.exists(file)) {
    .cli_action(
      gen_err$dir_not_file,
      action = "abort",
      call = call
    )
  }

  if (!file.exists(file)) {
    .cli_action(gen_err$no_file,
      action = "abort",
      call = call
    )
  } else if (!file_ext %in% valid_ext) {
    arg <- names(as.list(call))[as.list(call) == file]
    .cli_action(gen_err$invalid_file,
      action = "abort",
      call = call
    )
  }

  classes <- c(valid_ext, class(file))
  file <- normalizePath(file)
  class(file) <- classes
  return(file)
}