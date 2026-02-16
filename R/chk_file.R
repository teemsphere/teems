#' @importFrom tools file_ext
#' @keywords internal
#' @noRd
.check_file <- function(file,
                        valid_ext,
                        call,
                        file_ext) {
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

  ext <- tools::file_ext(file)
  classes <- c(ext, class(file))
  file <- normalizePath(file)
  class(file) <- classes
  return(file)
}