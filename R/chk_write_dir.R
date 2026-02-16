#' @importFrom tools R_user_dir
#' 
#' @keywords internal
#' @noRd
.check_write_dir <- function(write_dir,
                             call) {

  subdir <- .o_write_sub_dir()
  if (write_dir %!=% tools::R_user_dir("teems", "cache")) {
    write_dir <- normalizePath(write_dir)
    if (!dir.exists(write_dir)) {
      .cli_action(deploy_err$invalid_write_dir,
        action = "abort",
        call = call
      )
    }

    write_dir <- file.path(write_dir, subdir)
  }

  unlink(write_dir,
    recursive = TRUE
  )

  dir.create(write_dir,
    recursive = TRUE
  )
  return(write_dir)
}