#' @keywords internal
#' @noRd
.check_write_dir <- function(write_dir,
                             call) {
  subdir <- .o_write_sub_dir()

  if (!dir.exists(write_dir)) {
    created <- dir.create(write_dir, recursive = FALSE, showWarnings = FALSE)

    if (!created) {
      .cli_action(deploy_err$invalid_write_dir,
        action = "abort",
        call = call
      )
    }

    .cli_action(deploy_wrn$mkdir,
      action = "warn",
      call = call
    )
  }

  write_dir <- normalizePath(write_dir)

  if (file.access(write_dir, 2) != 0L) {
    .cli_action(deploy_err$write_dir_not_writable,
      action = "abort",
      call = call
    )
  }

  sub_path <- file.path(write_dir, subdir)

  if (dir.exists(sub_path)) {
    existing <- list.files(sub_path, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0) {
      .cli_action(gen_info$unlink,
        action = "inform",
        call = call
      )
      unlink(file.path(sub_path, "*"), expand = TRUE)
    }
  } else {
    dir.create(sub_path, recursive = FALSE)
  }

  return(sub_path)
}