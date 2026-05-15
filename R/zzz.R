#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  data_dir <- tools::R_user_dir("teems")

  if (dir.exists(data_dir)) {
    unlink(data_dir, recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}