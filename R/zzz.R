#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  cache_dir <- tools::R_user_dir("teems", "cache")
  data_dir  <- tools::R_user_dir("teems", "data")

  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE, force = TRUE)
  }

  if (dir.exists(data_dir)) {
    unlink(data_dir, recursive = TRUE, force = TRUE)
  }

  invisible(NULL)
}