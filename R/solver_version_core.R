#' `MAJOR.MINOR.PATCH` of a version string, pre-release suffix
#' (`-dev.4`) and R's development component (`.9000`) dropped: a dev
#' build of 1.1.0 carries the 1.1.0 interface.
#'
#' @keywords internal
#' @noRd
.solver_version_core <- function(x) {
  core <- regmatches(x, regexpr("^[0-9]+\\.[0-9]+\\.[0-9]+", x))
  if (!length(core)) {
    return(NA)
  }
  core <- numeric_version(core)
  return(core)
}
