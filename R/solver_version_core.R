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
