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

#' @keywords internal
#' @noRd
.solver_version_key <- function(x) {
  m <- regmatches(x, regexec("^([0-9]+)\\.([0-9]+)\\.([0-9]+)(-dev\\.([0-9]+))?", x))[[1]]
  if (!length(m)) {
    return(NA)
  }
  pre <- if (nzchar(m[[6]])) c("0", m[[6]]) else "1"
  key <- numeric_version(paste(c(m[2:4], pre), collapse = "."))
  return(key)
}
