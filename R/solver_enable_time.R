#' @keywords internal
#' @noRd
.solver_enable_time <- function(paths) {
  if ("tab_path" %in% names(attributes(paths$cmf))) {
    tab <- readLines(attr(paths$cmf, "tab_path"))
  } else {
    tab <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = paths$cmf
    )
    tab <- readLines(tab)
  }
  enable_time <- any(grepl(pattern = "\\(\\s*intertemporal\\s*\\)", tab, ignore.case = TRUE))
  return(enable_time)
}
