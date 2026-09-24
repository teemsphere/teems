#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.closure_add_omitted <- function(closure,
                                 omit_vars) {
  if (length(omit_vars) == 0L) {
    return(closure)
  }
  cls_var <- purrr::map_chr(strsplit(closure, "\\(", perl = TRUE), 1)
  missing <- omit_vars[!tolower(omit_vars) %in% tolower(trimws(cls_var))]
  if (length(missing) == 0L) {
    return(closure)
  }
  out <- c(closure, missing)
  attributes(out) <- attributes(closure)
  return(out)
}
