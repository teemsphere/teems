#' @keywords internal
#' @noRd
.csub_ref <- function(name, dims) {
  if (length(dims) == 0L) {
    return(name)
  }
  ref <- paste0(name, "(", paste(dims, collapse = ","), ")")
  return(ref)
}
