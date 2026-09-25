#' @keywords internal
#' @noRd
.fac_text <- function(fac, ops) {
  if (length(fac) == 0L) {
    return("")
  }
  text <- fac[[1]]
  if (length(ops) > 0L && ops[[1]] %=% "/") {
    text <- paste0("1/", text)
  }
  for (f in seq_along(fac)[-1]) {
    text <- paste0(text, ops[[f]], fac[[f]])
  }
  return(text)
}
