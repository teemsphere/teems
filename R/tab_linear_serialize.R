#' @keywords internal
#' @noRd
.serialize_linear <- function(terms) {
  if (length(terms) == 0L) {
    return("0")
  }
  text <- ""
  for (n in seq_along(terms)) {
    joint <- if (n == 1L) {
      ifelse(terms[[n]]$sign == 1L, "", "-")
    } else {
      ifelse(terms[[n]]$sign == 1L, " + ", " - ")
    }
    text <- paste0(text, joint, .serialize_term(terms[[n]]))
  }
  return(text)
}
