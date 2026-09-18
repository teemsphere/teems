#' Elements of a set the IF rewrite synthesized for an element-equality
#' condition (`Set IFSn # ... # = "coa" & DCOMM;`). Such a set is a
#' quoted element intersected with the quantifier's range, so its
#' elements are read straight off the declaration; the caller narrows
#' them to the operand's own elements, which also covers an element
#' that is not in the range at all. The rewrite's other synthesized
#' form, a set-algebra remainder, is left unresolved.
#'
#' @keywords internal
#' @noRd
.if_rewrite_elements <- function(model, nm) {
  if (is.null(model)) {
    return(NULL)
  }
  rows <- which(model$type %in% "Set" & grepl(
    paste0("^\\s*Set\\s+", nm, "\\s*[#=]"), model$tab,
    ignore.case = TRUE
  ))
  if (!length(rows) %=% 1L) {
    return(NULL)
  }
  body <- trimws(sub(";\\s*$", "", sub("^[^=]*=", "", model$tab[rows[[1]]])))
  m <- regmatches(body, regexec(
    "^\"([^\"]+)\"\\s*&\\s*[A-Za-z_][A-Za-z0-9_]*$", body
  ))[[1]]
  if (length(m) == 0L) {
    return(NULL)
  }
  element <- tolower(m[2])
  return(element)
}
