#' @keywords internal
#' @noRd
.formula_lhs_rows <- function(model, nme) {
  is_f <- model$type %in% "Formula" & !is.na(model$comp1)
  lhs <- rep(NA_character_, nrow(model))
  lhs[is_f] <- tolower(trimws(sub("[[({].*$", "", model$comp1[is_f])))
  rows <- which(lhs %in% tolower(nme))
  return(rows)
}
