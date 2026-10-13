#' @keywords internal
#' @noRd
.name_tab_obj <- function(obj) {
  split <- .split_tab_qualifiers(obj$remainder)
  obj$qualifier_list <- split$qualifier_list
  quant <- .split_tab_quantifiers(split$rest)
  obj$name <- gsub("\\s", "", quant$rest)
  obj$remainder <- ifelse(is.na(quant$full_set), "", quant$full_set)
  return(obj)
}
