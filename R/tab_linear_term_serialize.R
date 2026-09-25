#' @keywords internal
#' @noRd
.serialize_term <- function(term) {
  prod <- .fac_text(term$fac, term$ops)
  if (!is.null(term$var)) {
    var_text <- term$var$name
    if (length(term$var$args) > 0L) {
      var_text <- paste0(var_text, "(", paste(term$var$args, collapse = ","), ")")
    }
    prod <- ifelse(nzchar(prod), paste0(prod, "*", var_text), var_text)
  }
  if (!nzchar(prod)) {
    prod <- "1"
  }
  for (q in rev(term$quants)) {
    prod <- paste0("sum{", q$idx, ",", q$set, q$cond %|||% "", ", ",
                   prod, "}")
  }
  return(prod)
}
