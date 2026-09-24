#' @keywords internal
#' @noRd
.binding_used <- function(binding,
                          expr) {
  ids <- .expr_idents(expr)
  hit <- match(tolower(names(binding)), tolower(ids))
  used <- binding[!is.na(hit)]
  names(used) <- ids[hit[!is.na(hit)]]
  return(used)
}
