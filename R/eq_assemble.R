#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.assemble_eq <- function(side_chunks,
                         quants,
                         eq_name,
                         qual,
                         label) {
  header <- paste0(purrr::map_chr(quants, "text"), collapse = "")
  lhs <- sub("^\\+\\s*", "", paste(side_chunks[[1]], collapse = " "))
  rhs <- sub("^\\+\\s*", "", paste(side_chunks[[2]], collapse = " "))
  if (lhs %=% "") {
    lhs <- "0"
  }
  if (rhs %=% "") {
    rhs <- "0"
  }
  stmt <- paste0(
    "Equation ", qual, eq_name, " ", label, header, " ",
    lhs, " = ", rhs
  )
  return(stmt)
}
