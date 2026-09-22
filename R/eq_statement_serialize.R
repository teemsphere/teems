#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.serialize_eq_statement <- function(entry) {
  quant_text <- paste0(
    purrr::map_chr(entry$quants, \(q) {
      paste0("(all,", q$idx, ",", q$set, ")")
    }),
    collapse = ""
  )

  parts <- c(
    "Equation",
    if (!is.na(entry$qualifier_list)) {
      entry$qualifier_list
    },
    entry$name,
    if (!is.na(entry$label)) {
      paste0("# ", entry$label, " #")
    },
    quant_text,
    paste(
      .serialize_linear(entry$lhs),
      "=",
      .serialize_linear(entry$rhs)
    )
  )
  statement <- paste(parts[nzchar(parts)], collapse = " ")
  return(statement)
}
