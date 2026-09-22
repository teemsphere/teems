#' @keywords internal
#' @noRd
.eq_entry <- function(eq_name,
                      eqs,
                      math_extract,
                      tab,
                      var_lookup,
                      call) {
  key <- tolower(eq_name)
  if (!is.null(eqs[[key]])) {
    return(eqs[[key]])
  }

  r_idx <- match(key, tolower(math_extract$name))
  row <- math_extract[r_idx, ]
  statement <- tab[[row$row_id]]

  quants <- .parse_eq_quants(
    eq_name = eq_name,
    statement = statement,
    call = call
  )

  sides <- lapply(c(row$comp1, row$comp2), \(comp) {
    if (is.na(comp)) {
      entry <- list()
      return(entry)
    }
    tryCatch(
      .prune_zero_terms(.parse_linear_side(comp, var_lookup)),
      error = \(e) {
        parse_reason <- conditionMessage(e)
        .cli_action(model_err$condense_parse,
          action = c("abort", "inform"),
          call = call
        )
      }
    )
  })

  entry <- list(
    name = row$name,
    label = row$label,
    qualifier_list = row$qualifier_list,
    quants = quants,
    lhs = sides[[1]],
    rhs = sides[[2]],
    dirty = FALSE,
    row_id = row$row_id
  )
  eqs[[key]] <- entry
  return(entry)
}
