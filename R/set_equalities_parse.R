#' @keywords internal
#' @noRd
.parse_set_equalities <- function(sets,
                                  is_builder,
                                  call) {
  is_set_eq <- !is.na(sets$definition) &
    grepl("^\\s*=\\s*[A-Za-z_][A-Za-z0-9_]*\\s*$", sets$definition)

  for (i in which(is_set_eq)) {
    rhs_nm <- trimws(sub("^\\s*=\\s*", "", sets$definition[i]))
    if (tolower(rhs_nm) %=% tolower(sets$name[i])) {
      bad_set <- sets$name[i]
      .cli_action(model_err$set_self_eq,
        action = "abort",
        call = call
      )
    }
    rhs_idx <- match(rhs_nm, sets$name)
    if (is.na(rhs_idx)) {
      rhs_idx <- match(tolower(rhs_nm), tolower(sets$name))
      if (is.na(rhs_idx)) {
        bad_stmt <- paste("Set", sets$name[i], sets$definition[i])
        bad_refs <- rhs_nm
        .cli_action(model_err$set_undeclared,
          action = c("abort", "inform"),
          call = call
        )
      }
      sets$definition[i] <- paste("=", sets$name[rhs_idx])
    }
    if (tolower(sets$qualifier_list[i]) %=% "(intertemporal)" ||
      (!is.na(rhs_idx) &&
        tolower(sets$qualifier_list[rhs_idx]) %=% "(intertemporal)")) {
      eq_statement <- paste("Set", sets$name[i], sets$definition[i])
      .cli_action(model_err$int_set_eq_fail,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  lapply(sets$definition[!is_set_eq & !is_builder], \(entry) {
    if (!is.na(entry)) {
      if (!any(grepl(
        '\\+|\\-|\\^|&|\\*|\\(|\\)|"|union|intersect|\\s[xX]\\s',
        entry,
        ignore.case = TRUE
      ))) {
        if (grepl(pattern = "=", x = entry)) {
          bad_def <- entry
          .cli_action(model_err$invalid_set_def,
            action = "abort",
            call = call
          )
        }
      }
    }
  })
  parsed <- list(sets = sets, is_set_eq = is_set_eq)
  return(parsed)
}
