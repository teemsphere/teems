#' @keywords internal
#' @noRd
.resolve_backsolves <- function(intab_backsolve,
                                backsolve,
                                var_extract,
                                eq_names,
                                quiet = FALSE,
                                call) {
  pairs <- list()

  arg_actions <- list()
  if (!is.null(backsolve)) {
    arg_names <- names(backsolve) %|||% rep("", length(backsolve))
    for (b in seq_along(backsolve)) {
      if (nzchar(arg_names[[b]])) {
        arg_actions[[length(arg_actions) + 1L]] <- list(
          var = arg_names[[b]],
          eq = backsolve[[b]]
        )
      } else {
        arg_actions[[length(arg_actions) + 1L]] <- list(
          var = backsolve[[b]],
          eq = NA_character_
        )
      }
    }
  }

  for (action in c(intab_backsolve, arg_actions)) {
    bs_var <- .canonical_vars(
      input = action$var,
      var_extract = var_extract,
      err = model_err$invalid_backsolve_var,
      call = call
    )

    if (is.na(action$eq)) {
      conv_eq <- paste0("E_", bs_var)
      e_idx <- match(tolower(conv_eq), tolower(eq_names))
      if (is.na(e_idx)) {
        .cli_action(model_err$backsolve_unresolvable,
          action = c("abort", "inform", "inform"),
          call = call
        )
      }
    } else {
      e_idx <- match(tolower(action$eq), tolower(eq_names))
      if (is.na(e_idx)) {
        parts <- eq_names[grepl(
          paste0("^", action$eq, "[A-Z]+$"), eq_names,
          ignore.case = TRUE
        )]
        if (length(parts) > 0L) {
          if (!quiet) {
            skip_var <- bs_var
            skip_eq <- action$eq
            skip_parts <- parts
            .cli_action(model_info$backsolve_partitioned,
              action = c("inform", "inform"),
              call = call
            )
          }
          next
        }
        invalid_eq <- action$eq
        .cli_action(model_err$invalid_backsolve_eq,
          action = "abort",
          call = call
        )
      }
    }

    pairs[[length(pairs) + 1L]] <- list(var = bs_var, eq = eq_names[[e_idx]])
  }

  return(pairs)
}