# S1/S2: expression operands must be declared sets (quoted single
# elements aside) and never the set being defined; a spelling that
# differs only by case is canonicalized to the declared form so the
# downstream exact matches (implied subsets, .eval_set_expr) hold
#' @keywords internal
#' @noRd
.chk_set_expr_refs <- function(sets,
                               is_expr,
                               is_set_eq,
                               call) {
  for (i in which(is_expr & !is_set_eq)) {
    toks <- .set_expr_tokens(sets$definition[[i]])
    named <- toks[!toks %in% c("+", "-", "^", "&", "*", "(", ")") &
      !grepl('^"', toks)]
    bad_refs <- character(0)
    for (tk in unique(named)) {
      if (tolower(tk) %=% tolower(sets$name[i])) {
        bad_set <- sets$name[i]
        bad_def <- sets$definition[[i]]
        .cli_action(model_err$set_self_ref,
          action = c("abort", "inform"),
          call = call
        )
      }
      if (tk %in% sets$name) {
        next
      }
      ci <- match(tolower(tk), tolower(sets$name))
      if (is.na(ci)) {
        bad_refs <- c(bad_refs, tk)
      } else {
        sets$definition[[i]] <- gsub(
          paste0("\\b", tk, "\\b"),
          sets$name[ci],
          sets$definition[[i]]
        )
      }
    }
    if (length(bad_refs) > 0L) {
      bad_stmt <- paste("Set", sets$name[i], "=", sets$definition[[i]])
      .cli_action(model_err$set_undeclared,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  return(sets)
}
