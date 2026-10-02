#' @keywords internal
#' @noRd
.chk_tab_qualifiers <- function(statements,
                                call) {
  kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", statements))
  rows <- which(kw %in% c("variable", "coefficient"))
  bad_quals <- character(0)
  for (i in rows) {
    is_variable <- kw[i] == "variable"
    vocab <- if (is_variable) {
      tab_var_qualifiers
    } else {
      tab_coef_qualifiers
    }
    prefixes <- if (is_variable) {
      tab_var_qualifier_prefixes
    } else {
      character(0)
    }
    parsed <- .tab_qualifier_groups(statements[i])
    bad_stmt <- trimws(statements[i])
    if (parsed$unbalanced) {
      .cli_action(model_err$qual_unbalanced,
        action = "abort",
        call = call
      )
    }
    n_lower <- 0L
    n_upper <- 0L
    for (g in parsed$groups) {
      toks <- tolower(gsub("[[:space:]]", "", strsplit(g, ",", fixed = TRUE)[[1]]))
      if (length(toks) == 0L) {
        toks <- ""
      }
      if (any(grepl("^default", toks))) {
        next
      }
      for (tok in toks) {
        if (!nzchar(tok)) {
          .cli_action(model_err$qual_empty,
            action = "abort",
            call = call
          )
        }
        if (is_variable && tok == "no_split") {
          .cli_action(model_err$qual_no_split,
            action = "abort",
            call = call
          )
        }
        if (grepl("^(ge|gt|le|lt)[-+0-9.]", tok)) {
          if (grepl("^g", tok)) {
            n_lower <- n_lower + 1L
          } else {
            n_upper <- n_upper + 1L
          }
          next
        }
        if (tok %in% vocab) {
          next
        }
        if (length(prefixes) > 0L &&
          any(startsWith(tok, prefixes))) {
          next
        }
        bad_quals <- c(bad_quals, tok)
      }
    }
    if (n_lower > 1L || n_upper > 1L) {
      bound_dir <- if (n_lower > 1L) {
        "lower"
      } else {
        "upper"
      }
      .cli_action(model_err$bound_dup,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  if (length(bad_quals) > 0L) {
    bad_quals <- unique(bad_quals)
    .cli_action(model_err$qual_unknown,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
