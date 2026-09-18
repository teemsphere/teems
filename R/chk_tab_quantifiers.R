#' Quantifier, dimension, sum-index and Zerodivide-default shape checks
#' (fuzz batch 13: the solver's equation_order_read/linvar_dim_read
#' faulted on (all,i) or (all,SET), variables_read wrote past
#' MAXVARDIM, sum_dedup_indices spun on an empty sum index, and
#' formula_subst_scalar returned 0 for an unknown Zerodivide name)
#'
#' @keywords internal
#' @noRd
.chk_tab_quantifiers <- function(statements,
                                 call) {
  no_label <- gsub("#[^#]*#", "", statements)
  kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", no_label))
  max_dims <- 10L
  decl <- kw %in% c("variable", "coefficient")
  quant <- decl | kw %in% c("equation", "formula", "update", "assertion", "write", "display")
  for (i in which(quant)) {
    groups <- regmatches(
      no_label[i],
      gregexpr("\\(\\s*all\\s*,[^)]*\\)?", no_label[i], ignore.case = TRUE)
    )[[1]]
    bad_stmt <- trimws(statements[i])
    for (g in groups) {
      inner <- sub("^\\(\\s*all\\s*,", "", sub("\\)$", "", g), ignore.case = TRUE)
      toks <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
      set_head <- if (length(toks) >= 2L) {
        sub(":.*$", "", toks[2])
      } else {
        ""
      }
      if (length(toks) < 2L || !nzchar(toks[1]) || !nzchar(trimws(set_head))) {
        bad_group <- trimws(g)
        .cli_action(model_err$quantifier_malformed,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
    if (decl[i] && length(groups) > max_dims) {
      n_dims <- length(groups)
      .cli_action(model_err$dims_too_many,
        action = "abort",
        call = call
      )
    }
  }
  # sum(<index>,<set>...) / sum{<index>,<set>...}: the index must be there
  sums <- regmatches(no_label, gregexpr("\\bsum\\s*[({]\\s*[^,({}]*,", no_label, ignore.case = TRUE))
  for (i in seq_along(sums)) {
    for (h in sums[[i]]) {
      index <- trimws(sub(",$", "", sub("^sum\\s*[({]\\s*", "", h, ignore.case = TRUE)))
      if (!nzchar(index)) {
        bad_stmt <- trimws(statements[i])
        .cli_action(model_err$sum_index_empty,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  # Zerodivide (...) default <value>: a number or a declared coefficient
  # (the solver's scalar lookup silently yields 0 for an unknown name)
  coef_rows <- which(kw == "coefficient")
  coef_names <- vapply(coef_rows, \(i) {
    x <- sub("^\\s*coefficient\\s*", "", no_label[i], ignore.case = TRUE)
    repeat {
      y <- sub("^\\s*\\([^)]*\\)", "", x)
      if (identical(y, x)) {
        break
      }
      x <- y
    }
    toupper(sub("^\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", x))
  }, character(1))
  for (i in which(kw == "zerodivide")) {
    if (!grepl("\\bdefault\\b", no_label[i], ignore.case = TRUE)) {
      next
    }
    bad_val <- trimws(sub("^.*\\bdefault\\s+([^;]*).*$", "\\1", no_label[i], ignore.case = TRUE))
    is_num <- !is.na(suppressWarnings(as.numeric(bad_val)))
    if (!is_num && !(toupper(bad_val) %in% coef_names)) {
      bad_stmt <- trimws(statements[i])
      .cli_action(model_err$zerodivide_unknown,
        action = "abort",
        call = call
      )
    }
  }
  return(invisible(NULL))
}
