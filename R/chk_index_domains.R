#' @keywords internal
#' @noRd
.strip_tab_labels <- function(text) {
  stripped <- gsub("#[^#]*#", " ", text)
  return(stripped)
}

#' @keywords internal
#' @noRd
.check_index_domains <- function(extract,
                                 set_extract,
                                 call) {
  if (is.null(extract) || nrow(extract) == 0L || is.null(extract$remainder)) {
    return(invisible(NULL))
  }
  decl <- .declared_arg_sets(extract)
  if (length(decl) == 0L) {
    return(invisible(NULL))
  }
  closure <- .set_superset_closure(set_extract)
  set_case <- set_extract$name
  names(set_case) <- tolower(set_extract$name)

  stmt_rows <- which(tolower(extract$type) %in% c("equation", "formula", "update", "assertion"))
  ref_pattern <- "\\b([A-Za-z_][A-Za-z0-9_@]*)\\s*\\(([^()]*)\\)"

  for (n in stmt_rows) {
    text <- .strip_tab_labels(extract$remainder[[n]])
    idx_sets <- .stmt_index_sets(text)
    if (length(idx_sets) == 0L) {
      next
    }
    refs <- regmatches(text, gregexpr(ref_pattern, text, perl = TRUE))[[1]]
    for (ref in refs) {
      nm <- tolower(sub("\\s*\\(.*$", "", ref))
      d <- decl[[nm]]
      if (is.null(d)) {
        next
      }
      args <- trimws(strsplit(sub("^[^(]*\\((.*)\\)$", "\\1", ref), ",")[[1]])
      if (length(args) != length(d)) {
        next
      }
      for (k in seq_along(args)) {
        a <- tolower(args[[k]])
        if (!grepl("^[a-z_][a-z0-9_@]*$", a) || is.na(idx_sets[a])) {
          next
        }
        s <- idx_sets[[a]]
        if (tolower(s) == tolower(d[[k]])) {
          next
        }
        if (tolower(s) %in% closure[[tolower(d[[k]])]]) {
          next
        }
        bad_idx <- args[[k]]
        bad_ref <- gsub("\\s+", "", ref)
        stmt_name <- if (tolower(extract$type[[n]]) %=% "equation") {
          regmatches(text, regexpr("[A-Za-z_][A-Za-z0-9_@]*", text))
        } else {
          NULL
        }
        bad_stmt <- paste(c(extract$type[[n]], stmt_name), collapse = " ")
        bad_set <- if (!is.na(set_case[tolower(s)])) {
          set_case[[tolower(s)]]
        } else {
          s
        }
        decl_set <- if (!is.na(set_case[tolower(d[[k]])])) {
          set_case[[tolower(d[[k]])]]
        } else {
          d[[k]]
        }
        .cli_action(model_err$index_not_subset,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  return(invisible(NULL))
}
