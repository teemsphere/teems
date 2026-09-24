#' @keywords internal
#' @noRd
.expand_tab_defaults <- function(statements,
                                 call) {
  no_label <- gsub("#[^#]*#", "", statements)
  is_def <- grepl("^\\s*[A-Za-z_]+\\s*\\(\\s*default\\b", no_label, ignore.case = TRUE)
  if (!any(is_def)) {
    return(statements)
  }
  .chk_tab_defaults(statements[is_def], call = call)
  kw <- sub("^\\s*([A-Za-z_]+).*$", "\\1", statements)
  state <- list()
  for (i in seq_along(statements)) {
    classes <- tab_default_classes[[kw[i]]]
    if (is.null(classes)) {
      next
    }
    if (is_def[i]) {
      val <- sub("^[^(]*\\(\\s*default\\s*=?\\s*([^);]*).*$", "\\1", no_label[i], ignore.case = TRUE)
      val <- tolower(gsub("[[:space:]]", "", val))
      for (cl in seq_along(classes)) {
        if (val %in% classes[[cl]]) {
          state[[paste(kw[i], cl)]] <- val
        }
      }
      next
    }
    toks <- .tab_qualifier_groups(statements[i])$groups
    toks <- tolower(gsub("[[:space:]]", "", unlist(strsplit(toks, ",", fixed = TRUE))))
    add <- character(0)
    for (cl in seq_along(classes)) {
      v <- state[[paste(kw[i], cl)]]
      if (!is.null(v) && !any(toks %in% classes[[cl]])) {
        add <- c(add, v)
      }
    }
    if (length(add) > 0L) {
      statements[i] <- .add_leading_qualifiers(statements[i], add)
    }
  }
  statements <- statements[!is_def]
  return(statements)
}

#' @keywords internal
#' @noRd
.add_leading_qualifiers <- function(statement,
                                    add) {
  kw <- sub("^\\s*([A-Za-z_]+).*$", "\\1", statement)
  rest <- trimws(sub("^\\s*[A-Za-z_]+", "", statement))
  toks <- character(0)
  repeat {
    if (!startsWith(rest, "(") || grepl("^\\(\\s*all[ ,]", rest, ignore.case = TRUE)) {
      break
    }
    close <- .match_bracket(rest, 1L)
    toks <- c(toks, trimws(strsplit(substr(rest, 2L, close - 1L), ",", fixed = TRUE)[[1]]))
    rest <- trimws(substring(rest, close + 1L))
  }
  statement <- paste0(kw, " (", paste(c(toks, add), collapse = ","), ") ", rest)
  return(statement)
}
