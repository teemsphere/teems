#' @keywords internal
#' @noRd
.rewrite_tab_if <- function(tab,
                            call) {
  if_pattern <- "(^|[^A-Za-z0-9_@])[Ii][Ff]\\s*[][({]"
  has_if <- grepl(if_pattern, tab)
  if (!any(has_if)) {
    return(tab)
  }

  synth <- new.env(parent = emptyenv())
  synth$n <- 0L
  synth$tab <- tab

  out <- vector("list", length(tab))
  for (s in seq_along(tab)) {
    if (!has_if[s]) {
      out[[s]] <- tab[s]
      next
    }
    type <- tolower(sub("\\s.*$", "", tab[s]))
    if (type %=% "formula") {
      out[[s]] <- .rewrite_formula_if(
        stmt = tab[s],
        synth = synth,
        call = call
      )
    } else if (type %=% "equation") {
      out[[s]] <- .rewrite_equation_if(
        stmt = tab[s],
        synth = synth,
        call = call
      )
    } else {
      out[[s]] <- tab[s]
    }
  }
  tab <- unlist(out, use.names = FALSE)
  return(tab)
}

#' @keywords internal
#' @noRd
.tab_intertemporal_sets <- function(tab) {
  stmts <- tab[grepl("^\\s*[Ss][Ee][Tt]\\s*\\(\\s*[Ii][Nn][Tt][Ee][Rr][Tt][Ee][Mm][Pp][Oo][Rr][Aa][Ll]\\s*\\)", tab)]
  sets <- toupper(sub("^\\s*[Ss][Ee][Tt]\\s*\\([^)]*\\)\\s*([A-Za-z_][A-Za-z0-9_@]*).*$", "\\1", stmts))
  return(sets)
}

#' @keywords internal
#' @noRd
.distribute_terms <- function(sign, value) {
  vt <- .split_tab_terms(value)
  distributed <- paste(ifelse(vt$sign == sign, "+", "-"), vt$body, collapse = " ")
  return(distributed)
}

#' @keywords internal
#' @noRd
.tab_mentions <- function(text, sym) {
  hit <- grepl(paste0("(?<![A-Za-z0-9_@])", sym, "(?![A-Za-z0-9_@])"), text, ignore.case = TRUE, perl = TRUE)
  return(hit)
}

#' @keywords internal
#' @noRd
.tab_subst_symbol <- function(text, sym, new) {
  substituted <- gsub(paste0("(?<![A-Za-z0-9_@])", sym, "(?![A-Za-z0-9_@])"), new, text, ignore.case = TRUE, perl = TRUE)
  return(substituted)
}
