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
  synth$names <- unique(toupper(unlist(regmatches(
    tab,
    gregexpr("[A-Za-z_][A-Za-z0-9_@]*", tab, perl = TRUE)
  ))))
  synth$var_names <- .tab_linear_variable_names(tab)

  out <- vector("list", length(tab))
  for (s in seq_along(tab)) {
    if (!has_if[s]) {
      out[[s]] <- tab[s]
      next
    }
    type <- tolower(sub("\\s.*$", "", tab[s]))
    .chk_if_args(tab[s], call)
    .chk_if_in_compound(tab[s], call)
    if (type %=% "equation") {
      .native_if_stmt(tab[s], synth, call)
    }
    snap <- as.list(synth, all.names = TRUE)
    out[[s]] <- tryCatch(
      if (type %=% "equation" && grepl("\\(\\s*levels\\s*\\)", tab[s], ignore.case = TRUE)) {
        .if_native()
      } else if (type %=% "formula") {
        .rewrite_formula_if(stmt = tab[s], synth = synth, call = call)
      } else if (type %=% "equation") {
        .rewrite_equation_if(stmt = tab[s], synth = synth, call = call)
      } else {
        tab[s]
      },
      teems_if_native = \(e) {
        rm(list = ls(synth, all.names = TRUE), envir = synth)
        list2env(snap, envir = synth)
        .native_if_stmt(tab[s], synth, call)
      }
    )
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
