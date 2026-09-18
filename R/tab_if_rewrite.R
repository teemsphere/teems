#' GEMPACK IF in formulas (manual 11.4.5-11.4.7). An IF term entering a
#' Formula's right-hand side additively at the top level is rewritten
#' into sequential formulas the solver executes natively: a base
#' formula with the IF terms dropped, then one accumulate formula per
#' IF term whose domain is narrowed to where the condition holds, so
#' the value expression is only ever evaluated there (matching GEMPACK,
#' which does not evaluate the expression when the condition fails).
#' Conditions follow the manual's simple shapes:
#'   <index> in <set>     the index's quantifier is narrowed to
#'                        <set> & <range> (rule 4: <set> need not be a
#'                        subset of the current range)
#'   <index> = "element"  the quantifier is narrowed to a synthesized
#'                        singleton set "element" & <range>
#'   <coefref> <op> <c>   a conditional quantifier ':' is appended to
#'                        the statement's last quantifier
#'   <expr> <op> <expr>   a helper coefficient IFX<n> = <expr> [- <expr>]
#'                        is synthesized ahead of the statement and the
#'                        condition takes the <coefref> route above
#' An IF whose value itself carries top-level IF terms is rewritten
#' recursively on the narrowed statement (the GTAP-AEZ shapes: a
#' membership IF wrapping comparison IFs in a Formula, membership IFs
#' on a second index inside a membership branch of an Equation). A
#' membership set that is the quantifier's set or a declared subset
#' of it narrows to that set directly; otherwise an intersection set
#' is synthesized. A Formula whose right-hand side references its own
#' target (the AEZ calibration `ESUBVA = IF[.., ESUBVA] + ..`) reads
#' the pre-assignment values through a synthesized copy coefficient,
#' so the sequential rewrite never destroys what it still needs.
#' Compound conditions (AND/OR/NOT) and non-additive IF placement
#' abort.
#'
#' @keywords internal
#' @noRd
.rewrite_tab_if <- function(tab,
                            call) {
  if_pattern <- "(^|[^A-Za-z0-9_])[Ii][Ff]\\s*[][({]"
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

#' Names of the sets declared `Set (intertemporal)`, upper-cased.
#'
#' @keywords internal
#' @noRd
.tab_intertemporal_sets <- function(tab) {
  stmts <- tab[grepl("^\\s*[Ss][Ee][Tt]\\s*\\(\\s*[Ii][Nn][Tt][Ee][Rr][Tt][Ee][Mm][Pp][Oo][Rr][Aa][Ll]\\s*\\)", tab)]
  sets <- toupper(sub("^\\s*[Ss][Ee][Tt]\\s*\\([^)]*\\)\\s*([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", stmts))
  return(sets)
}

#' Spread a signed IF value over its top-level terms: "+ a - b" for
#' sign "+" and value "a - b", the signs flipped for "-".
#'
#' @keywords internal
#' @noRd
.distribute_terms <- function(sign, value) {
  vt <- .split_tab_terms(value)
  distributed <- paste(ifelse(vt$sign == sign, "+", "-"), vt$body, collapse = " ")
  return(distributed)
}

#' Does `text` reference symbol `sym` (identifier-bounded, any case)?
#'
#' @keywords internal
#' @noRd
.tab_mentions <- function(text, sym) {
  hit <- grepl(paste0("(?<![A-Za-z0-9_])", sym, "(?![A-Za-z0-9_])"), text, ignore.case = TRUE, perl = TRUE)
  return(hit)
}

#' Replace every identifier-bounded occurrence of `sym` in `text`.
#'
#' @keywords internal
#' @noRd
.tab_subst_symbol <- function(text, sym, new) {
  substituted <- gsub(paste0("(?<![A-Za-z0-9_])", sym, "(?![A-Za-z0-9_])"), new, text, ignore.case = TRUE, perl = TRUE)
  return(substituted)
}
