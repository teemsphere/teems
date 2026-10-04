#' @keywords internal
#' @noRd
.rewrite_sum_conditions <- function(tab,
                                    call) {
  has_cond <- grepl("(?<![A-Za-z0-9_@])sum\\s*[\\[({][^:]*:", tab, ignore.case = TRUE, perl = TRUE)
  is_eq <- grepl("^\\s*equation\\b", tab, ignore.case = TRUE) & has_cond & !.eq_is_levels(tab)
  is_fml <- grepl("^\\s*formula\\b", tab, ignore.case = TRUE) & has_cond
  if (!any(is_eq | is_fml)) {
    return(tab)
  }
  synth <- new.env(parent = emptyenv())
  synth$tab <- tab
  synth$maps <- .tab_mappings(tab)
  out <- as.list(tab)
  for (s in which(is_eq | is_fml)) {
    synth$native <- is_fml[s]
    synth$pre <- character(0)
    if (is_eq[s]) {
      header <- .parse_eq_header(tab[s])
      scope <- .quant_scope(header$groups)
      body <- .sum_cond_walk(header$rest, scope, synth)
      stmt <- paste0(
        "Equation ", header$qual, header$name, " ", header$label,
        paste(header$groups, collapse = ""), " ", body
      )
    } else {
      header <- .parse_formula_header(tab[s])
      scope <- .quant_scope(header$groups)
      body <- .sum_cond_walk(header$rest, scope, synth)
      stmt <- paste0("Formula ", paste(header$groups, collapse = ""), " ", body)
    }
    if (length(synth$pre) %=% 0L) {
      next
    }
    out[[s]] <- c(synth$pre, gsub("\\s{2,}", " ", stmt))
    synth$tab <- unlist(out, use.names = FALSE)
  }
  tab <- unlist(out, use.names = FALSE)
  return(tab)
}

#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.quant_scope <- function(groups) {
  m <- regmatches(groups, regexec(
    "^\\(\\s*all\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)",
    groups,
    ignore.case = TRUE
  ))
  m <- m[lengths(m) > 0L]
  scope <- stats::setNames(
    vapply(m, `[`, character(1), 3L),
    vapply(m, `[`, character(1), 2L)
  )
  return(scope)
}

#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.sum_cond_walk <- function(text,
                           scope,
                           synth) {
  out <- ""
  pos <- 1L
  repeat {
    rest <- substring(text, pos)
    m <- regexpr("(?<![A-Za-z0-9_@])sum\\s*[\\[({]", rest, ignore.case = TRUE, perl = TRUE)
    if (m < 0L) {
      break
    }
    open <- pos + m + attr(m, "match.length") - 2L
    close <- .match_bracket(text, open)
    if (is.na(close)) {
      break
    }
    inner <- substr(text, open + 1L, close - 1L)
    scan <- .tab_scan(inner)
    commas <- which(scan$chs == "," & scan$depth_before == 0L & !scan$in_quote)
    if (length(commas) < 2L) {
      out <- paste0(out, substr(text, pos, close))
      pos <- close + 1L
      next
    }
    idx <- trimws(substr(inner, 1L, commas[1] - 1L))
    setcond <- substr(inner, commas[1] + 1L, commas[2] - 1L)
    body <- substring(inner, commas[2] + 1L)
    colon <- regexpr(":", setcond, fixed = TRUE)
    set <- trimws(if (colon > 0L) substr(setcond, 1L, colon - 1L) else setcond)
    cond <- if (colon > 0L) .cond_unwrap(substring(setcond, colon + 1L)) else NA_character_
    inner_scope <- c(scope, stats::setNames(set, idx))
    body <- .sum_cond_walk(body, inner_scope, synth)
    if (!is.na(cond) && !.is_map_cond(cond, idx, synth$maps) &&
      !(isTRUE(synth$native) && .is_native_cond(cond)) &&
      !(length(.cond_logic_ops(cond)) > 0L && .is_index_cond(cond, inner_scope, synth$maps))) {
      ref <- .sum_cond_indicator(cond, inner_scope, synth)
      new_inner <- paste0(idx, ",", set, ", ", ref, "*[", body, "]")
    } else {
      new_inner <- paste0(idx, ",", setcond, ",", body)
    }
    out <- paste0(out, substr(text, pos, open), new_inner, substr(text, close, close))
    pos <- close + 1L
  }
  walked <- paste0(out, substring(text, pos))
  return(walked)
}

#' @keywords internal
#' @noRd
.is_map_cond <- function(cond,
                         idx,
                         maps) {
  m <- regmatches(cond, regexec(
    "^([A-Za-z_][A-Za-z0-9_@]*)\\s*[[(]\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*[])]\\s*=[^=<>]",
    cond
  ))[[1]]
  is_map <- length(m) > 0L && toupper(m[2]) %in% names(maps) &&
    tolower(m[3]) %=% tolower(idx)
  return(is_map)
}

#' @keywords internal
#' @noRd
.sum_cond_indicator <- function(cond,
                                scope,
                                synth) {
  scope <- scope[!duplicated(tolower(names(scope)), fromLast = TRUE)]
  used <- vapply(names(scope), \(ix) {
    grepl(paste0("(^|[^A-Za-z0-9_@\"])", ix, "([^A-Za-z0-9_@\"]|$)"), cond, ignore.case = TRUE)
  }, logical(1))
  dims <- names(scope)[used]
  sets <- unname(scope[used])
  key <- paste0("SUMCOND|", toupper(gsub("\\s", "", cond)), "|", paste(toupper(dims), toupper(sets), collapse = ","))
  nm <- synth[[key]]
  if (is.null(nm)) {
    nm <- .synth_coeff_name(synth)
    synth[[key]] <- nm
    quants <- paste0(sprintf("(all,%s,%s)", dims, sets), collapse = "")
    dimargs <- if (length(dims) > 0L) paste0("(", paste(dims, collapse = ","), ")") else ""
    synth$pre <- c(
      synth$pre,
      sprintf("Coefficient %s %s%s # sum-condition indicator #", quants, nm, dimargs),
      sprintf("Formula %s %s%s = if[%s, 1]", quants, nm, dimargs, cond)
    )
    synth$tab <- c(synth$tab, synth$pre)
  }
  ref <- if (length(dims) > 0L) paste0(nm, "(", paste(dims, collapse = ","), ")") else nm
  return(ref)
}

#' @keywords internal
#' @noRd
.parse_formula_header <- function(stmt) {
  rest <- trimws(sub("^\\s*[Ff][Oo][Rr][Mm][Uu][Ll][Aa]\\s*", "", stmt))
  groups <- character(0)
  repeat {
    rest <- sub("^\\s+", "", rest)
    if (!startsWith(rest, "(")) {
      break
    }
    close <- .match_bracket(rest, 1L)
    if (is.na(close)) {
      break
    }
    groups <- c(groups, substr(rest, 1L, close))
    rest <- substring(rest, close + 1L)
  }
  header <- list(groups = groups, rest = rest)
  return(header)
}

#' @keywords internal
#' @noRd
.is_native_cond <- function(cond) {
  native <- grepl(paste0(
    "^\\s*[A-Za-z_][A-Za-z0-9_@]*\\s*(\\([^()]*\\)|\\[[^][]*\\])?\\s*",
    "(<>|<=|>=|=|<|>|\\s[Ee][Qq]\\s|\\s[Nn][Ee]\\s|\\s[Gg][Tt]\\s|\\s[Ll][Tt]\\s|\\s[Gg][Ee]\\s|\\s[Ll][Ee]\\s)\\s*",
    "[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?\\s*$"
  ), cond)
  return(native)
}

#' @keywords internal
#' @noRd
.eq_is_levels <- function(tab) {
  is_eq <- grepl("^\\s*equation\\b", tab, ignore.case = TRUE)
  head <- tolower(gsub("\\s", "", sub("^\\s*equation\\s*((\\([^)]*\\)\\s*)*).*$", "\\1", tab, ignore.case = TRUE)))
  default <- ifelse(grepl("default=levels", head), "levels", ifelse(grepl("default=linear", head), "linear", NA_character_))
  is_default <- is_eq & !is.na(default)
  state <- cumsum(is_default)
  current <- c("linear", default[is_default])[state + 1L]
  qual_levels <- grepl("[(,]levels[,)]", head)
  qual_linear <- grepl("[(,]linear[,)]", head)
  levels <- is_eq & !is_default & (qual_levels | (!qual_linear & current == "levels"))
  return(levels)
}

