#' @keywords internal
#' @noRd
.if_native <- function() {
  cnd <- structure(
    class = c("teems_if_native", "condition"),
    list(message = "IF left to the solver", call = NULL)
  )
  stop(cnd)
}

#' @keywords internal
#' @noRd
.if_conds <- function(stmt) {
  if_pattern <- "(^|[^A-Za-z0-9_@])[Ii][Ff]\\s*[][({]"
  conds <- character(0)
  rest <- stmt
  repeat {
    m <- regexpr(if_pattern, rest, perl = TRUE)
    if (m < 0L) {
      break
    }
    open <- m + attr(m, "match.length") - 1L
    close <- .match_bracket(rest, open)
    if (is.na(close)) {
      break
    }
    inner <- substr(rest, open + 1L, close - 1L)
    scan <- .tab_scan(inner)
    comma <- which(scan$chs == "," & scan$depth_before == 0L & !scan$in_quote)
    if (length(comma) > 0L) {
      conds <- c(conds, substr(inner, 1L, comma[1] - 1L))
    }
    rest <- substring(rest, open + 1L)
  }
  return(conds)
}

#' @keywords internal
#' @noRd
.chk_if_args <- function(stmt,
                         call) {
  if_pattern <- "(^|[^A-Za-z0-9_@])[Ii][Ff]\\s*[][({]"
  from <- 1L
  repeat {
    rest <- substring(stmt, from)
    m <- regexpr(if_pattern, rest, perl = TRUE)
    if (m < 0L) {
      break
    }
    open <- from + m + attr(m, "match.length") - 2L
    close <- .match_bracket(stmt, open)
    if (is.na(close)) {
      break
    }
    inner <- substr(stmt, open + 1L, close - 1L)
    scan <- .tab_scan(inner)
    if (sum(scan$chs == "," & scan$depth_before == 0L & !scan$in_quote) > 1L) {
      if_term <- substr(stmt, open - 2L, close)
      .cli_action(model_err$if_args,
        action = c("abort", "inform"),
        call = call
      )
    }
    from <- open + 1L
  }
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.chk_if_in_compound <- function(stmt,
                                call) {
  for (if_cond in .if_conds(stmt)) {
    cond <- .cond_unwrap(if_cond)
    if (length(.cond_logic_ops(cond)) %=% 0L) {
      next
    }
    leaves <- .cond_leaves(cond)
    if (any(grepl("^[A-Za-z_][A-Za-z0-9_@]*\\s+[Ii][Nn]\\s+[A-Za-z_][A-Za-z0-9_@]*$", leaves))) {
      if_cond <- trimws(if_cond)
      .cli_action(model_err$if_in_compound,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.native_if_stmt <- function(stmt,
                            synth,
                            call) {
  var_names <- .tab_linear_variable_names(synth$tab)
  for (cond in .if_conds(stmt)) {
    cond <- gsub('"[^"]*"', " ", cond)
    toks <- toupper(unique(regmatches(cond, gregexpr("[A-Za-z_][A-Za-z0-9_@]*", cond))[[1]]))
    if (any(toks %in% var_names)) {
      if_statement <- stmt
      bad_vars <- toks[toks %in% var_names]
      .cli_action(model_err$if_cond_variable,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  return(stmt)
}
