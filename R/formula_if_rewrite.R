#' @keywords internal
#' @noRd
.narrow <- function(cond_info, if_cond, q_idx, quant, synth, call) {
  at <- match(tolower(cond_info$idx), tolower(q_idx))
  if (is.na(at)) {
    .if_native()
  }
  range_set <- quant[[at]]$set
  operand <- if (cond_info$kind %=% "in_set") {
    cond_info$set
  } else {
    paste0('"', cond_info$elem, '"')
  }
  inter <- .synth_intersect_set(operand, range_set, synth)
  q2 <- quant
  q2[[at]]$text <- sprintf("(all,%s,%s%s)", cond_info$idx, inter$name, quant[[at]]$cond_text)
  narrowed <- list(quant = q2, pre = inter$pre)
  return(narrowed)
}

#' @importFrom purrr map_chr map_lgl
#' @keywords internal
#' @noRd
.rewrite_formula_if <- function(stmt,
                                synth,
                                call,
                                depth = 0L) {
  body <- sub("^\\s*[Ff][Oo][Rr][Mm][Uu][Ll][Aa]\\s*", "", stmt)

  label <- ""
  if (startsWith(trimws(body), "#")) {
    close <- regexpr("#[^#]*#", body)
    label <- paste0(substr(body, close, close + attr(close, "match.length") - 1L), " ")
    body <- substring(body, close + attr(close, "match.length"))
  }

  groups <- character(0)
  repeat {
    body <- sub("^\\s+", "", body)
    if (!startsWith(body, "(")) {
      break
    }
    close <- .match_bracket(body, 1L)
    groups <- c(groups, substr(body, 1L, close))
    body <- substring(body, close + 1L)
  }

  quant <- lapply(groups, \(g) {
    inner <- trimws(substr(g, 2L, nchar(g) - 1L))
    m <- regmatches(inner, regexec(
      "^[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s*(:.*)?$",
      inner
    ))[[1]]
    if (length(m) %=% 0L) {
      rewritten <- list(is_quant = FALSE, text = g)
      return(rewritten)
    }
    list(
      is_quant = TRUE, text = g, idx = m[2], set = m[3],
      cond = nchar(m[4]) > 0L, cond_text = m[4]
    )
  })

  scan <- .tab_scan(body)
  eq_pos <- which(scan$chs == "=" & scan$depth_before == 0L & !scan$in_quote)
  lhs <- trimws(substr(body, 1L, eq_pos[1] - 1L))
  rhs <- trimws(substring(body, eq_pos[1] + 1L))

  q_idx <- purrr::map_chr(quant, \(q) {
    if (isTRUE(q$is_quant)) {
      q$idx
    } else {
      NA_character_
    }
  })
  qual_groups <- purrr::map_chr(quant[!purrr::map_lgl(quant, "is_quant")], "text")
  pre <- character(0)

  if (depth %=% 0L && all(is.na(q_idx))) {
    statements <- .rewrite_scalar_if(label, qual_groups, lhs, rhs, synth, call)
    return(statements)
  }

  lhs_sym <- sub("^\\s*([A-Za-z_][A-Za-z0-9_@]*).*$", "\\1", lhs)
  if (depth %=% 0L && .tab_mentions(rhs, lhs_sym)) {
    cp <- .if_self_copy(lhs, lhs_sym, quant, qual_groups, synth)
    pre <- c(pre, cp$pre)
    rhs <- .tab_subst_symbol(rhs, lhs_sym, cp$name)
  }

  if_pattern <- "(^|[^A-Za-z0-9_@])[Ii][Ff]\\s*[][({]"
  terms <- .distribute_if_factors(.split_tab_terms(rhs), if_pattern)
  parsed <- lapply(terms$body, .parse_if_term)
  is_if <- !purrr::map_lgl(parsed, is.null)

  if (any(grepl(if_pattern, terms$body[!is_if]))) {
    .if_native()
  }

  base_rhs <- paste(
    ifelse(terms$sign[!is_if] == "-", "- ", ""),
    terms$body[!is_if],
    sep = "",
    collapse = " + "
  )
  base_rhs <- gsub("+ - ", "- ", base_rhs, fixed = TRUE)
  if (base_rhs %=% "") {
    base_rhs <- "0"
  }
  header <- paste0(label, paste0(purrr::map_chr(quant, "text"), collapse = ""))
  statements <- if (depth > 0L && base_rhs %=% lhs) {
    character(0)
  } else {
    paste("Formula", header, lhs, "=", base_rhs)
  }

  for (k in which(is_if)) {
    if_cond <- parsed[[k]]$cond
    cond_info <- .classify_if_cond(if_cond)
    if (is.null(cond_info)) {
      .if_native()
    }
    cond_info <- .if_index_cond(cond_info, quant, q_idx, synth, if_cond, call)
    if (cond_info$kind %=% "expr") {
      hx <- .if_expr_helper(cond_info, quant, q_idx, qual_groups, synth, if_cond, stmt, call)
      pre <- c(pre, hx$pre)
      cond_info <- hx$cond_info
    }
    if (cond_info$kind %in% c("in_set", "elem")) {
      if (cond_info$kind %=% "in_set" &&
        toupper(cond_info$set) %in% toupper(q_idx[!is.na(q_idx)])) {
        .if_native()
      }
      narrowed <- .narrow(cond_info, if_cond, q_idx, quant, synth, call)
      pre <- c(pre, narrowed$pre)
      q2 <- narrowed$quant
      header2 <- paste0(label, paste0(purrr::map_chr(q2, "text"), collapse = ""))
    } else {
      free <- !is.na(q_idx) & !purrr::map_lgl(quant, \(q) isTRUE(q$cond))
      last_q <- max(which(free), -Inf)
      if (is.infinite(last_q)) {
        .if_native()
      }
      cond_text <- if (cond_info$kind %=% "compound") {
        cond_info$cond
      } else {
        paste(cond_info$ref, cond_info$op, cond_info$num)
      }
      q2 <- quant
      q2[[last_q]]$text <- sprintf(
        "(all,%s,%s: %s)",
        quant[[last_q]]$idx, quant[[last_q]]$set, cond_text
      )
      header2 <- paste0(label, paste0(purrr::map_chr(q2, "text"), collapse = ""))
    }
    value <- parsed[[k]]$value
    if (grepl(if_pattern, value)) {
      statements <- c(statements, .rewrite_formula_if(
        paste("Formula", header2, lhs, "=", lhs, .distribute_terms(terms$sign[k], value)),
        synth, call,
        depth = depth + 1L
      ))
    } else {
      statements <- c(statements, paste(
        "Formula", header2, lhs, "=",
        lhs, terms$sign[k], paste0("[", value, "]")
      ))
    }
  }

  rewritten <- c(pre, statements)
  return(rewritten)
}
