#' @keywords internal
#' @noRd
.pos <- function(side, lift) {
  if (side$kind %=% "elem") {
    pos_expr <- sprintf("$POS(%s,%s)", side$text, lift)
    return(pos_expr)
  }
  if (toupper(lift) %=% toupper(side$set)) {
    pos_expr <- sprintf("$POS(%s)", side$text)
    return(pos_expr)
  }
  pos_expr <- sprintf("$POS(%s,%s)", side$text, lift)
  return(pos_expr)
}

#' @keywords internal
#' @noRd
.reject <- function(if_reason, if_cond, call) {
  msg <- .cli_action(model_err$invalid_if_index_cond,
    action = c("abort", "inform"),
    call = call
  )
  return(msg)
}

#' @keywords internal
#' @noRd
.classify <- function(x, idx_set, maps) {
  x <- trimws(x)
  if (grepl('^"[^"]+"$', x)) {
    side <- list(kind = "elem", text = x)
    return(side)
  }
  if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", x) && toupper(x) %in% names(idx_set)) {
    side <- list(kind = "index", text = x, set = idx_set[[toupper(x)]])
    return(side)
  }
  m <- regmatches(x, regexec(
    "^([A-Za-z_][A-Za-z0-9_]*)\\s*\\(\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)$", x
  ))[[1]]
  if (length(m) > 0L && toupper(m[2]) %in% names(maps) &&
    toupper(m[3]) %in% names(idx_set)) {
    side <- list(kind = "map", text = x, set = maps[[toupper(m[2])]][2])
    return(side)
  }
  side <- list(kind = "expr", text = x)
  return(side)
}

#' @importFrom stats setNames
#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.if_index_cond <- function(cond_info,
                           quant,
                           q_idx,
                           synth,
                           if_cond,
                           call) {
  if (!cond_info$kind %in% c("expr", "cmp")) {
    return(cond_info)
  }
  lhs <- if (cond_info$kind %=% "cmp") {
    cond_info$ref
  } else {
    cond_info$lhs
  }
  rhs <- if (cond_info$kind %=% "cmp") {
    cond_info$num
  } else {
    cond_info$rhs
  }
  live <- !is.na(q_idx)
  idx_set <- stats::setNames(
    purrr::map_chr(quant[live], "set"),
    toupper(q_idx[live])
  )
  maps <- .tab_mappings(synth$tab)
  a <- .classify(lhs, idx_set, maps)
  b <- .classify(rhs, idx_set, maps)
  if (a$kind %=% "expr" && b$kind %=% "expr") {
    return(cond_info)
  }
  if (a$kind %=% "expr" || b$kind %=% "expr") {
    .reject(model_err$if_index_reason$mixed_operands, if_cond, call)
  }
  if (a$kind %=% "elem" && b$kind %=% "elem") {
    .reject(model_err$if_index_reason$both_elements, if_cond, call)
  }
  set_a <- if (a$kind %=% "elem") {
    b$set
  } else {
    a$set
  }
  set_b <- if (b$kind %=% "elem") {
    a$set
  } else {
    b$set
  }
  if (toupper(set_a) %=% toupper(set_b)) {
    common <- set_a
  } else if (.tab_is_subset(set_a, set_b, synth)) {
    common <- set_b
  } else if (.tab_is_subset(set_b, set_a, synth)) {
    common <- set_a
  } else {
    .reject(
      sprintf(model_err$if_index_reason$unrelated_sets, set_a, set_b),
      if_cond, call
    )
  }
  if (!cond_info$op %in% c("=", "<>") &&
    !toupper(common) %in% .tab_intertemporal_sets(synth$tab)) {
    .reject(
      sprintf(model_err$if_index_reason$not_intertemporal, common),
      if_cond, call
    )
  }
  cond <- list(kind = "expr", lhs = .pos(a, common), op = cond_info$op, rhs = .pos(b, common))
  return(cond)
}
