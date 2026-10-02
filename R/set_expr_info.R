#' @keywords internal
#' @noRd
.set_expr_info <- function(d) {
  toks <- .set_expr_tokens(d)
  is_op <- toks %in% c("+", "-", "%", "^", "&", "*")
  is_paren <- toks %in% c("(", ")")
  is_quote <- grepl('^"', toks)
  named <- toks[!is_op & !is_paren & !is_quote]
  ops <- toks[is_op]
  depth <- cumsum((toks == "(") - (toks == ")"))
  top_op_idx <- which(is_op & depth == 0)
  last_top_op <- if (length(top_op_idx)) {
    toks[max(top_op_idx)]
  } else {
    NA_character_
  }
  last_term <- NA_character_
  if (length(top_op_idx)) {
    after <- toks[seq(max(top_op_idx) + 1L, length(toks))]
    if (length(after) %=% 1L && !after %in% c("(", ")") && !grepl('^"', after)) {
      last_term <- after
    }
  }
  top_ops <- toks[is_op & depth == 0]
  complement_of <- NA_character_
  if (length(top_ops) > 0L && all(top_ops %in% c("-", "%")) && length(toks) > 0L &&
    !toks[1] %in% c("(", ")") && !grepl('^"', toks[1])) {
    complement_of <- toks[1]
  }
  info <- list(
    named = named,
    ops = ops,
    all_plus_union = length(ops) > 0 && all(ops %in% c("+", "^")),
    all_intersect = length(ops) > 0 && all(ops %=% "&"),
    simple_complement = length(ops) %=% 1L && ops[1] %=% "-" &&
      length(named) %=% 2L && !any(is_paren) && !any(is_quote),
    complement_of = complement_of,
    last_top_op = last_top_op,
    last_term = last_term
  )
  return(info)
}
