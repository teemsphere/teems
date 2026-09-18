# pass 1 over both sides: data comparisons become 0/1 indicator
# factors, set-membership conditions are collected for the domain
# split that follows
#' @keywords internal
#' @noRd
.rewrite_if_terms <- function(sides,
                              quant,
                              q_idx,
                              synth,
                              stmt,
                              if_pattern,
                              call) {
  pre <- character(0)
  membership <- list()
  side_terms <- vector("list", 2L)
  for (h in 1:2) {
    terms <- .split_tab_terms(sides[[h]])
    parsed <- lapply(terms$body, .parse_if_term)
    is_if <- !purrr::map_lgl(parsed, is.null)
    if (any(grepl(if_pattern, terms$body[!is_if]))) {
      if_statement <- stmt
      .cli_action(model_err$invalid_if_placement,
        action = c("abort", "inform"),
        call = call
      )
    }
    chunks <- paste(terms$sign, terms$body)
    for (k in which(is_if)) {
      if_cond <- parsed[[k]]$cond
      cond_info <- .classify_if_cond(if_cond)
      if (is.null(cond_info)) {
        .cli_action(model_err$invalid_if_cond,
          action = c("abort", "inform"),
          call = call
        )
      }
      cond_info <- .if_index_cond(cond_info, quant, q_idx, synth, if_cond, call)
      if (cond_info$kind %=% "expr") {
        # an Equation host: the helper is an ordinary (always) Formula
        hx <- .if_expr_helper(cond_info, quant, q_idx, character(0), synth, if_cond, stmt, call)
        pre <- c(pre, hx$pre)
        cond_info <- hx$cond_info
      }
      if (cond_info$kind %=% "cmp") {
        ind <- .if_indicator(cond_info, quant, q_idx, synth, if_cond, call)
        pre <- c(pre, ind$pre)
        vt <- .split_tab_terms(parsed[[k]]$value)
        chunks[k] <- paste(
          ifelse(vt$sign == terms$sign[k], "+", "-"),
          paste0(ind$ref, " * ", vt$body),
          collapse = " "
        )
      } else {
        membership[[length(membership) + 1L]] <- list(
          side = h, term = k, cond_info = cond_info,
          sign = terms$sign[k], value = parsed[[k]]$value,
          if_cond = if_cond
        )
      }
    }
    side_terms[[h]] <- chunks
  }
  rewritten <- list(side_terms = side_terms, pre = pre, membership = membership)
  return(rewritten)
}
