#' @keywords internal
#' @noRd
.pe_args <- function(st) {
  args <- character()
  current <- character()
  depth <- 1L
  repeat {
    tok <- .adv(st)
    if (is.na(tok)) {
      stop(model_err$linear_reason$unbalanced, call. = FALSE)
    }
    if (tok %in% c("(", "[", "{")) {
      depth <- depth + 1L
    } else if (tok %in% c(")", "]", "}")) {
      depth <- depth - 1L
      if (depth == 0L) {
        args <- c(args, .join_tokens(current))
        return(args)
      }
    } else if (tok %=% "," && depth == 1L) {
      args <- c(args, .join_tokens(current))
      current <- character()
      next
    }
    current <- c(current, tok)
  }
}

#' @keywords internal
#' @noRd
.join_tokens <- function(tokens) {
  if (length(tokens) < 2L) {
    joined <- paste(tokens, collapse = "")
    return(joined)
  }
  word <- "[A-Za-z0-9_@.\"]"
  gap <- grepl(paste0(word, "$"), tokens[-length(tokens)]) & grepl(paste0("^", word), tokens[-1])
  joined <- paste0(tokens[1], paste0(ifelse(gap, " ", ""), tokens[-1], collapse = ""))
  return(joined)
}

