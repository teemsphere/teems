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
        args <- c(args, paste(current, collapse = ""))
        return(args)
      }
    } else if (tok %=% "," && depth == 1L) {
      args <- c(args, paste(current, collapse = ""))
      current <- character()
      next
    }
    current <- c(current, tok)
  }
}
