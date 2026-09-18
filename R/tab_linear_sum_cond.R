# A sum's condition segment: "" when the sum is unconditional, else
# ": <text>" up to the comma that opens the summand. Depth-tracked, so
# commas inside the condition's own references stay put.
#' @keywords internal
#' @noRd
.pe_sum_cond <- function(st) {
  if (!identical(.pk(st), ":")) {
    return("")
  }
  .adv(st)
  parts <- character()
  depth <- 0L
  repeat {
    tok <- .pk(st)
    if (is.na(tok)) {
      stop("unterminated sum condition", call. = FALSE)
    }
    if (tok %in% c("(", "[", "{")) {
      depth <- depth + 1L
    } else if (tok %in% c(")", "]", "}")) {
      if (depth == 0L) {
        stop("unterminated sum condition", call. = FALSE)
      }
      depth <- depth - 1L
    } else if (tok %=% "," && depth == 0L) {
      break
    }
    parts <- c(parts, .adv(st))
  }
  if (length(parts) == 0L) {
    stop("empty sum condition", call. = FALSE)
  }
  cond <- paste0(": ", paste(parts, collapse = ""))
  return(cond)
}
