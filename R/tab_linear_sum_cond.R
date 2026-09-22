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
      stop(model_err$linear_reason$sum_cond_unterminated, call. = FALSE)
    }
    if (tok %in% c("(", "[", "{")) {
      depth <- depth + 1L
    } else if (tok %in% c(")", "]", "}")) {
      if (depth == 0L) {
        stop(model_err$linear_reason$sum_cond_unterminated, call. = FALSE)
      }
      depth <- depth - 1L
    } else if (tok %=% "," && depth == 0L) {
      break
    }
    parts <- c(parts, .adv(st))
  }
  if (length(parts) == 0L) {
    stop(model_err$linear_reason$sum_cond_empty, call. = FALSE)
  }
  cond <- paste0(": ", paste(parts, collapse = ""))
  return(cond)
}
