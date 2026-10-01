#' @keywords internal
#' @noRd
.pe_if_cond <- function(st, var_lookup) {
  parts <- character()
  depth <- 0L
  repeat {
    tok <- .pk(st)
    if (is.na(tok)) {
      stop(model_err$linear_reason$unexpected_end, call. = FALSE)
    }
    if (tok %in% c("(", "[", "{")) {
      depth <- depth + 1L
    } else if (tok %in% c(")", "]", "}")) {
      depth <- depth - 1L
    } else if (tok %=% "," && depth == 0L) {
      break
    }
    if (tolower(tok) %in% names(var_lookup)) {
      stop(model_err$linear_reason$if_cond, call. = FALSE)
    }
    parts <- c(parts, .adv(st))
  }
  wordy <- grepl("^[A-Za-z0-9_@.\"]", parts) & grepl("[A-Za-z0-9_@.\"]$", parts)
  glue <- c(wordy[-1] & wordy[-length(wordy)], FALSE)
  cond <- paste0(parts, ifelse(glue, " ", ""), collapse = "")
  return(cond)
}
