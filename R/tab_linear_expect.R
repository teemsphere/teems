#' @keywords internal
#' @noRd
.expect <- function(st, tok) {
  got <- .adv(st)
  if (is.na(got) || got != tok) {
    stop(
      sprintf(
        model_err$linear_reason$expected, tok,
        ifelse(is.na(got), "end of expression", got)
      ),
      call. = FALSE
    )
  }
  return(invisible(got))
}
