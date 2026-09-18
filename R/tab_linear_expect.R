#' @keywords internal
#' @noRd
.expect <- function(st, tok) {
  got <- .adv(st)
  if (is.na(got) || got != tok) {
    stop(paste0("expected `", tok, "` but found `",
                ifelse(is.na(got), "end of expression", got), "`"),
         call. = FALSE)
  }
  return(invisible(got))
}
