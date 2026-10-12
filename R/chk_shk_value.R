#' @importFrom utils head
#' @keywords internal
#' @noRd
.chk_shk_value <- function(input,
                           call) {
  bad <- which(!is.finite(input$Value))
  if (length(bad) > 0L) {
    n_bad <- length(bad)
    bad_rows <- utils::head(bad, 10L)
    .cli_action(shk_err$value_finite,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
