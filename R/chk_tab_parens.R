#' @keywords internal
#' @noRd
# Parenthesis balance over the whole statement (labels and quoted text
# removed): an unclosed sum( sent the solver's span cutter off the end
# of the statement buffer (fuzz batch 13; now a named solver abort), and
# every other unbalanced statement mis-parses downstream.
.chk_tab_parens <- function(statements,
                            call) {
  txt <- gsub("\"[^\"]*\"", "", gsub("#[^#]*#", "", statements))
  opens <- lengths(regmatches(txt, gregexpr("(", txt, fixed = TRUE)))
  closes <- lengths(regmatches(txt, gregexpr(")", txt, fixed = TRUE)))
  bad <- opens != closes
  if (any(bad)) {
    bad_stmt <- trimws(statements[bad][1])
    if (nchar(bad_stmt) > 60L) {
      bad_stmt <- paste0(substr(bad_stmt, 1, 60), "...")
    }
    .cli_action(model_err$stmt_unbalanced,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
