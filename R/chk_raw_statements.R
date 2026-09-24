#' @importFrom tools toTitleCase
#' @keywords internal
#' @noRd
.chk_raw_statements <- function(statements,
                                call) {
  ps_begin <- sum(grepl("^\\s*postsim\\s*\\(\\s*begin", statements, ignore.case = TRUE))
  ps_end <- sum(grepl("^\\s*postsim\\s*\\(\\s*end", statements, ignore.case = TRUE))
  if (ps_begin != ps_end) {
    .cli_action(model_err$postsim_unbalanced,
      action = "abort",
      call = call
    )
  }
  kw_stmt <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", statements))
  math_stmt <- kw_stmt %in% c("formula", "equation", "update")
  no_eq <- math_stmt & !grepl("=", gsub("#[^#]*#", "", statements), fixed = TRUE)
  if (any(no_eq)) {
    bad_stmt <- trimws(statements[no_eq][1])
    stmt_kw <- tools::toTitleCase(kw_stmt[no_eq][1])
    .cli_action(model_err$stmt_missing_equals,
      action = c("abort", "inform"),
      call = call
    )
  }
  .chk_tab_qualifiers(statements, call = call)
  .chk_tab_quantifiers(statements, call = call)
  .chk_tab_ref_indices(statements, call = call)
  .chk_tab_stmt_length(statements, call = call)
  .chk_tab_parens(statements, call = call)
  .chk_raw_reads(statements, call = call)
  return(invisible(NULL))
}
