#' Raw-statement checks that must run before any object parsing
#'
#' Formula & Equation, Default statements, malformed qualifier lists,
#' and headerless/terminal Reads crash or corrupt the downstream
#' extract parsers, so they are diagnosed on the cleaned statement
#' vector straight after .check_statements().
#'
#' @keywords internal
#' @noRd
.chk_raw_statements <- function(statements,
                                call) {
  # Formula&Equation is expanded into its two 10.9.1 halves by
  # .check_statements before this runs (C0 levels support)
  ps_begin <- sum(grepl("^\\s*postsim\\s*\\(\\s*begin", statements, ignore.case = TRUE))
  ps_end <- sum(grepl("^\\s*postsim\\s*\\(\\s*end", statements, ignore.case = TRUE))
  if (ps_begin != ps_end) {
    .cli_action(model_err$postsim_unbalanced,
      action = "abort",
      call = call
    )
  }
  # Formula/Equation/Update need an "=": a statement whose leading
  # token is no recognized keyword is folded into the preceding
  # statement as an implicit continuation (.check_statements), so an
  # unknown keyword surfaces here as a math statement without "=" --
  # the raw form crashes the downstream extract parsers
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
  .chk_tab_defaults(statements, call = call)
  .chk_tab_qualifiers(statements, call = call)
  .chk_tab_quantifiers(statements, call = call)
  .chk_tab_ref_indices(statements, call = call)
  .chk_tab_stmt_length(statements, call = call)
  .chk_tab_parens(statements, call = call)
  .chk_raw_reads(statements, call = call)
  return(invisible(NULL))
}
