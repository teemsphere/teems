#' @keywords internal
#' @noRd
.chk_shk_col <- function(input,
                         type,
                         call) {

  value_colnames <- colnames(input)
  if (!"Value" %in% value_colnames) {
    .cli_action(
      shk_err$cst_scen_val_df,
      action = c("abort", "inform"),
      url = NULL,
      hyperlink = NULL,
      call = call
    )
  }

  if (type %=% "scenario" && !"Year" %in% value_colnames) {
    .cli_action(
      shk_err$scen_year_df,
      action = c("abort", "inform"),
      url = NULL,
      hyperlink = NULL,
      call = call
    )
  }
  return(invisible(NULL))
}