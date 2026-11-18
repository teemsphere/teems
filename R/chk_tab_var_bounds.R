#' @keywords internal
#' @noRd
.chk_tab_var_bounds <- function(model,
                                call) {
  quals <- model$qualifier_list
  is_var <- tolower(model$type) == "variable" & !is.na(quals)
  has_bound <- grepl("(^|[(,])\\s*(ge|gt|le|lt)\\s*[-+0-9.]", quals, ignore.case = TRUE)
  is_levels <- grepl("\\blevels\\b", quals, ignore.case = TRUE)
  bad <- is_var & has_bound & !is_levels
  if (any(bad)) {
    bad_names <- unique(model$name[bad])
    .cli_action(model_err$bound_linear_var,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
