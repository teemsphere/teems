#' @importFrom cli cli_h1 cli_dl cli_fmt
#'
# the counts printed after a model loads
#' @keywords internal
#' @noRd
.model_summary <- function(var_extract,
                           coeff_extract,
                           math_extract,
                           extract,
                           condensed) {
    n_var <- nrow(var_extract)
    n_eq <- nrow(math_extract[math_extract$type %in% "Equation",])
    n_form <- nrow(math_extract[math_extract$type %in% "Formula",])
    n_coeff <- nrow(coeff_extract)
    n_sets <- nrow(extract$set)

    summary_items <- c(
      "Variables" = n_var,
      "Equations" = n_eq,
      "Coefficients" = n_coeff,
      "Formulas" = n_form,
      "Sets" = n_sets
    )
    if (condensed$n_backsolve > 0L) {
      summary_items <- c(
        summary_items,
        "Backsolved" = condensed$n_backsolve
      )
    }

    model_summary <- cli::cli_fmt({
      cli::cli_h1("Model summary:")
      cli::cli_dl(summary_items)
    })
  return(model_summary)
}
