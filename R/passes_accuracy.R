#' @importFrom data.table data.table rbindlist
#' @keywords internal
#' @noRd
.passes_accuracy <- function(output) {
  code_order <- c("XMA", "XRA", "C0", "FC0", "CX", "FCX", "MVC", "MN0", "OVC", "ON0", "MC?", "OC?", "MD?", "OD?", "MD!", "OD!")
  figures <- .o_accuracy_figures()
  is_var <- output$type == "variable"
  rows <- lapply(which(is_var), \(i) {
    d <- output$dat[[i]]
    if (!"figures" %in% names(d)) {
      return(NULL)
    }
    fig <- ifelse(is.na(d$figures), 6L, d$figures)
    code_rank <- match(d$code, code_order)
    worst <- if (all(is.na(code_rank))) NA_character_ else code_order[max(code_rank, na.rm = TRUE)]
    row <- data.table::data.table(
      Variable = output$name[i],
      elements = nrow(d),
      summarized = sum(d$summarized %in% TRUE),
      min_figures = if (nrow(d) > 0L) min(fig) else NA_integer_,
      min_levels_figures = if (nrow(d) > 0L) min(d$levels_figures) else NA_integer_,
      share_figures = mean(fig >= figures),
      share_levels_figures = mean(d$levels_figures >= figures),
      worst_code = worst,
      max_error = if (nrow(d) > 0L) max(d$error) else NA_real_
    )
    return(row)
  })
  accuracy <- data.table::rbindlist(rows)
  return(accuracy)
}
