#' @importFrom data.table data.table rbindlist
#' @keywords internal
#' @noRd
.passes_accuracy <- function(output) {
  code_order <- c("XMA", "XRA", "C0", "FC0", "CX", "FCX", "MVC", "MN0", "OVC", "ON0", "MC?", "OC?", "MD?", "OD?", "MD!", "OD!")
  figures <- .o_accuracy_figures()
  is_var <- output$type == "variable"
  rows <- lapply(which(is_var), \(i) {
    d <- output$dat[[i]]
    if (!"Figures" %in% names(d)) {
      return(NULL)
    }
    fig <- ifelse(is.na(d$Figures), 6L, d$Figures)
    code_rank <- match(d$Code, code_order)
    worst <- if (all(is.na(code_rank))) NA_character_ else code_order[max(code_rank, na.rm = TRUE)]
    row <- data.table::data.table(
      Variable = output$name[i],
      Elements = nrow(d),
      Summarised = sum(d$Summarised %in% TRUE),
      MinFigures = if (nrow(d) > 0L) min(fig) else NA_integer_,
      MinLevelsFigures = if (nrow(d) > 0L) min(d$LevelsFigures) else NA_integer_,
      ShareFigures = mean(fig >= figures),
      ShareLevelsFigures = mean(d$LevelsFigures >= figures),
      WorstCode = worst,
      MaxError = if (nrow(d) > 0L) max(d$Error) else NA_real_
    )
    return(row)
  })
  accuracy <- data.table::rbindlist(rows)
  return(accuracy)
}
