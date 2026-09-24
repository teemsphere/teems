#' @keywords internal
#' @noRd
.parse_eq_quants <- function(eq_name,
                             statement,
                             call) {
  bare <- gsub("#[^#]*#", "", statement)
  bare <- gsub("\\s", "", bare)
  m <- gregexpr("\\(all,([[:alnum:]_@]+),([[:alnum:]_@]+)\\)", bare)
  hits <- regmatches(bare, m)[[1]]
  n_all <- length(gregexpr("(all,", bare, fixed = TRUE)[[1]])
  if (identical(gregexpr("(all,", bare, fixed = TRUE)[[1]][1], -1L)) {
    n_all <- 0L
  }
  if (n_all != length(hits)) {
    parse_reason <- model_err$condense_parse_reason$quantifier
    .cli_action(model_err$condense_parse,
      action = c("abort", "inform"),
      call = call
    )
  }
  quants <- lapply(hits, \(h) {
    parts <- strsplit(gsub("[()]", "", h), ",")[[1]]
    list(idx = parts[[2]], set = parts[[3]])
  })
  return(quants)
}
