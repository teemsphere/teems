#' @importFrom data.table fsetdiff
#' @importFrom utils capture.output
#' 
#' @keywords internal
#' @noRd
.capture_dt_tup <- function(template,
                            value,
                            drop = 2) {
  missing_tuples <- data.table::fsetdiff(value, template)
  n_missing_tuples <- nrow(missing_tuples)
  missing_tuples <- utils::capture.output(print(missing_tuples))
  missing_tuples <- missing_tuples[-seq_len(drop)]
  cli_info <- list(
    missing = missing_tuples,
    n = n_missing_tuples
  )
  return(cli_info)
}