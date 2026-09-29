#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @noRd
.parse_solution_cols <- function(sol_prefix) {
  if (!.solver_output_listed(sol_prefix, "cols") ||
    !.solver_output_listed(sol_prefix, "cols.json")) {
    return(NULL)
  }
  kinds <- c("mixed", "subtotal", "sagem_individual", "approx_cumulative", "pass_solution")
  con <- file(paste0(sol_prefix, "cols"), "rb")
  on.exit(close(con))
  hdr <- readBin(con, "integer", n = 4L, size = 8L, endian = "little")
  if (length(hdr) != 4L || !identical(hdr[1], 1L) || hdr[4] < 0L || hdr[4] >= length(kinds)) {
    return(NULL)
  }
  ncol <- hdr[2]
  nrow <- hdr[3]
  rows <- readBin(con, "integer", n = nrow, size = 8L, endian = "little")
  values <- readBin(con, "double", n = ncol * nrow, size = 8L, endian = "little")
  index <- jsonlite::fromJSON(paste0(sol_prefix, "cols.json"), simplifyVector = TRUE)
  if (length(rows) != nrow || length(values) != ncol * nrow ||
    !identical(as.integer(index$ncol), ncol) || !identical(as.integer(index$nrow), nrow)) {
    return(NULL)
  }
  cols <- list(
    kind = kinds[hdr[4] + 1L],
    rows = rows,
    columns = as.data.frame(index$columns, stringsAsFactors = FALSE),
    values = matrix(values, nrow = nrow, ncol = ncol)
  )
  return(cols)
}
