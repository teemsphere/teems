#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @noRd
.parse_jacobian <- function(sol_prefix) {
  if (!.solver_output_listed(sol_prefix, "jac") ||
    !.solver_output_listed(sol_prefix, "jac.json")) {
    return(NULL)
  }
  con <- file(paste0(sol_prefix, "jac"), "rb")
  on.exit(close(con))
  hdr <- readBin(con, "integer", n = 4L, size = 8L, endian = "little")
  if (length(hdr) != 4L || !identical(hdr[1], 1L) || any(is.na(hdr)) || any(hdr[2:4] < 0L)) {
    return(NULL)
  }
  nnz <- hdr[4]
  rows <- readBin(con, "integer", n = nnz, size = 8L, endian = "little")
  cols <- readBin(con, "integer", n = nnz, size = 8L, endian = "little")
  values <- readBin(con, "double", n = nnz, size = 8L, endian = "little")
  index <- jsonlite::fromJSON(paste0(sol_prefix, "jac.json"), simplifyVector = TRUE)
  if (length(rows) != nnz || length(cols) != nnz || length(values) != nnz ||
    !identical(as.integer(index$nrow), hdr[2]) || !identical(as.integer(index$ncol), hdr[3]) ||
    !identical(as.integer(index$nnz), nnz)) {
    return(NULL)
  }
  equations <- as.data.frame(index$equations[c("name", "first_row", "nrows")], stringsAsFactors = FALSE)
  equations$sets <- index$equations$sets
  jac <- list(
    nrow = hdr[2],
    ncol = hdr[3],
    row = rows,
    col = cols,
    value = values,
    equations = equations
  )
  return(jac)
}
