#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
.parse_solution_xac <- function(sol_prefix, var_tbl) {
  if (!.solver_output_listed(sol_prefix, "xac")) {
    return(NULL)
  }
  path <- paste0(sol_prefix, "xac")
  con <- file(path, "rb")
  on.exit(close(con))
  hdr <- readBin(con, "integer", n = 4L, size = 8L, endian = "little")
  if (length(hdr) != 4L || anyNA(hdr) || !identical(hdr[1], 1L) || !identical(hdr[3], 3L) || hdr[2] < 0L) {
    return(NULL)
  }
  nrow <- as.numeric(hdr[2])
  if (!identical(file.size(path), 32 + 3 * 8 * nrow + 4 * nrow)) {
    return(NULL)
  }
  read_rows <- function(offset, size, type, beg, n) {
    seek(con, where = offset + size * beg, origin = "start")
    readBin(con, type, n = n, size = size, endian = "little")
  }
  beg <- as.numeric(var_tbl$begadd)
  n <- var_tbl$matsize
  passes <- lapply(0:2, \(p) {
    unlist(lapply(seq_along(beg), \(i) {
      read_rows(32 + 8 * p * nrow, 8L, "double", beg[i], n[i])
    }))
  })
  accuracy <- unlist(lapply(seq_along(beg), \(i) {
    read_rows(32 + 24 * nrow, 4L, "integer", beg[i], n[i])
  }))
  xac <- data.table::data.table(
    Pass1 = passes[[1]],
    Pass2 = passes[[2]],
    Pass3 = passes[[3]],
    Accuracy = accuracy
  )
  return(xac)
}
