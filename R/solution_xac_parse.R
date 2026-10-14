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
  if (length(hdr) != 4L || anyNA(hdr) || !hdr[1] %in% c(1L, 2L) || !identical(hdr[3], 3L) || hdr[2] < 0L) {
    return(NULL)
  }
  version <- hdr[1]
  nrow <- as.numeric(hdr[2])
  expected_size <- if (identical(version, 1L)) 32 + 3 * 8 * nrow + 4 * nrow else 32 + 4 * 8 * nrow + 4 * nrow
  if (!identical(file.size(path), expected_size)) {
    return(NULL)
  }
  read_rows <- function(offset, size, type, beg, n) {
    seek(con, where = offset + size * beg, origin = "start")
    readBin(con, type, n = n, size = size, endian = "little")
  }
  beg <- as.numeric(var_tbl$begadd)
  n <- var_tbl$matsize
  read_column <- function(offset, size, type) {
    unlist(lapply(seq_along(beg), \(i) {
      read_rows(offset, size, type, beg[i], n[i])
    }))
  }
  passes <- lapply(0:2, \(p) read_column(32 + 8 * p * nrow, 8L, "double"))
  if (identical(version, 1L)) {
    figures <- read_column(32 + 24 * nrow, 4L, "integer")
    levels_figures <- rep(NA_integer_, length(figures))
    error <- rep(NA_real_, length(figures))
    summarised <- rep(NA, length(figures))
    code_number <- integer(length(figures))
  } else {
    error <- read_column(32 + 24 * nrow, 8L, "double")
    byte_base <- 32 + 32 * nrow
    figures <- read_column(byte_base, 1L, "integer")
    levels_figures <- read_column(byte_base + nrow, 1L, "integer")
    code_number <- read_column(byte_base + 2 * nrow, 1L, "integer")
    flags <- read_column(byte_base + 3 * nrow, 1L, "integer")
    summarised <- bitwAnd(flags, 1L) > 0L
  }
  figures[figures == 7L] <- NA_integer_
  code_names <- c("XMA", "XRA", "C0", "FC0", "CX", "FCX", "MVC", "MN0", "OVC", "ON0", "MC?", "OC?", "MD?", "OD?", "MD!", "OD!")
  code <- ifelse(code_number >= 1L & code_number <= 16L, code_names[pmax(code_number, 1L)], NA_character_)
  xac <- data.table::data.table(
    pass1 = passes[[1]],
    pass2 = passes[[2]],
    pass3 = passes[[3]],
    error = error,
    figures = figures,
    levels_figures = levels_figures,
    code = code,
    summarised = summarised
  )
  return(xac)
}
