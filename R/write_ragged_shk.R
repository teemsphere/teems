#' @importFrom data.table dcast fwrite setorder setorderv .SD
#' @importFrom utils head
#'
#' @keywords internal
#' @noRd
.shk_ragged_write <- function(input,
                              lead,
                              write_path) {

  k <- ncol(input) - 1L

  cat(if (is.null(lead)) attr(input, "lead") else lead,
    file = write_path,
    sep = "\n",
    append = TRUE
  )

  if (k %=% 1L) {
    data.table::setorder(input)
    data.table::fwrite(
      x = input[, "Value", with = FALSE],
      file = write_path,
      quote = FALSE,
      col.names = FALSE,
      append = TRUE,
      sep = " "
    )
    cat(";\n\n", file = write_path, sep = "", append = TRUE)
  } else {
    col_col <- colnames(input)[[k]]
    row_col <- colnames(input)[[k - 1L]]
    blk <- utils::head(colnames(input), k - 2L)

    if (length(blk) %=% 0L) {
      mat <- data.table::dcast(input,
        paste(row_col, "~", col_col),
        value.var = "Value"
      )
      data.table::setorder(mat)
      data.table::fwrite(
        x = mat[, -1L],
        file = write_path,
        quote = FALSE,
        col.names = FALSE,
        append = TRUE,
        sep = " "
      )
      cat(";\n\n", file = write_path, sep = "", append = TRUE)
    } else {
      blk_grid <- unique(input[, .SD, .SDcols = blk])
      data.table::setorderv(blk_grid, blk)
      n_blk <- nrow(blk_grid)

      for (i in seq_len(n_blk)) {
        slice <- input[blk_grid[i], on = blk]
        mat <- data.table::dcast(slice,
          paste(row_col, "~", col_col),
          value.var = "Value"
        )
        data.table::setorder(mat)
        data.table::fwrite(
          x = mat[, -1L],
          file = write_path,
          quote = FALSE,
          col.names = FALSE,
          append = TRUE,
          sep = " "
        )
        if (i == n_blk) {
          cat(";\n\n", file = write_path, sep = "", append = TRUE)
        } else {
          cat("\n", file = write_path, sep = "", append = TRUE)
        }
      }
    }
  }

  return(write_path)
}
