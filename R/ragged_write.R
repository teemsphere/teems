#' @importFrom data.table fwrite setorder as.data.table set
#' @importFrom utils head
#' @keywords internal
#' @noRd
.ragged_write <- function(input,
                          write_path) {
  dim_sizes <- attr(input, "dim_sizes")
  idx <- length(colnames(input))

  cat(attr(input, "lead"),
    file = write_path,
    sep = "\n",
    append = TRUE
  )

  if (idx %=% 1L) {
    data.table::fwrite(
      x = input,
      file = write_path,
      quote = FALSE,
      append = TRUE
    )
  } else if (idx %=% 2L) {
    data.table::setorder(input)
    data.table::fwrite(
      x = input[, -1],
      file = write_path,
      quote = FALSE,
      append = TRUE
    )
  } else if (idx %=% 3L) {
    input <- input[, c(rev(utils::head(seq_len(idx), -1)), idx), with = FALSE]
    data.table::setorder(input)
    arr <- array(input$Value, dim_sizes)

    data.table::fwrite(
      x = data.table::as.data.table(arr),
      file = write_path,
      col.names = FALSE,
      append = TRUE
    )
  } else if (is.numeric(input[[idx]])) {
    input <- input[, c(rev(utils::head(seq_len(idx), -1)), idx), with = FALSE]
    data.table::setorder(input)
    d1 <- dim_sizes[1]
    d2 <- dim_sizes[2]
    n_slices <- prod(dim_sizes[-c(1, 2)])
    arr <- input[[idx]]
    if (d1 == 1L && d2 > 1L) {
      dim(arr) <- c(d2 * n_slices, 1L)
      d1 <- d2
      d2 <- 1L
    } else {
      dim(arr) <- c(d1, d2, n_slices)
      arr <- aperm(arr, c(1, 3, 2))
      dim(arr) <- c(d1 * n_slices, d2)
    }
    stacked <- data.table::as.data.table(arr)
    scratch <- tempfile(fileext = ".txt")
    data.table::fwrite(
      stacked[, d2, with = FALSE],
      file = scratch,
      col.names = FALSE
    )
    final_col <- readLines(scratch)
    unlink(scratch)
    slice_ends <- seq.int(d1, d1 * n_slices, by = d1)
    final_col[slice_ends] <- paste0(final_col[slice_ends], "\n")
    data.table::set(stacked, j = d2, value = final_col)
    data.table::fwrite(
      stacked,
      file = write_path,
      col.names = FALSE,
      quote = FALSE,
      append = TRUE
    )
  } else {
    input <- input[, c(rev(utils::head(seq_len(idx), -1)), idx), with = FALSE]
    data.table::setorder(input)
    arr <- array(input[[idx]], dim_sizes)

    ls_dt <- .slice_array(
      arr = arr,
      dim_sizes = dim_sizes
    )

    lapply(
      ls_dt,
      \(dt) {
        data.table::fwrite(
          dt,
          file = write_path,
          col.names = FALSE,
          append = TRUE
        )
        cat("\n",
          file = write_path,
          sep = "",
          append = TRUE
        )
      }
    )
  }

  if (idx < 4) {
    cat("\n",
      file = write_path,
      sep = "",
      append = TRUE
    )
  }

  return(write_path)
}
