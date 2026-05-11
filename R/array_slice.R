#' @importFrom data.table as.data.table
#' 
#' @keywords internal
#' @noRd
.slice_array <- function(arr, dim_sizes) {
  n_dims <- length(dim_sizes)
  other_dims <- setdiff(1:n_dims, c(1, 2))
  other_indices <- lapply(dim_sizes[other_dims], seq_len)
  other_grid <- as.matrix(expand.grid(other_indices))
  n_slices <- nrow(other_grid)

  ls_dt <- vector("list", n_slices)
  for (i in seq_len(n_slices)) {
    full_indices <- vector("list", n_dims)
    full_indices[c(1, 2)] <- list(quote(expr = ))
    full_indices[other_dims] <- other_grid[i, ]
    ls_dt[[i]] <- data.table::as.data.table(
      do.call("[", c(list(arr), full_indices))
    )
  }
  return(ls_dt)
}