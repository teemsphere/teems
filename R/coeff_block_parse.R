#' @importFrom data.table setDT
#'
#' @keywords internal
#' @noRd
.parse_coeff_block <- function(dimen, col_nmes, num_ls, sets, call) {
  dim_length <- length(dimen)
  has_sets   <- col_nmes %!=% NA_character_
  
  if (has_sets) {
    r_idx <- match(.dock_tail(string = col_nmes), names(sets))
    if (anyNA(r_idx)) {
      .cli_action(compose_err$invalid_coeff_set, action = "abort", call = call)
    }
    setele <- sets[r_idx]
  }
  
  if (dim_length %=% 1L) {
    if (!has_sets) {
      df <- data.frame(Value = as.numeric(num_ls))
    } else {
      df <- data.frame(setele[[1]], Value = as.numeric(num_ls))
      colnames(df)[1] <- col_nmes
    }
  } else {
    if (dim_length %=% 2L) {
      flat_vec <- as.numeric(unlist(strsplit(num_ls, split = ",")))
      arr <- t(array(flat_vec, rev(dimen)))
    } else {
      rows  <- lapply(num_ls, function(r) as.numeric(unlist(strsplit(r, ","))))
      blank <- vapply(rows, function(r) length(r) %=% 0L, logical(1))
      slices <- split(rows[!blank], cumsum(blank)[!blank])
      ls_mat <- lapply(slices, function(g) t(array(unlist(g), rev(dimen[c(1, 2)]))))
      arr <- array(unlist(ls_mat), dimen)
    }
    dimnames(arr) <- setele
    names(dimnames(arr)) <- col_nmes
    df <- array2DF(arr)
  }
  
  if (has_sets) {
    numeric_cols <- colnames(df)[vapply(setele, is.numeric, logical(1))]
    if (length(numeric_cols) > 0L) {
      df[numeric_cols] <- lapply(df[numeric_cols], as.numeric)
    }
    data.table::setDT(df, key = col_nmes)
  } else {
    data.table::setDT(df)
  }
  return(df)
}