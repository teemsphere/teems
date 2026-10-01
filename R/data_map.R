#' @importFrom data.table set 
#' @keywords internal
#' @noRd
.map_data <- function(dt,
                      sets,
                      col) {
  
  col_mapping <- data.frame(
    pos = which(!colnames(dt) %in% c("Value", "sigma", "omega", "sigma_v", "omega_v")),
    name = col,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(col_mapping))) {
    col_pos <- col_mapping$pos[i]
    set_col <- col_mapping$name[i]

    if (set_col %in% names(sets)) {
      table <- sets[[set_col]]
      r_idx <- match(dt[[col_pos]], tolower(table[, 1][[1]]))
      .abort_unmapped(dt[[col_pos]], r_idx, set_col)
      data.table::set(dt, j = col_pos, value = table[, 2][[1]][r_idx])
    }
  }
  return(dt)
}

#' @keywords internal
#' @noRd
.abort_unmapped <- function(ele,
                            r_idx,
                            map_name) {
  if (anyNA(r_idx)) {
    missing_ele <- unique(ele[is.na(r_idx)])
    .cli_action(data_err$missing_ele_mapping,
      action = "abort"
    )
  }
  return(invisible(NULL))
}
