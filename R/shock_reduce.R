#' @importFrom utils combn
#' @importFrom data.table copy setorder setorderv uniqueN .SD .N
#' @noRd
#' @keywords internal
.reduce_shock <- function(raw_shock,
                          sets) {
  
  # NSE
  Value <- NULL
  n <- NULL
  val <- NULL
  n_vals <- NULL
  .N <- NULL
  
  set_cols <- raw_shock$ls_mixed
  set_upper <- raw_shock$ls_upper
  n_sets <- length(set_cols)

  set_sizes <- with(sets$ele, lengths(mget(set_upper)))
  names(set_sizes) <- set_cols

  all_free <- unlist(
    lapply(seq_len(n_sets - 1L), function(k) {
      utils::combn(seq_len(n_sets), k, simplify = FALSE)
    }),
    recursive = FALSE
  )
  factors <- vapply(all_free, function(idx) prod(set_sizes[idx]), numeric(1L))
  all_free <- all_free[order(factors, decreasing = TRUE)]

  remaining <- data.table::copy(raw_shock$input)
  shock_list <- list()
  base_cls <- class(raw_shock)

  for (free_idx in all_free) {
    if (nrow(remaining) == 0L) break

    free_cols <- set_cols[free_idx]
    fixed_cols <- set_cols[-free_idx]
    n_free <- prod(set_sizes[free_idx])

    grp <- remaining[,
      list(n = .N, val = Value[1L], n_vals = data.table::uniqueN(Value)),
      by = fixed_cols
    ]

    uni_grps <- grp[n == n_free & n_vals == 1L]
    het_grps <- grp[n == n_free & n_vals > 1L]

    if (nrow(uni_grps) > 0L) {
      ele_lines <- character(nrow(uni_grps))
      for (g_i in seq_len(nrow(uni_grps))) {
        g <- uni_grps[g_i]
        idx_parts <- set_upper
        names(idx_parts) <- set_cols
        for (fc in fixed_cols) {
          idx_parts[fc] <- paste0('"', g[[fc]], '"')
        }
        
        ele_lines[g_i] <- paste0(
          "Shock ", raw_shock$var, "(",
          paste(idx_parts, collapse = ","),
          ") = uniform ", g$val, ";\n"
        )
      }
      shock_list <- c(
        shock_list,
        list(structure(
          list(ele = ele_lines),
          class = append(base_cls, "ele", after = 1L)
        ))
      )
      remaining <- remaining[
        !uni_grps[, .SD, .SDcols = fixed_cols],
        on = fixed_cols
      ]
    }

    for (g_i in seq_len(nrow(het_grps))) {
      g <- het_grps[g_i]
      sub_dt <- remaining[g[, .SD, .SDcols = fixed_cols], on = fixed_cols]
      sub_dt <- sub_dt[, .SD, .SDcols = c(free_cols, "Value")]

      idx_parts <- set_upper
      names(idx_parts) <- set_cols
      for (fc in fixed_cols) {
        idx_parts[fc] <- paste0('"', g[[fc]], '"')
      }
      lead <- paste(
        "Shock",
        paste0(raw_shock$var, "(", paste(idx_parts, collapse = ","), ")"),
        "="
      )

      full_dimsizes <- set_sizes
      full_dimsizes[-free_idx] <- 1L
      write_order <- .shk_write_order(free_idx, full_dimsizes)
      sub_dt <- sub_dt[, .SD, .SDcols = c(free_cols[write_order], "Value")]
      data.table::setorderv(sub_dt, free_cols[write_order])

      shock_list <- c(
        shock_list,
        list(structure(
          list(dt = sub_dt),
          lead = lead,
          class = append(base_cls, "full", after = 1L)
        ))
      )
    }
    if (nrow(het_grps) > 0L) {
      remaining <- remaining[
        !het_grps[, .SD, .SDcols = fixed_cols],
        on = fixed_cols
      ]
    }
  }

  if (nrow(remaining) > 0L) {
    ele_lines <- character(nrow(remaining))
    for (r_i in seq_len(nrow(remaining))) {
      row <- remaining[r_i]
      idx_parts <- paste0('"', unlist(row[, .SD, .SDcols = set_cols]), '"',
        collapse = ","
      )
      ele_lines[r_i] <- paste0(
        "Shock ", raw_shock$var, "(", idx_parts, ") = ", row$Value, ";"
      )
    }
    shock_list <- c(
      shock_list,
      list(structure(
        list(ele = ele_lines),
        class = append(base_cls, "ele", after = 1L)
      ))
    )
  }

  return(shock_list)
}
