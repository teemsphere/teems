#' @importFrom data.table rbindlist let
#' @importFrom purrr map
#' @keywords internal
#' @noRd
.zero_families <- function(.data,
                           zero_below) {
  Value <- NULL

  if (zero_below <= 0) {
    return(.data)
  }
  for (family in zero_families) {
    members <- family$members[family$members %in% names(.data)]
    if (length(members) < 2L) {
      next
    }
    idx <- setdiff(names(.data[[members[1]]]), "Value")
    aligned <- purrr::map_lgl(members, \(m) {
      dt <- .data[[m]]
      is.numeric(dt$Value) && identical(setdiff(names(dt), "Value"), idx)
    })
    if (!all(aligned)) {
      next
    }
    cells <- data.table::rbindlist(purrr::map(members, \(m) {
      dt <- .data[[m]]
      dt[Value == 0, idx, with = FALSE]
    }))
    if (nrow(cells) == 0L) {
      next
    }
    cells <- unique(cells)
    for (m in members) {
      .data[[m]][cells, on = idx, let(Value = 0)]
    }
    slaves <- family$slaves[family$slaves %in% names(.data)]
    for (s in slaves) {
      on <- intersect(idx, setdiff(names(.data[[s]]), "Value"))
      if (length(on) == length(idx)) {
        .data[[s]][cells, on = on, let(Value = 0)]
      }
    }
  }
  return(.data)
}
