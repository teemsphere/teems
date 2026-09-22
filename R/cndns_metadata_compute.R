#' @keywords internal
#' @noRd
.compute_cndns_metadata <- function(model,
                                    sets,
                                    system_size) {
  vars <- model[model$type == "Variable" & !is.na(model$condense), ]
  backsolved <- vars[vars$condense %in% "backsolve", ]

  n_backsolve_ele <- .count_var_elements(
    var_extract = backsolved,
    sets = sets
  )
  uncondensed <- system_size + n_backsolve_ele

  metadata <- list(
    n_backsolve = nrow(backsolved),
    n_backsolve_ele = n_backsolve_ele,
    elimination_share = if (uncondensed > 0) {
      n_backsolve_ele / uncondensed
    } else {
      0
    }
  )
  return(metadata)
}
