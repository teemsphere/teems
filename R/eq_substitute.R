#' @keywords internal
#' @noRd
.substitute_into_eq <- function(entry_name,
                                eqs,
                                var_name,
                                def_args,
                                solution,
                                csub) {
  entry <- eqs[[entry_name]]

  touched <- FALSE
  refs <- FALSE
  for (t in c(entry$lhs, entry$rhs)) {
    if (!is.null(t$var) && t$var$name %=% var_name) {
      refs <- TRUE
      break
    }
  }
  if (!refs) {
    return(invisible(entry))
  }
  used <- entry$used
  added <- list()

  for (side in c("lhs", "rhs")) {
    new_side <- list()
    for (t in entry[[side]]) {
      if (is.null(t$var) || t$var$name %!=% var_name) {
        new_side <- c(new_side, list(t))
        next
      }
      touched <- TRUE
      expanded <- .expand_occurrence(
        t = t,
        def_args = def_args,
        solution = solution,
        entry = entry,
        used = used,
        csub = csub
      )
      new_side <- c(new_side, expanded$terms)
      used <- expanded$used
      added <- c(added, expanded$terms)
    }
    entry[[side]] <- new_side
  }

  if (touched) {
    entry$dirty <- TRUE
    entry$used <- unique(c(
      used,
      .eq_idents(list(quants = list(), lhs = added, rhs = list()))
    ))
    eqs[[entry_name]] <- entry
  }
  return(invisible(entry))
}
