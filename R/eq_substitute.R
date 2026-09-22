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
  used <- .eq_idents(entry)

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
    }
    entry[[side]] <- new_side
  }

  if (touched) {
    entry$dirty <- TRUE
    eqs[[entry_name]] <- entry
  }
  return(invisible(entry))
}
