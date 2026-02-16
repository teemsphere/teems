#' @keywords internal
#' @noRd
.finalize_closure <- function(swap_in,
                              swap_out,
                              closure,
                              closure_file,
                              sets,
                              var_extract,
                              call) {

  if (!is.null(swap_in)) {
    swap_in <- .classify_cls(
      closure = swap_in,
      sets = sets
    )

    swap_in <- lapply(swap_in,
      .exp_cls_entry,
      var_extract = var_extract,
      sets = sets$ele,
      call = call
    )

    closure <- c(closure, swap_in)
  }

  if (!is.null(swap_out)) {
    swap_out <- .classify_cls(
      closure = swap_out,
      sets = sets,
      call = call
    )

    swap_out <- lapply(swap_out,
      .exp_cls_entry,
      var_extract = var_extract,
      sets = sets$ele,
      call = call
    )

    for (s in seq_along(swap_out)) {
      swap <- swap_out[[s]]
      closure <- .swap_out(
        swap_out = swap,
        closure = closure,
        sets = sets,
        var_extract = var_extract,
        call = call
      )
    }
  }

  attr(closure, "file") <- closure_file
  class(closure) <- c("closure", class(closure))
  return(closure)
}