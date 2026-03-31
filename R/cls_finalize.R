#' @importFrom purrr map_chr map map_lgl
#' @importFrom data.table rbindlist fintersect
#' @importFrom utils capture.output
#'
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

    for (s in seq_along(swap_in)) {
      swap <- swap_in[[s]]
      var_name <- attr(swap, "var_name")
      var_entries <- closure[purrr::map_chr(closure, attr, "var_name") == var_name]

      if (length(var_entries) %!=% 0L) {
        check <- data.table::rbindlist(purrr::map(var_entries, attr, "ele"))
        if (nrow(data.table::fintersect(attr(swap, "ele"), check)) %!=% 0L) {
          overlap <- data.table::fintersect(attr(swap, "ele"), check)
          overlap <- utils::capture.output(print(overlap))[-c(1, 2)]
          .cli_action(swap_err$overlap_ele,
            action = "abort",
            call = call
          )
        }
      }
    }

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
      var_name <- attr(swap, "var_name")
      var_entries <- closure[purrr::map_lgl(closure, \(c) {
        attr(c, "var_name") == var_name
      })]

      if (length(var_entries) %=% 0L) {
        .cli_action(swap_err$no_var_cls,
          action = "abort",
          call = call
        )
      }

      closure <- .swap_out(
        swap_out = swap,
        closure = closure,
        sets = sets,
        var_name = var_name,
        var_extract = var_extract,
        var_entries = var_entries,
        call = call
      )
    }
  }

  attr(closure, "file") <- closure_file
  class(closure) <- c("closure", class(closure))
  return(closure)
}