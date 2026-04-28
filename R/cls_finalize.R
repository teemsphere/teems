#' @importFrom purrr map_chr map map_lgl
#' @importFrom data.table rbindlist fintersect
#' @importFrom utils capture.output
#'
#' @keywords internal
#' @noRd
.finalize_closure <- function(closure,
                              closure_file,
                              swap_in,
                              swap_out,
                              sets,
                              var_extract,
                              call,
                              model_call) {
  closure <- .validate_closure(
    closure = closure,
    sets = sets,
    var_extract = var_extract,
    call = call,
    model_call = model_call
  )

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
        idx_sets <- purrr::pluck(var_extract, "ls_mixed_idx", var_name)
        data.table::setnames(check, new = idx_sets)
        swap_check <- data.table::copy(attr(swap, "ele"))
        data.table::setnames(swap_check, new = idx_sets)
        if (nrow(data.table::fintersect(swap_check, check)) %!=% 0L) {
          if (!is.null(attr(swap, "call"))) {
            call <- attr(swap, "call")
          }
          overlap <- data.table::fintersect(swap_check, check)
          overlap <- utils::capture.output(print(overlap))[-c(1, 2)]
          .cli_action(swap_err$overlap_ele,
            action = c("abort", "inform"),
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
        if (!is.null(attr(swap, "call"))) {
          call <- attr(swap, "call")
        }
        .cli_action(swap_err$no_var_cls,
          action = c("abort", "inform"),
          call = call
        )
      }

      closure <- .swap_out(
        swap = swap,
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