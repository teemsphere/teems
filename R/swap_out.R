#' @keywords internal
#' @noRd
.swap_out <- function(swap,
                      closure,
                      sets,
                      var_name,
                      var_extract,
                      var_entries,
                      call) {
  UseMethod(".swap_out")
}

#' @importFrom purrr map pluck
#' @importFrom data.table rbindlist fsetdiff fintersect setnames copy
#' @importFrom utils capture.output head
#' @importFrom cli cli_format
#' @keywords internal
#' @noRd
#' @method .swap_out default
#' @export
.swap_out.default <- function(swap,
                              closure,
                              sets,
                              var_name,
                              var_extract,
                              var_entries,
                              call) {

  template <- data.table::rbindlist(purrr::map(var_entries, attr, "ele"))
  idx_sets <- purrr::pluck(var_extract, "ls_mixed_idx", var_name)
  data.table::setnames(template, new = idx_sets)
  check <- data.table::copy(attr(swap, "ele"))
  data.table::setnames(check, new = idx_sets)
  
  if (nrow(data.table::fsetdiff(check, template)) %!=% 0L) {
    invalid_tuples <- cli::cli_format(data.table::fsetdiff(check, template))
    invalid_tuples <- utils::head(utils::capture.output(print(invalid_tuples))[-c(1:3)], -1)
    n_invalid_tuples <- length(invalid_tuples)
    if (!is.null(attr(swap, "call"))) {
      call <- attr(swap, "call")
    }
    .cli_action(swap_err$invalid_tup,
      action = c("abort", "inform"),
      call = call
    )
  }

  for (e in seq_along(var_entries)) {
    entry <- var_entries[[e]]
    entry_check <- data.table::copy(attr(entry, "ele"))
    data.table::setnames(entry_check, new = idx_sets)
    if (nrow(data.table::fintersect(entry_check, check)) %!=% 0L) {
      reduced_entry <- list()
      reduced_entry <- .reduce2sets(
        preswap = entry,
        swap = swap,
        reduced_entry = reduced_entry
      )

      cls_append <- .classify_cls(
        closure = reduced_entry,
        sets = sets,
        call
      )
      cls_append <- lapply(cls_append,
        .exp_cls_entry,
        var_extract = var_extract,
        sets = sets$ele,
        call = call
      )
      closure <- setdiff(closure, entry)
      closure <- c(closure, cls_append)
    }
  }
  return(closure)
}

#' @importFrom purrr pluck map_lgl map
#' @importFrom data.table CJ rbindlist fsetdiff setnames
#' @keywords internal
#' @noRd
#' @method .swap_out full
#' @export
.swap_out.full <- function(swap,
                           closure,
                           sets,
                           var_name,
                           var_extract,
                           var_entries,
                           call) {
  if (length(var_entries) %=% 1L) {
    if (!inherits(var_entries[[1]], "full")) {
      .cli_action(swap_err$invalid_full,
        action = "abort",
        call = call
      )
    }

    closure <- setdiff(closure, var_name)
  } else {
    check <- data.table::rbindlist(purrr::map(var_entries, attr, "ele"))
    var_sets <- purrr::pluck(var_extract, "ls_upper_idx", var_name)
    full_var <- do.call(
      data.table::CJ,
      c(with(sets$ele, mget(var_sets, ifnotfound = "")), sorted = FALSE)
    )
    idx_sets <- purrr::pluck(var_extract, "ls_mixed_idx", var_name)
    data.table::setnames(full_var, new = idx_sets)
    data.table::setnames(check, new = idx_sets)
    
    if (nrow(data.table::fsetdiff(full_var, check)) %!=% 0L) {
      if (!is.null(attr(swap, "call"))) {
        call <- attr(swap, "call")
      }
      .cli_action(swap_err$invalid_full,
        action = c("abort", "inform"),
        call = call
      )
    }

    closure <- closure[!purrr::map_lgl(closure, \(c) {
      attr(c, "var_name") %=% var_name
    })]
  }
}
