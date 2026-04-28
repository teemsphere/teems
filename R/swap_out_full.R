#' @importFrom purrr pluck map_lgl map
#' @importFrom data.table CJ rbindlist fsetdiff setnames
#'
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

    closure <- closure[!purrr::map_lgl(closure, function(c) {
      attr(c, "var_name") %=% var_name
    })]
  }
}