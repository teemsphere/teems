#' @importFrom purrr pluck map_lgl map
#' @importFrom data.table CJ rbindlist fsetdiff
#' 
#' @keywords internal
#' @noRd
#' @method .swap_out full
#' @export
.swap_out.full <- function(swap_out,
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
    
    if (nrow(data.table::fsetdiff(full_var, check)) %!=% 0L) {
      .cli_action(swap_err$invalid_full,
                  action = "abort",
                  call = call
      )
    }
    
    closure <- closure[!purrr::map_lgl(closure, function(c) {
      attr(c, "var_name") %=% var_name
    })]
  }
}