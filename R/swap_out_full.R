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
                           var_extract,
                           call) {
  var_name <- attr(swap_out, "var_name")
  var_entries <- closure[purrr::map_lgl(closure, function(c) {
    attr(c, "var_name") == var_name
  })]

  if (length(var_entries) %=% 0L) {
    .cli_action(cls_err$no_var_cls,
                action = "abort",
                call = call
    )
  } else if (length(var_entries) %=% 1L) {
    if (!inherits(var_entries[[1]], "full")) {
      .cli_action(cls_err$invalid_full,
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
      c(with(sets$ele, mget(x = var_sets, ifnotfound = "")), sorted = FALSE)
    )
    if (nrow(data.table::fsetdiff(full_var, check)) %!=% 0L) {
      .cli_action(cls_err$invalid_full,
                  action = "abort",
                  call = call
      )
    }
    closure <- closure[!purrr::map_lgl(closure, function(c) {
      attr(c, "var_name") %=% var_name
    })]
  }
}