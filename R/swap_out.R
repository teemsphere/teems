#' @keywords internal
#' @noRd
.swap_out <- function(swap_out,
                      closure,
                      sets,
                      var_name,
                      var_extract,
                      var_entries,
                      call) {
  UseMethod(".swap_out")
}

#' @importFrom purrr map_lgl map
#' @importFrom data.table rbindlist fsetdiff fintersect
#' @importFrom utils capture.output head
#' @importFrom cli cli_format
#' 
#' @keywords internal
#' @noRd
#' @method .swap_out default
#' @export
.swap_out.default <- function(swap_out,
                              closure,
                              sets,
                              var_name,
                              var_extract,
                              var_entries,
                              call) {
  
  check <- data.table::rbindlist(purrr::map(var_entries, attr, "ele"))
  if (nrow(data.table::fsetdiff(attr(swap_out, "ele"), check)) %!=% 0L) {
    invalid_tuples <- cli::cli_format(data.table::fsetdiff(attr(swap_out, "ele"), check))
    invalid_tuples <- utils::head(utils::capture.output(print(invalid_tuples))[-c(1:3)], -1)
    n_invalid_tuples <- length(invalid_tuples)
    .cli_action(swap_err$invalid_tup,
      action = "abort",
      call = call
    )
  }

  for (e in seq_along(var_entries)) {
    entry <- var_entries[[e]]
    if (nrow(data.table::fintersect(attr(entry, "ele"), attr(swap_out, "ele"))) %!=% 0L) {
      reduced_entry <- list()
      reduced_entry <- .reduce2sets(
        preswap = entry,
        swap = swap_out,
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
