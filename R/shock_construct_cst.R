#' @importFrom data.table CJ setnames fsetequal fsetdiff rbindlist
#' @importFrom utils capture.output
#' @importFrom purrr map_lgl map
#'
#' @noRd
#' @keywords internal
#'
#' @method .construct_shock custom
#' @export
.construct_shock.custom <- function(raw_shock,
                                    closure,
                                    sets,
                                    ...) {
  set_ele <- with(sets$ele, mget(raw_shock$ls_upper))
  template <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  data.table::setnames(template, new = raw_shock$ls_mixed)

  is_full <- data.table::fsetequal(template, raw_shock$input[, !"Value"])

  if (!is_full) {
    if (nrow(data.table::fsetdiff(raw_shock$input[, !"Value"], template)) %!=% 0L) {
      errant_tuples <- data.table::fsetdiff(raw_shock$input[, !"Value"], template)
      errant_tuples <- utils::capture.output(print(errant_tuples))
      errant_tuples <- errant_tuples[-c(1, 2)]
      .cli_action(shk_err$cust_invalid_tup,
        action = "abort",
        call = attr(raw_shock, "call")
      )
    }
  }

  if (.o_check_shock_status()) {
    cls_entries <- closure[purrr::map_lgl(closure, function(c) {
      attr(c, "var_name") == raw_shock$var
    })]

    if (is_full) {
      if (!inherits(cls_entries[[1]], "full")) {
        .cli_action(
          shk_err$x_full_exo,
          action = c("abort", "inform"),
          call = attr(raw_shock, "call")
        )
      }
    } else {
      if (!inherits(cls_entries[[1]], "full")) {
        all_exo_parts <- data.table::rbindlist(
          purrr::map(cls_entries, attr, "ele")
        )

        key_names <- names(raw_shock$input[, !"Value"])
        data.table::setnames(all_exo_parts, new = key_names)
        if (nrow(data.table::fsetdiff(raw_shock$input[, !"Value"], all_exo_parts)) %!=% 0L) {
          x_exo_parts <- data.table::fsetdiff(
            raw_shock$input[, key_names, with = FALSE],
            all_exo_parts
          )
          x_exo_parts <- trimws(utils::capture.output(print(x_exo_parts)))
          x_exo_parts <- x_exo_parts[-c(1, 2)]

          .cli_action(shk_err$cust_endo_tup,
            action = "abort",
            call = attr(raw_shock, "call")
          )
        }
      }
    }
  }

  shock <- .reduce_shock(
    raw_shock = raw_shock,
    sets = sets
  )

  return(shock)
}
