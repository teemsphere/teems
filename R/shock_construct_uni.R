#' @importFrom utils capture.output
#' @importFrom purrr map_chr map_lgl
#'
#' @noRd
#' @keywords internal
#' @method .construct_shock uniform
#' @export
.construct_shock.uniform <- function(raw_shock,
                                     closure,
                                     sets,
                                     var_extract,
                                     ...) {
  call <- attr(raw_shock, "call")
  if (attr(raw_shock, "full_var")) {
    if (.o_check_shock_status()) {
      full_vars <- purrr::map_chr(closure[purrr::map_lgl(closure, inherits, "full")], attr, "var_name")
      if (!raw_shock$var %in% full_vars) {
        .cli_action(
          shk_err$x_full_exo,
          action = c("abort", "inform", "inform"),
          call = call
        )
      }
    }

    if (raw_shock$ls_upper %!=% "null_set") {
      shock_LHS <- paste0(raw_shock$var, "(", paste0(raw_shock$ls_upper, collapse = ","), ")")
    } else {
      shock_LHS <- raw_shock$var
    }
  } else {
    mixed_ss <- names(raw_shock$subset)
    r_idx <- match(mixed_ss, raw_shock$ls_mixed)
    if (raw_shock$ls_upper %!=% NA) {
      shock_LHS <- raw_shock$ls_upper
      ss <- purrr::map_lgl(raw_shock$subset, attr, "subset")
      shock_LHS[r_idx] <- ifelse(!ss,
                                 paste0('"', raw_shock$subset, '"'),
                                 raw_shock$subset)
      shock_LHS <- paste0(raw_shock$var, "(", paste0(shock_LHS, collapse = ","), ")")
    } else {
      shock_LHS <- raw_shock$var
    }

    if (.o_check_shock_status()) {
      classified_shk <- .classify_cls(
        closure = shock_LHS,
        sets = sets,
        call = call
      )[[1]]

      expanded_shk <- .exp_cls_entry(
        cls_entry = classified_shk,
        var_extract = var_extract,
        sets = sets$ele
      )

      check <- closure[purrr::map_lgl(closure, function(c) {
        attr(c, "var_name") %=% raw_shock$var
      })]

      if (length(check) %=% 0L) {
        .cli_action(shk_err$x_full_exo_part,
          action = c("abort", "inform"),
          call = call
        )
      }

      if (attr(check[[1]], "ele") %!=% NA) {
        check <- data.table::rbindlist(purrr::map(check, attr, "ele"))
        data.table::setnames(check, new = raw_shock$ls_mixed)
        check2 <- data.table::setnames(attr(expanded_shk, "ele"), new = raw_shock$ls_mixed)
        if (nrow(data.table::fsetdiff(check2, check)) %!=% 0L) {
          errant_tup <- data.table::fsetdiff(check2, check)
          errant_tup <- utils::capture.output(print(errant_tup))[-c(1, 2, 3)]
          .cli_action(
            shk_err$x_part_exo,
            action = c("abort", "inform"),
            call = call
          )
        }
      }
    }
  }

  shock_RHS <- paste("=", "uniform", paste0(raw_shock$input, ";", "\n"))
  shock <- list(shock = paste("Shock", shock_LHS, shock_RHS))
  shock <- structure(shock,
    class = class(raw_shock),
    full_var = attr(raw_shock, "full_var")
  )

  shock <- list(shock)
  return(shock)
}