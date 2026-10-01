#' @noRd
#' @keywords internal
.construct_shk <- function(raw_shock,
                           closure,
                           sets,
                           ...) {
  UseMethod(".construct_shk")
}

#' @importFrom data.table setnames fsetdiff rbindlist
#' @importFrom utils capture.output
#' @importFrom purrr map_lgl map
#' @noRd
#' @keywords internal
#' @method .construct_shk custom
#' @export
.construct_shk.custom <- function(raw_shock,
                                    closure,
                                    sets,
                                    ...) {
  
  is_full <- attr(raw_shock, "is_full")

  if (.o_check_shock_status()) {
    cls_entries <- closure[purrr::map_lgl(closure, \(c) {
      attr(c, "var_name") == raw_shock$var
    })]

    if (length(cls_entries) %=% 0L) {
      call <- attr(raw_shock, "call")
      .cli_action(
        if (is_full) shk_err$x_full_exo else shk_err$x_full_exo_part,
        action = if (is_full) c("abort", "inform", "inform") else c("abort", "inform"),
        call = call
      )
    }

    if (is_full) {
      if (!inherits(cls_entries[[1]], "full")) {
        .cli_action(
          shk_err$x_full_exo,
          action = c("abort", "inform", "inform"),
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

          call <- attr(raw_shock, "call")
          .cli_action(shk_err$cust_endo_tup,
            action = c("abort", "inform"),
            call = call
          )
        }
      }
    }
  }

  shock <- .reduce_shk(
    raw_shock = raw_shock,
    sets = sets
  )
  attr(shock, "tuples") <- .shk_input_tuples(raw_shock$input[, c(raw_shock$ls_mixed, "Value"), with = FALSE])

  return(shock)
}

#' @importFrom data.table CJ setnames
#' @noRd
#' @keywords internal
#' @method .construct_shk scenario
#' @export
.construct_shk.scenario <- function(raw_shock,
                                      closure,
                                      sets,
                                      ...) {
  Value <- NULL

  set_ele <- with(sets$ele, mget(raw_shock$ls_upper))
  template_shk <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  data.table::setnames(template_shk, new = raw_shock$ls_upper)
  value <- raw_shock$input

  class(value) <- c("dat", class(value))
  data.table::setnames(value, raw_shock$ls_mixed, raw_shock$ls_upper)
  value <- .aggregate_data(
    dt = value,
    sets = sets$mapping,
    shock = TRUE
  )

  int_set_names <- sets[sets$qualifier_list == "(intertemporal)", "name"][[1]]
  int_col <- which(colnames(value) %in% int_set_names)
  data.table::setnames(value, raw_shock$ls_upper, raw_shock$ls_mixed)
  non_int_col <- colnames(value)[-c(int_col, ncol(value))]
  int_col <- colnames(value)[int_col]

  value[, let(Value = {
    baseline <- Value[get(int_col) == 0]
    (Value - baseline) / baseline * 100
  }), by = non_int_col]

  raw_shock$input <- value
  class(raw_shock)[[1]] <- "custom"

  .construct_shk(
    raw_shock = raw_shock,
    closure = closure,
    sets = sets
  )
}

#' @importFrom utils capture.output
#' @importFrom purrr map_chr map_lgl map
#' @importFrom data.table fsetdiff rbindlist setnames
#' @noRd
#' @keywords internal
#' @method .construct_shk uniform
#' @export
.construct_shk.uniform <- function(raw_shock,
                                     closure,
                                     sets,
                                     var_extract,
                                     ...) {
  
  call <- attr(raw_shock, "call")
  single_ele <- FALSE
  scalar <- length(raw_shock$ls_upper) %=% 0L || anyNA(raw_shock$ls_upper) ||
    raw_shock$ls_upper %=% "null_set"
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

    if (scalar) {
      shock_LHS <- raw_shock$var
    } else {
      shock_LHS <- paste0(raw_shock$var, "(", paste0(raw_shock$ls_upper, collapse = ","), ")")
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
      single_ele <- length(r_idx) %=% length(raw_shock$ls_upper) && !any(ss)
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

      check <- closure[purrr::map_lgl(closure, \(c) {
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
        data.table::setnames(check, raw_shock$ls_mixed)
        check2 <- data.table::setnames(attr(expanded_shk, "ele"), raw_shock$ls_mixed)
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

  shock_RHS <- if (single_ele || scalar) {
    paste("=", paste0(raw_shock$input, ";", "\n"))
  } else {
    paste("=", "uniform", paste0(raw_shock$input, ";", "\n"))
  }
  shock <- list(shock = paste("Shock", shock_LHS, shock_RHS))
  shock <- structure(shock,
    class = class(raw_shock),
    full_var = attr(raw_shock, "full_var")
  )

  shock <- list(shock)
  return(shock)
}
