# Condensation engine (GEMPACK manual 10.16 / 14.1.10):
# backsolving at the TAB level. In-TAB Omit statements are read and
# ignored: omission neither decreases memory usage nor changes the
# solved system in TEEMS (measured 2026-09-17), so it is not offered. Backsolved
# variables are symbolically substituted out of every other equation;
# the nominated (defining) equation is retained and a Backsolve
# statement is emitted at write-out so the solver can recover values
# post-solve. Substitutions are applied to previously retained defining
# equations too, so every retained equation references surviving
# variables only.

#' @importFrom purrr map_chr map_lgl
#'
#' @keywords internal
#' @noRd
.cndns_model <- function(tab,
                         backsolve,
                         ignore_condense,
                         quiet,
                         call) {
  none <- list(
    tab = tab,
    flags = NULL,
    n_backsolve = 0L
  )

  intab <- .parse_intab_cndns(tab)

  if (length(intab$rows) > 0L) {
    tab <- tab[-intab$rows]
    if (ignore_condense) {
      n_ignored <- length(intab$rows)
      if (!quiet) {
        .cli_action(model_info$condense_ignored,
          action = "inform",
          call = call
        )
      }
      intab$actions <- list()
    } else {
      sub_var <- purrr::map_chr(
        intab$actions[purrr::map_lgl(intab$actions, "substitute")],
        "var"
      )
      if (length(sub_var) > 0L && !quiet) {
        .cli_action(model_info$substitute_as_backsolve,
          action = c("inform", "inform"),
          call = call
        )
      }
    }
  }

  omit_var <- purrr::map_chr(
    intab$actions[purrr::map_chr(intab$actions, "action") == "omit"],
    "var"
  )
  if (length(omit_var) > 0L && !quiet) {
    .cli_action(model_info$omit_ignored,
      action = c("inform", "inform"),
      call = call
    )
  }
  intab_backsolve <- intab$actions[
    purrr::map_chr(intab$actions, "action") == "backsolve"
  ]

  if (length(intab_backsolve) == 0L && is.null(backsolve)) {
    none$tab <- tab
    return(none)
  }

  extract <- .generate_extracts(tab = tab, call = call)
  var_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "variable",
    call = call
  )
  coeff_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "coefficient",
    call = call
  )
  math_extract <- .parse_tab_maths(
    extract = extract$model,
    call = call
  )
  eq_names <- math_extract$name[math_extract$type %in% "Equation"]

  pairs <- .resolve_backsolves(
    intab_backsolve = intab_backsolve,
    backsolve = backsolve,
    var_extract = var_extract,
    eq_names = eq_names,
    quiet = quiet,
    call = call
  )
  pairs <- pairs[!duplicated(purrr::map_chr(pairs, \(p) {
    paste(tolower(p$var), tolower(p$eq))
  }))]

  all_vars <- purrr::map_chr(pairs, "var")
  if (anyDuplicated(tolower(all_vars))) {
    conflict_var <- unique(all_vars[duplicated(tolower(all_vars))])
    .cli_action(model_err$condense_conflict,
      action = "abort",
      call = call
    )
  }

  if (length(pairs) > 0L) {
    all_eqs <- purrr::map_chr(pairs, "eq")
    if (anyDuplicated(tolower(all_eqs))) {
      reused_eq <- unique(all_eqs[duplicated(tolower(all_eqs))])
      .cli_action(model_err$condense_eq_reused,
        action = "abort",
        call = call
      )
    }
  }

  flags <- NULL
  if (length(pairs) > 0L) {
    tab <- .backsolve_all(
      tab = tab,
      pairs = pairs,
      var_extract = var_extract,
      coeff_extract = coeff_extract,
      eq_names = eq_names,
      call = call
    )

    flags <- rbind(
      data.frame(
        name = purrr::map_chr(pairs, "var"),
        type = "Variable",
        condense = "backsolve",
        condense_eq = purrr::map_chr(pairs, "eq")
      ),
      data.frame(
        name = purrr::map_chr(pairs, "eq"),
        type = "Equation",
        condense = "backsolve",
        condense_eq = purrr::map_chr(pairs, "eq")
      )
    )
  }

  condensed <- list(
    tab = tab,
    flags = flags,
    n_backsolve = length(pairs)
  )
  return(condensed)
}

# ---------------------------------------------------------------------
# Backsolve engine
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.prune_zero_terms <- function(terms) {
  return(terms[!purrr::map_lgl(terms, \(t) {
    any(t$fac == "0" & t$ops == "*")
  })])
}

#' @keywords internal
#' @noRd
.quant_binding <- function(quants) {
  binding <- purrr::map_chr(quants, "set")
  names(binding) <- purrr::map_chr(quants, "idx")
  return(binding)
}
