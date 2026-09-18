#' Closure count-squaring check (validation table row C4)
#'
#' The exogenous element count fixed by the closure (after swaps) must
#' leave exactly as many endogenous variable elements as the equation
#' system determines. The solver has no named check for this: a
#' non-square system surfaces downstream as an unnamed MA48 failure.
#' Equation element counts come from the (all,...) quantifier sets of
#' each retained equation; equations nominated as condensation
#' defining equations are out of the solve system. When a quantifier
#' set cannot be resolved against the loaded set elements the check is
#' skipped rather than risk a false abort.
#'
#' @keywords internal
#' @noRd
.check_system_square <- function(model,
                                 var_extract,
                                 sets,
                                 closure,
                                 size_metadata,
                                 n_comp_active = 0,
                                 call) {
  eqs <- model[model$type == "Equation", ]
  defining <- unique(stats::na.omit(model$condense_eq))
  if (length(defining) > 0L) {
    eqs <- eqs[!tolower(eqs$name) %in% tolower(defining), ]
  }
  if (nrow(eqs) == 0L && n_comp_active == 0) {
    return(invisible(NULL))
  }

  eq_sets <- regmatches(
    eqs$tab,
    gregexpr("\\(\\s*all\\s*,[^,()]+,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)", eqs$tab, ignore.case = TRUE)
  )
  eq_sets <- lapply(eq_sets, \(m) {
    toupper(sub(".*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)$", "\\1", m))
  })
  set_sizes <- lengths(sets$ele)
  names(set_sizes) <- toupper(names(sets$ele))
  unresolved <- setdiff(unique(unlist(eq_sets)), names(set_sizes))
  if (length(unresolved) > 0L) {
    return(invisible(NULL))
  }
  n_eq_ele <- sum(vapply(
    eq_sets,
    \(s) prod(set_sizes[s]),
    numeric(1)
  ))
  # C2: one E_$comp equation element per ACTIVE (endogenous)
  # complementarity component (11.14 counting; inert components are
  # absorbed solver-side by their endogenous dummy, net zero)
  n_eq_ele <- n_eq_ele + n_comp_active

  n_var_ele <- size_metadata$n_var_ele
  n_exo_ele <- size_metadata$n_exo_ele
  n_endo <- n_var_ele - n_exo_ele
  if (n_endo == n_eq_ele) {
    return(invisible(NULL))
  }

  gap <- n_endo - n_eq_ele
  gap_abs <- abs(gap)
  gap_dir <- if (gap > 0) {
    "must still be exogenized"
  } else {
    "too many are exogenous (endogenize via swaps)"
  }
  candidate_txt <- .square_candidates(
    gap = gap,
    var_extract = var_extract,
    sets = sets,
    closure = closure
  )
  msg <- .cli_action(cls_err$not_square,
    action = c("abort", "inform", "inform", "inform"),
    call = call
  )
  return(msg)
}
