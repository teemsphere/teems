#' Complementarity statement validation (GEMPACK manual 10.17/11.14)
#'
#' Mirrors the solver-side fatals of tab_complementarity_transform:
#' the VARIABLE qualifier names a declared levels variable; at least
#' one bound, each a levels variable, a Coefficient (parameter) or a
#' real constant; the name is limited to 10 characters; the quantifier
#' count equals the argument count of the variable and of each
#' non-constant bound; and the 11.14.1 condensation guards (the
#' complementarity variable must not be backsolved). Set matching (equal or same-ordered
#' subset) stays solver-side: it needs resolved set elements.
#'
#' @keywords internal
#' @noRd
.chk_tab_comp <- function(model,
                          call) {
  comp_stmts <- model$tab[tolower(model$type) == "complementarity"]
  if (length(comp_stmts) == 0L) {
    return(invisible(NULL))
  }
  typ <- tolower(model$type)
  is_lev <- typ == "variable" & !is.na(model$qualifier_list) &
    grepl("\\blevels\\b", model$qualifier_list, ignore.case = TRUE)
  lev_names <- tolower(model$name[is_lev])
  par_names <- tolower(model$name[
    typ == "coefficient" & !is.na(model$qualifier_list) &
      grepl("\\bparameter\\b", model$qualifier_list, ignore.case = TRUE) &
      !grepl("\\bnon_parameter\\b", model$qualifier_list, ignore.case = TRUE)
  ])
  is_num <- function(x) {
    hit <- grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", x)
    return(hit)
  }
  n_args_of <- function(nme) {
    r <- which(tolower(model$name) == tolower(nme) &
      typ %in% c("variable", "coefficient"))[1]
    idx <- model$ls_upper_idx[[r]]
    if (idx %=% NA || is.null(idx)) {
      return(0L)
    }
    n_args <- length(idx)
    return(n_args)
  }
  for (statement in comp_stmts) {
    cp <- .parse_comp_stmt(statement, call = call)
    comp_name <- cp$name
    if (nchar(comp_name) > 10L) {
      .cli_action(model_err$comp_name_length,
        action = "abort",
        call = call
      )
    }
    comp_var <- cp$comp_var
    if (!tolower(comp_var) %in% lev_names) {
      .cli_action(model_err$comp_not_levels,
        action = "abort",
        call = call
      )
    }
    bounds <- c(cp$lower_bound, cp$upper_bound)
    if (length(bounds) == 0L) {
      .cli_action(model_err$comp_no_bound,
        action = "abort",
        call = call
      )
    }
    for (b in bounds) {
      if (is_num(b)) {
        next
      }
      if (tolower(b) %in% c(lev_names, par_names)) {
        next
      }
      bad_bound <- b
      .cli_action(model_err$comp_bad_bound,
        action = c("abort", "inform"),
        call = call
      )
    }
    # quantifier count vs the variable's and non-constant bounds'
    # argument counts (11.14 points 2-3; set matching is solver-side)
    for (ref_name in c(comp_var, bounds[!is_num(bounds)])) {
      n_args <- n_args_of(ref_name)
      n_quant <- cp$n_quant
      if (n_quant != n_args) {
        .cli_action(model_err$comp_quant_count,
          action = "abort",
          call = call
        )
      }
    }
    # 11.14.1 condensation guards
    comp_refs <- rbind(
      data.frame(nme = comp_var, role = "variable", no_backsolve = TRUE),
      if (!is.null(cp$lower_bound) && !is_num(cp$lower_bound)) {
        data.frame(nme = cp$lower_bound, role = "lower bound", no_backsolve = FALSE)
      },
      if (!is.null(cp$upper_bound) && !is_num(cp$upper_bound)) {
        data.frame(nme = cp$upper_bound, role = "upper bound", no_backsolve = FALSE)
      }
    )
    for (r in seq_len(nrow(comp_refs))) {
      v_row <- which(tolower(model$name) == tolower(comp_refs$nme[r]) &
        typ == "variable")[1]
      if (is.na(v_row)) {
        next
      }
      cond <- model$condense[v_row]
      if (comp_refs$no_backsolve[r] && cond %in% "backsolve") {
        bad_var <- model$name[v_row]
        bad_action <- "backsolved"
        comp_role <- comp_refs$role[r]
        .cli_action(model_err$comp_condense,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  return(invisible(NULL))
}
