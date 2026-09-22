#' @importFrom purrr map_chr map_int map_lgl
#' @keywords internal
#' @noRd
.rearrange_defining <- function(entry,
                                var_name,
                                def_args,
                                csub,
                                call) {
  eq_name <- entry$name
  all_terms <- c(entry$lhs, .negate_terms(entry$rhs))
  is_x <- purrr::map_lgl(all_terms, \(t) {
    !is.null(t$var) && t$var$name %=% var_name
  })
  x_terms <- all_terms[is_x]
  others <- .negate_terms(all_terms[!is_x])

  pieces <- purrr::map_chr(x_terms, \(t) {
    body <- "1"
    if (length(t$fac) > 0L) {
      body <- t$fac[[1]]
      for (f in seq_along(t$fac)[-1]) {
        body <- paste0(body, t$ops[[f]], t$fac[[f]])
      }
    }
    for (q in rev(t$quants)) {
      body <- paste0("sum{", q$idx, ",", q$set, ", ", body, "}")
    }
    body
  })
  signs <- purrr::map_int(x_terms, "sign")

  if (all(pieces == "1")) {
    pivot_num <- sum(signs)
    if (pivot_num == 0L) {
      rule_text <- model_err$condense_cancels
      .cli_action(model_err$condense_rule,
        action = c("abort", "inform", "inform"),
        call = call
      )
    }
    if (pivot_num < 0L) {
      others <- .negate_terms(others)
    }
    if (abs(pivot_num) != 1L) {
      others <- lapply(others, \(t) {
        t$fac <- c(t$fac, as.character(abs(pivot_num)))
        t$ops <- c(t$ops, "/")
        t
      })
    }
    solution <- others
  } else {
    pivot_expr <- ""
    for (p in seq_along(pieces)) {
      joint <- if (p == 1L) {
        ifelse(signs[[p]] == 1L, "", "-")
      } else {
        ifelse(signs[[p]] == 1L, " + ", " - ")
      }
      pivot_expr <- paste0(pivot_expr, joint, pieces[[p]])
    }

    binding <- .quant_binding(entry$quants)
    binding <- binding[intersect(names(binding), .expr_idents(pivot_expr))]
    pivot_ref <- .csub_new(
      expr = pivot_expr,
      binding = binding,
      label = paste0("backsolve pivot (", entry$name, ")"),
      csub = csub
    )

    .cli_action(model_wrn$condense_pivot_zero,
      action = c("warn", "inform"),
      call = call
    )

    solution <- lapply(others, \(t) {
      t$fac <- c(t$fac, pivot_ref)
      t$ops <- c(t$ops, "/")
      t
    })
  }

  solution <- lapply(solution, \(t) {
    .hoist_term(
      term = t,
      binding = .quant_binding(c(entry$quants, t$quants)),
      label = paste0("backsolve product (", entry$name, ")"),
      csub = csub,
      min_fac = 2L
    )
  })
  return(solution)
}
