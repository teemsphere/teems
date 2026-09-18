#' @importFrom purrr map map_chr list_flatten
#' @importFrom utils tail
#'
# explicit Subset statements plus the subset relations a set expression
# implies (manual 10.1.1.1), collected per set
#' @keywords internal
#' @noRd
.parse_set_subsets <- function(sets,
                               extract,
                               expr_info,
                               is_builder,
                               is_set_eq,
                               call) {
  subsets <- extract[tolower(extract$type) %in% "subset",]
  if (any(grepl(pattern = "\\(by numbers\\)", subsets$remainder))) {
    .cli_action(
      msg = "Subset '(by numbers)' argument not supported.",
      action = "abort",
      call = call
    )
  }

  subsets$subset <- purrr::map_chr(subsets$remainder, \(s) {
    strsplit(s, " ")[[1]][1]
  })

  subsets$set <- purrr::map_chr(subsets$remainder, \(s) {
    utils::tail(strsplit(s, " ")[[1]], 1)
  })

  # S2 for Subset statements: both sides must be declared sets (an
  # unknown superset used to crash the fold below with a raw indexing
  # error); case mismatches are canonicalized to the declared form
  for (col in c("subset", "set")) {
    known <- subsets[[col]] %in% sets$name
    ci <- match(tolower(subsets[[col]]), tolower(sets$name))
    undecl <- !known & is.na(ci)
    if (any(undecl)) {
      j <- which(undecl)[1]
      bad_stmt <- paste("Subset", subsets$remainder[j])
      bad_refs <- subsets[[col]][j]
      .cli_action(model_err$set_undeclared,
        action = c("abort", "inform"),
        call = call
      )
    }
    subsets[[col]] <- ifelse(known, subsets[[col]], sets$name[ci])
  }

  sets$subsets <- vector("list", nrow(sets))
  r_idx <- match(subsets$set, sets$name)
  
  for (pos in seq_along(r_idx)) {
    id <- r_idx[pos]
    sets$subsets[[id]] <- c(sets$subsets[[id]], subsets$subset[[pos]])
  }

  # implied SUBSET statements (GEMPACK manual): all UNION/'+' makes
  # every named operand a subset of the result; all INTERSECT makes the
  # result a subset of every operand; a trailing top-level UNION
  # (INTERSECT) term is a subset (superset) of the result; the simple
  # two-set complement keeps the legacy rule (result and subtrahend are
  # subsets of the minuend). Anything else needs an explicit Subset.
  add_subs <- function(sets, set_nm, new) {
    r <- which(sets$name == set_nm)[1]
    if (is.na(r)) {
      return(sets)
    }
    sets$subsets[r] <- purrr::list_flatten(list(unique(c(sets$subsets[[r]], new))))
    return(sets)
  }
  for (i in seq_len(nrow(sets))) {
    fo <- expr_info[[i]]
    nm <- sets$name[i]
    if (is_builder[i]) {
      # the solver emits "subset NAME is subset of SRC" with the
      # rewritten element list
      sets <- add_subs(sets, sets$comp1[i], nm)
      next
    }
    if (!is.list(fo)) {
      next
    }
    if (is_set_eq[i]) {
      # set equality generates SUBSET statements both ways (manual 10.1.2.1)
      sets <- add_subs(sets, nm, fo$named[1])
      sets <- add_subs(sets, fo$named[1], nm)
      next
    }
    if (fo$simple_complement) {
      sets <- add_subs(sets, fo$named[1], c(nm, fo$named[2]))
    } else if (!is.na(fo$complement_of)) {
      # A - (...): the result is a subset of A (manual 11.7)
      sets <- add_subs(sets, fo$complement_of, nm)
    } else if (fo$all_plus_union) {
      sets <- add_subs(sets, nm, fo$named)
    } else if (fo$all_intersect) {
      for (tnm in fo$named) sets <- add_subs(sets, tnm, nm)
    } else {
      if (isTRUE(fo$last_top_op %=% "^") && !is.na(fo$last_term)) {
        sets <- add_subs(sets, nm, fo$last_term)
      }
      if (isTRUE(fo$last_top_op %=% "&") && !is.na(fo$last_term)) {
        sets <- add_subs(sets, fo$last_term, nm)
      }
    }
  }
  
  sets$subsets <- purrr::map(sets$subsets, \(s) {
    if (is.null(s)) {
      NA
    } else {
      s
    }
  })

  names(sets$subsets) <- sets$name
  return(sets)
}
