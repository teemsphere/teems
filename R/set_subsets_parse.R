#' @importFrom purrr list_flatten
#' @keywords internal
#' @noRd
.add_subs <- function(sets, set_nm, new, subsets) {
  r <- which(sets$name == set_nm)[1]
  if (is.na(r)) {
    return(sets)
  }
  sets$subsets[r] <- purrr::list_flatten(list(unique(c(sets$subsets[[r]], new))))
  return(sets)
}

#' @importFrom purrr map map_chr
#' @importFrom utils tail
#' @keywords internal
#' @noRd
.parse_set_subsets <- function(sets,
                               extract,
                               expr_info,
                               is_builder,
                               is_set_eq,
                               call) {
  subsets <- extract[tolower(extract$type) %in% "subset",]
  if (any(grepl("\\(\\s*by[_ ]numbers\\s*\\)", subsets$remainder, ignore.case = TRUE))) {
    .cli_action(
      msg = gen_err$subset_by_numbers,
      action = "abort",
      call = call
    )
  }
  subsets$remainder <- trimws(sub("^\\s*\\(\\s*by_elements\\s*\\)\\s*", "", subsets$remainder, ignore.case = TRUE))

  subsets$subset <- purrr::map_chr(subsets$remainder, \(s) {
    strsplit(s, " ")[[1]][1]
  })

  subsets$set <- purrr::map_chr(subsets$remainder, \(s) {
    utils::tail(strsplit(s, " ")[[1]], 1)
  })

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

  for (i in seq_len(nrow(sets))) {
    fo <- expr_info[[i]]
    nm <- sets$name[i]
    if (is_builder[i]) {
      sets <- .add_subs(sets, sets$comp1[i], nm, subsets)
      next
    }
    if (!is.list(fo)) {
      next
    }
    if (is_set_eq[i]) {
      sets <- .add_subs(sets, nm, fo$named[1], subsets)
      sets <- .add_subs(sets, fo$named[1], nm, subsets)
      next
    }
    if (fo$simple_complement) {
      sets <- .add_subs(sets, fo$named[1], c(nm, fo$named[2]), subsets)
    } else if (!is.na(fo$complement_of)) {
      sets <- .add_subs(sets, fo$complement_of, nm, subsets)
    } else if (fo$all_plus_union) {
      sets <- .add_subs(sets, nm, fo$named, subsets)
    } else if (fo$all_intersect) {
      for (tnm in fo$named) sets <- .add_subs(sets, tnm, nm, subsets)
    } else {
      if (isTRUE(fo$last_top_op %=% "^") && !is.na(fo$last_term)) {
        sets <- .add_subs(sets, nm, fo$last_term, subsets)
      }
      if (isTRUE(fo$last_top_op %=% "&") && !is.na(fo$last_term)) {
        sets <- .add_subs(sets, fo$last_term, nm, subsets)
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
