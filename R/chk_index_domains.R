#' @keywords internal
#' @noRd
.strip_tab_labels <- function(text) {
  stripped <- gsub("#[^#]*#", " ", text)
  return(stripped)
}

#' @importFrom utils relist
#' @keywords internal
#' @noRd
.check_index_domains <- function(extract,
                                 set_extract,
                                 call) {
  if (is.null(extract) || nrow(extract) == 0L || is.null(extract$remainder)) {
    return(invisible(NULL))
  }
  decl <- .declared_arg_sets(extract)
  if (length(decl) == 0L) {
    return(invisible(NULL))
  }
  closure <- .set_superset_closure(set_extract)
  set_case <- set_extract$name
  names(set_case) <- tolower(set_extract$name)

  stmt_rows <- which(tolower(extract$type) %in% c("equation", "formula", "update", "assertion"))
  ref_pattern <- "\\b([A-Za-z_][A-Za-z0-9_@]*)\\s*\\(([^()]*)\\)"

  texts <- .strip_tab_labels(extract$remainder[stmt_rows])
  idx_list <- .stmts_index_sets(texts)

  for (j in seq_along(stmt_rows)) {
    n <- stmt_rows[[j]]
    text <- texts[[j]]
    idx_sets <- idx_list[[j]]
    if (length(idx_sets) == 0L) {
      next
    }
    rebound <- .if_in_rebind(text)
    text <- rebound$text
    idx_sets <- c(idx_sets, rebound$extra)
    refs <- regmatches(text, gregexpr(ref_pattern, text, perl = TRUE))[[1]]
    nm <- tolower(sub("\\s*\\(.*$", "", refs))
    keep <- nm %in% names(decl)
    if (!any(keep)) {
      next
    }
    refs <- refs[keep]
    d <- decl[nm[keep]]
    args <- strsplit(sub("^[^(]*\\((.*)\\)$", "\\1", refs), ",")
    args <- utils::relist(trimws(unlist(args)), args)
    same_len <- lengths(args) == lengths(d)
    if (!any(same_len)) {
      next
    }
    refs <- refs[same_len]
    d <- d[same_len]
    args <- args[same_len]
    ref_at <- rep(seq_along(refs), lengths(args))
    arg_at <- unlist(lapply(lengths(args), seq_len))
    a <- tolower(unlist(args))
    dk <- tolower(unlist(d))
    s <- unname(idx_sets[a])
    s[!grepl("^[a-z_][a-z0-9_@]*$", a)] <- NA_character_
    cand <- which(!is.na(s) & tolower(s) != dk)
    bad <- cand[!vapply(cand, \(i) {
      tolower(s[[i]]) %in% closure[[dk[[i]]]]
    }, logical(1))]
    if (length(bad) == 0L) {
      next
    }
    i <- bad[[1]]
    ref <- refs[[ref_at[[i]]]]
    k <- arg_at[[i]]
    args_k <- args[[ref_at[[i]]]][[k]]
    d_k <- d[[ref_at[[i]]]][[k]]
    s_k <- s[[i]]
    bad_idx <- sub("@in[0-9]+$", "", args_k)
    bad_ref <- gsub("@in[0-9]+", "", gsub("\\s+", "", ref))
    stmt_name <- if (tolower(extract$type[[n]]) %=% "equation") {
      regmatches(text, regexpr("[A-Za-z_][A-Za-z0-9_@]*", text))
    } else {
      NULL
    }
    bad_stmt <- paste(c(extract$type[[n]], stmt_name), collapse = " ")
    bad_set <- if (!is.na(set_case[tolower(s_k)])) {
      set_case[[tolower(s_k)]]
    } else {
      s_k
    }
    decl_set <- if (!is.na(set_case[tolower(d_k)])) {
      set_case[[tolower(d_k)]]
    } else {
      d_k
    }
    .cli_action(model_err$index_not_subset,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
