#' @keywords internal
#' @noRd
.chk_shk_dup_tup <- function(shock,
                             call) {
  keys <- .shk_input_tuples(shock$input)
  dup <- duplicated(keys)
  if (any(dup)) {
    var_name <- shock$var
    dup_tuples <- .shk_tuple_labels(unique(keys[dup]))
    .cli_action(shk_err$cust_dup_tup,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}

#' @importFrom utils head
#' @keywords internal
#' @noRd
.shk_tuple_labels <- function(tuples,
                              n = 5L) {
  tuples <- utils::head(tuples, n)
  paste0("(", do.call(paste, c(as.list(tuples), sep = ",")), ")")
}

#' @importFrom data.table as.data.table data.table setnames
#' @keywords internal
#' @noRd
.shk_input_tuples <- function(input) {
  key_cols <- setdiff(colnames(input), "Value")
  if (length(key_cols) %=% 0L) {
    blank <- data.table::data.table(V1 = rep("", nrow(input)))
    return(blank)
  }
  keys <- data.table::as.data.table(lapply(input[, key_cols, with = FALSE], \(k) tolower(as.character(k))))
  data.table::setnames(keys, paste0("V", seq_along(keys)))
}

#' @importFrom data.table CJ data.table
#' @keywords internal
#' @noRd
.shk_uniform_tuples <- function(shk,
                                sets) {
  ls_upper <- shk$ls_upper
  if (length(ls_upper) %=% 0L || anyNA(ls_upper) || ls_upper %=% "null_set") {
    blank <- data.table::data.table(V1 = "")
    return(blank)
  }
  dims <- lapply(seq_along(ls_upper), \(i) {
    ss <- shk$subset[[shk$ls_mixed[[i]]]]
    if (is.null(ss)) {
      sets$ele[[ls_upper[[i]]]]
    } else if (isTRUE(attr(ss, "subset"))) {
      unlist(lapply(ss, \(s) sets$ele[[s]]))
    } else {
      as.character(ss)
    }
  })
  if (any(vapply(dims, is.null, logical(1L)))) {
    return(NULL)
  }
  names(dims) <- paste0("V", seq_along(dims))
  do.call(data.table::CJ, c(lapply(dims, tolower), sorted = FALSE, unique = TRUE))
}

#' @importFrom data.table rbindlist
#' @keywords internal
#' @noRd
.chk_shk_repeat <- function(shocks,
                            tuples) {
  vars <- tolower(vapply(shocks, \(s) s$var, character(1L)))
  rep_vars <- unique(vars[duplicated(vars)])
  for (v in rep_vars) {
    idx <- which(vars == v)
    v_tuples <- tuples[idx]
    if (any(vapply(v_tuples, is.null, logical(1L)))) {
      next
    }
    all_tuples <- data.table::rbindlist(v_tuples)
    dup <- duplicated(all_tuples)
    if (any(dup)) {
      var_name <- shocks[[idx[[1L]]]]$var
      dup_tuples <- .shk_tuple_labels(unique(all_tuples[dup]))
      n_shocks <- length(idx)
      call <- attr(shocks[[idx[[length(idx)]]]], "call")
      .cli_action(shk_err$repeat_shock,
        action = c("abort", "inform", "inform"),
        call = call
      )
    }
  }
  return(invisible(NULL))
}
