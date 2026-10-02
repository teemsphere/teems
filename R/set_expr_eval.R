#' @keywords internal
#' @noRd
.is_set_expr <- function(d) {
  return(!is.na(d) & grepl("^\\s*=", d))
}

#' @keywords internal
#' @noRd
.peek <- function(st) {
  if (st$pos <= length(st$toks)) {
    return(st$toks[st$pos])
  }
  return(NA_character_)
}

#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
.term <- function(st, mappings, owner, call) {
  tk <- .peek(st)
  st$term_name <- ""
  if (is.na(tk)) {
    return(NULL)
  }
  if (tk %=% "(") {
    st$pos <- st$pos + 1L
    v <- .eval_expr(st, mappings, owner, call)
    if (isTRUE(.peek(st) %=% ")")) {
      st$pos <- st$pos + 1L
    }
    st$term_name <- ""
    return(v)
  }
  st$pos <- st$pos + 1L
  if (grepl('^"', tk)) {
    el <- tolower(gsub('"', "", tk))
    dt <- data.table::data.table(
      origin = el,
      mapping = el,
      key = c("origin", "mapping")
    )
    return(dt)
  }
  st$term_name <- tk
  v <- mappings[[tk]]
  if (is.null(v)) {
    st$ready <- FALSE
  } else {
    st$conflicts <- unique(c(st$conflicts, attr(v, "origin_conflict")))
  }
  return(v)
}

#' @importFrom data.table data.table funion
#' @keywords internal
#' @noRd
.eval_expr <- function(st, mappings, owner, call) {
  acc <- .term(st, mappings, owner, call)
  acc_name <- st$term_name
  while (isTRUE(.peek(st) %in% c("+", "-", "%", "^", "&", "*"))) {
    op <- .peek(st)
    st$pos <- st$pos + 1L
    rhs <- .term(st, mappings, owner, call)
    rhs_name <- st$term_name
    if (!st$ready || is.null(acc) || is.null(rhs)) {
      st$ready <- FALSE
      return(NULL)
    }
    if (op %=% "*") {
      nms <- .set_product_names(
        a = unique(acc$mapping), nm1 = acc_name,
        b = unique(rhs$mapping), nm2 = rhs_name,
        owner = owner, call = call
      )
      acc <- data.table::data.table(
        origin = nms,
        mapping = nms
      )
      acc_name <- ""
      next
    }
    acc_name <- ""
    if (op %=% "+") {
      d <- intersect(unique(acc$mapping), unique(rhs$mapping))
      if (length(d) %!=% 0L) {
        .cli_action(deploy_err$invalid_plus,
          action = "abort",
          call = call
        )
      }
      acc <- data.table::funion(acc, rhs)
    } else if (op %=% "-") {
      rhs_ele <- unique(rhs$mapping)
      missing_ele <- setdiff(rhs_ele, unique(acc$mapping))
      if (length(missing_ele) %!=% 0L) {
        d <- missing_ele
        .cli_action(deploy_err$invalid_minus,
          action = "abort",
          call = call
        )
      }
      acc <- acc[!acc$mapping %in% rhs_ele, ]
    } else if (op %=% "%") {
      acc <- acc[!acc$mapping %in% unique(rhs$mapping), ]
    } else if (op %=% "^") {
      acc <- data.table::funion(acc, rhs)
    } else {
      shared <- intersect(unique(acc$mapping), unique(rhs$mapping))
      acc_sh <- acc[acc$mapping %in% shared, ]
      if (length(shared) %!=% 0L) {
        rhs_sh <- rhs[rhs$mapping %in% shared, ]
        acc_or <- lapply(split(acc_sh$origin, acc_sh$mapping), unique)
        rhs_or <- lapply(split(rhs_sh$origin, rhs_sh$mapping), unique)
        agree <- mapply(setequal, acc_or, rhs_or[names(acc_or)])
        st$conflicts <- unique(c(st$conflicts, names(acc_or)[!agree]))
      }
      acc <- acc_sh
    }
  }
  return(acc)
}

#' @importFrom data.table setattr
#' @keywords internal
#' @noRd
.eval_set_expr <- function(d, mappings, owner, call) {
  if (length(d) %!=% 1L || is.na(d)) {
    return(NULL)
  }
  st <- new.env(parent = emptyenv())
  st$toks <- .set_expr_tokens(d)
  st$pos <- 1L
  st$ready <- TRUE
  st$conflicts <- character(0)
  st$term_name <- ""

  out <- .eval_expr(st, mappings, owner, call)
  if (!st$ready) {
    return(NULL)
  }
  if (!is.null(out)) {
    conflicts <- intersect(st$conflicts, unique(out$mapping))
    data.table::setattr(
      out,
      "origin_conflict",
      if (length(conflicts) > 0L) {
        conflicts
      } else {
        NULL
      }
    )
  }
  return(out)
}
