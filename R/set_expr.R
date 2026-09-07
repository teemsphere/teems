#' GEMPACK set expressions (manual 10.1.1.1): named sets, quoted single
#' elements, '(' ')' grouping, and the operators UNION, INTERSECT, '+',
#' '-' and '\' (a synonym of '-'), applied left to right, plus the set
#' product 'x' (manual 10.1.6), normalized to '*'. UNION is normalized
#' to '^' and INTERSECT to '&'. NB keyword normalization is
#' substring-based (matching the solver's parser), so set names must
#' not contain "union" or "intersect", and the standalone token x is
#' always the product operator.
#'
#' @keywords internal
#' @noRd
.set_expr_tokens <- function(d) {
  # set-equality definitions arrive with their leading "=" preserved
  d <- sub("^\\s*=\\s*", "", d)
  d <- gsub("\\\\", "-", d)
  d <- gsub("union", " ^ ", d, ignore.case = TRUE)
  d <- gsub("intersect", " & ", d, ignore.case = TRUE)
  d <- gsub("(?<=[[:space:])])[xX](?=[[:space:](])", " * ", d, perl = TRUE)
  m <- gregexpr('"[^"]*"|[()+^&*-]|[^()+^&*"[:space:]-]+', d)[[1]]
  if (m[1] %=% -1L) {
    return(character(0))
  }
  regmatches(d, list(m))[[1]]
}

#' Does a set definition hold an expression (as opposed to an explicit
#' element list)? Expressions arrive with their leading "=" preserved.
#'
#' @keywords internal
#' @noRd
.is_set_expr <- function(d) {
  !is.na(d) & grepl("^\\s*=", d)
}

#' Structural facts about an expression used for the manual's implied-
#' SUBSET rules: every named term, whether all operators are UNION/'+'
#' (or all INTERSECT), and the last top-level operator and term.
#'
#' @keywords internal
#' @noRd
.set_expr_info <- function(d) {
  toks <- .set_expr_tokens(d)
  is_op <- toks %in% c("+", "-", "^", "&", "*")
  is_paren <- toks %in% c("(", ")")
  is_quote <- grepl('^"', toks)
  named <- toks[!is_op & !is_paren & !is_quote]
  ops <- toks[is_op]
  depth <- cumsum((toks == "(") - (toks == ")"))
  top_op_idx <- which(is_op & depth == 0)
  last_top_op <- if (length(top_op_idx)) toks[max(top_op_idx)] else NA_character_
  last_term <- NA_character_
  if (length(top_op_idx)) {
    after <- toks[seq(max(top_op_idx) + 1L, length(toks))]
    if (length(after) %=% 1L && !after %in% c("(", ")") && !grepl('^"', after)) {
      last_term <- after
    }
  }
  # complement of any shape (manual 11.7): a named first term followed
  # only by top-level '-' operators makes the result a subset of it
  top_ops <- toks[is_op & depth == 0]
  complement_of <- NA_character_
  if (length(top_ops) > 0L && all(top_ops %=% "-") && length(toks) > 0L &&
    !toks[1] %in% c("(", ")") && !grepl('^"', toks[1])) {
    complement_of <- toks[1]
  }
  list(
    named = named,
    ops = ops,
    all_plus_union = length(ops) > 0 && all(ops %in% c("+", "^")),
    all_intersect = length(ops) > 0 && all(ops %=% "&"),
    simple_complement = length(ops) %=% 1L && ops[1] %=% "-" &&
      length(named) %=% 2L && !any(is_paren) && !any(is_quote),
    complement_of = complement_of,
    last_top_op = last_top_op,
    last_term = last_term
  )
}

#' Evaluate a set expression against resolved mappings (data.tables with
#' origin/mapping columns). Returns NULL when a referenced set is not
#' resolved yet (the caller's fixed-point loop retries). Validity per
#' the manual, at ELEMENT level throughout: '+' operands must be
#' disjoint; '-' may only remove elements that are present; '&' keeps
#' the accumulator's rows and order (manual 10.1.1: "elements are
#' ordered as in <set1>"). A shared element whose origin coverage
#' disagrees between '&' operands is recorded on the result as the
#' "origin_conflict" attribute rather than aborting: origins are teems
#' aggregation bookkeeping with no GEMPACK counterpart and are
#' meaningless for loop domains (e.g. the IF-rewrite's synthetic
#' intersections); the consumers that do read origin rows (the
#' by_elements mapping compose, .finalize_map_data) abort on the stamp
#' at the point of use.
#'
#' @importFrom data.table data.table funion fsetdiff fintersect setattr
#'
#' @keywords internal
#' @noRd
.eval_set_expr <- function(d, mappings, owner, call) {
  if (length(d) %!=% 1L || is.na(d)) {
    return(NULL)
  }
  toks <- .set_expr_tokens(d)
  pos <- 1L
  ready <- TRUE
  conflicts <- character(0)
  # name of the set a term denotes ("" for a quoted element or a
  # parenthesized subexpression): the product naming rule prefixes
  # truncated element names with the factor set's first letter
  term_name <- ""

  peek <- function() {
    if (pos <= length(toks)) toks[pos] else NA_character_
  }

  term <- function() {
    tk <- peek()
    term_name <<- ""
    if (is.na(tk)) {
      return(NULL)
    }
    if (tk %=% "(") {
      pos <<- pos + 1L
      v <- expr()
      if (isTRUE(peek() %=% ")")) pos <<- pos + 1L
      term_name <<- ""
      return(v)
    }
    pos <<- pos + 1L
    if (grepl('^"', tk)) {
      el <- tolower(gsub('"', "", tk))
      return(data.table::data.table(
        origin = el,
        mapping = el,
        key = c("origin", "mapping")
      ))
    }
    term_name <<- tk
    v <- mappings[[tk]]
    if (is.null(v)) {
      ready <<- FALSE
    } else {
      # a stamped operand taints the expression; the stamp is trimmed
      # to the surviving elements at the end
      conflicts <<- unique(c(conflicts, attr(v, "origin_conflict")))
    }
    v
  }

  expr <- function() {
    acc <- term()
    acc_name <- term_name
    while (isTRUE(peek() %in% c("+", "-", "^", "&", "*"))) {
      op <- peek()
      pos <<- pos + 1L
      rhs <- term()
      rhs_name <- term_name
      if (!ready || is.null(acc) || is.null(rhs)) {
        ready <<- FALSE
        return(NULL)
      }
      if (op %=% "*") {
        # set product (manual 10.1.6/11.7.11): first factor fastest;
        # product elements have no data origin of their own
        nms <- .set_product_names(
          a = unique(acc$mapping), nm1 = acc_name,
          b = unique(rhs$mapping), nm2 = rhs_name,
          owner = owner, call = call
        )
        # unkeyed: a key would sort the rows and lose the product order,
        # which is the element order the solver builds
        acc <- data.table::data.table(
          origin = nms,
          mapping = nms
        )
        acc_name <- ""
        next
      }
      acc_name <- ""
      if (op %=% "+") {
        # disjointness is an element-level requirement (manual
        # 10.1.1.1): a shared element with disjoint origin rows used
        # to slip past the row-level overlap test
        d <- intersect(unique(acc$mapping), unique(rhs$mapping))
        if (length(d) %!=% 0L) {
          .cli_action(deploy_err$invalid_plus,
            action = "abort",
            call = call
          )
        }
        acc <- data.table::funion(acc, rhs)
      } else if (op %=% "-") {
        # GEMPACK set operations act element-level on the (aggregated)
        # sets (manual 10.1.1.1): a subtracted element disappears
        # entirely, including every origin row that maps to it. A
        # row-level fsetdiff kept an aggregated element whenever any
        # origin outside the subtrahend mapped to it (e.g. NMRG =
        # COMM - MARG retained the margin commodity).
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
      } else if (op %=% "^") {
        acc <- data.table::funion(acc, rhs)
      } else {
        # element-level intersection (manual 10.1.1/11.7.3): the
        # elements in both operands, keeping the accumulator's rows
        # and order ("elements are ordered as in <set1>"). Disagreeing
        # origin coverage for a shared element is recorded, not fatal:
        # only consumers that read origin rows can be harmed and they
        # check the stamp at the point of use
        shared <- intersect(unique(acc$mapping), unique(rhs$mapping))
        acc_sh <- acc[acc$mapping %in% shared, ]
        if (length(shared) %!=% 0L) {
          rhs_sh <- rhs[rhs$mapping %in% shared, ]
          acc_or <- lapply(split(acc_sh$origin, acc_sh$mapping), unique)
          rhs_or <- lapply(split(rhs_sh$origin, rhs_sh$mapping), unique)
          agree <- mapply(setequal, acc_or, rhs_or[names(acc_or)])
          conflicts <<- unique(c(conflicts, names(acc_or)[!agree]))
        }
        acc <- acc_sh
      }
    }
    acc
  }

  out <- expr()
  if (!ready) {
    return(NULL)
  }
  if (!is.null(out)) {
    # trim to surviving elements; data.table subsetting copies custom
    # attributes through, so a stale operand stamp must be cleared
    conflicts <- intersect(conflicts, unique(out$mapping))
    data.table::setattr(
      out,
      "origin_conflict",
      if (length(conflicts) > 0L) conflicts else NULL
    )
  }
  out
}

#' Element names of SET3 = SET1 x SET2 (GEMPACK manual 11.7.11): xx_yyy
#' with the first factor varying fastest. When the longest names would
#' exceed the 12-character element limit, the elements of a factor are
#' truncated to "<first letter of the factor set><element number><leading
#' characters that fit>": both factors to 6 and 5 characters when both
#' are long, otherwise the long one to 11 minus the short one's length.
#' Mirrors set_product_names() in the solver (tab_parse.c) exactly.
#'
#' @keywords internal
#' @noRd
.set_product_names <- function(a, nm1, b, nm2, owner, call) {
  mx1 <- max(nchar(a), 0L)
  mx2 <- max(nchar(b), 0L)
  lim1 <- 0L
  lim2 <- 0L
  if (mx1 + mx2 > 11L) {
    if (mx1 <= 5L) {
      lim2 <- 11L - mx1
    } else if (mx2 <= 5L) {
      lim1 <- 11L - mx2
    } else {
      lim1 <- 6L
      lim2 <- 5L
    }
  }
  letter <- function(nm) {
    if (nzchar(nm)) tolower(substr(nm, 1L, 1L)) else tolower(substr(owner, 1L, 1L))
  }
  trunc_names <- function(x, nm, lim) {
    if (lim == 0L) {
      return(x)
    }
    pre <- paste0(letter(nm), seq_along(x))
    room <- pmax(lim - nchar(pre), 0L)
    paste0(pre, substr(x, 1L, room))
  }
  e1 <- trunc_names(a, nm1, lim1)
  e2 <- trunc_names(b, nm2, lim2)
  # element of the first factor varies fastest
  out <- as.vector(outer(e1, e2, function(x, y) paste0(x, "_", y)))
  if (anyDuplicated(out)) {
    bad_set <- owner
    dup_ele <- out[duplicated(out)][1]
    .cli_action(model_err$set_product_dup,
      action = "abort",
      call = call
    )
  }
  out
}
