# GEMPACK set expressions (manual 10.1.1.1): named sets, quoted single
# elements, '(' ')' grouping, and the operators UNION, INTERSECT, '+',
# '-' and '\' (a synonym of '-'), applied left to right, plus the set
# product 'x' (manual 10.1.6), normalized to '*'. UNION is normalized
# to '^' and INTERSECT to '&'. NB keyword normalization is
# substring-based (matching the solver's parser), so set names must
# not contain "union" or "intersect", and the standalone token x is
# always the product operator.

#' Does a set definition hold an expression (as opposed to an explicit
#' element list)? Expressions arrive with their leading "=" preserved.
#'
#' @keywords internal
#' @noRd
.is_set_expr <- function(d) {
  return(!is.na(d) & grepl("^\\s*=", d))
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
#' @importFrom data.table data.table funion setattr
#'
#' @keywords internal
#' @noRd
.eval_set_expr <- function(d, mappings, owner, call) {
  if (length(d) %!=% 1L || is.na(d)) {
    return(NULL)
  }
  # the recursive descent carries its state in an explicit environment
  # (as .parse_linear_side does), threaded through every helper, rather
  # than mutating this frame from the closures
  st <- new.env(parent = emptyenv())
  st$toks <- .set_expr_tokens(d)
  st$pos <- 1L
  st$ready <- TRUE
  st$conflicts <- character(0)
  # name of the set a term denotes ("" for a quoted element or a
  # parenthesized subexpression): the product naming rule prefixes
  # truncated element names with the factor set's first letter
  st$term_name <- ""

  peek <- function(st) {
    if (st$pos <= length(st$toks)) {
      return(st$toks[st$pos])
    }
    return(NA_character_)
  }

  term <- function(st) {
    tk <- peek(st)
    st$term_name <- ""
    if (is.na(tk)) {
      return(NULL)
    }
    if (tk %=% "(") {
      st$pos <- st$pos + 1L
      v <- expr(st)
      if (isTRUE(peek(st) %=% ")")) {
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
      # a stamped operand taints the expression; the stamp is trimmed
      # to the surviving elements at the end
      st$conflicts <- unique(c(st$conflicts, attr(v, "origin_conflict")))
    }
    return(v)
  }

  expr <- function(st) {
    acc <- term(st)
    acc_name <- st$term_name
    while (isTRUE(peek(st) %in% c("+", "-", "^", "&", "*"))) {
      op <- peek(st)
      st$pos <- st$pos + 1L
      rhs <- term(st)
      rhs_name <- st$term_name
      if (!st$ready || is.null(acc) || is.null(rhs)) {
        st$ready <- FALSE
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
          st$conflicts <- unique(c(st$conflicts, names(acc_or)[!agree]))
        }
        acc <- acc_sh
      }
    }
    return(acc)
  }

  out <- expr(st)
  if (!st$ready) {
    return(NULL)
  }
  if (!is.null(out)) {
    # trim to surviving elements; data.table subsetting copies custom
    # attributes through, so a stale operand stamp must be cleared
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
