#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.cond_logic_ops <- function(cond) {
  scan <- .tab_scan(cond)
  top <- scan$depth_before == 0L & !scan$in_quote
  hits <- gregexpr("(?<![A-Za-z0-9_@.])(and|or|not)(?![A-Za-z0-9_@])", cond, ignore.case = TRUE, perl = TRUE)[[1]]
  if (hits[1] < 0L) {
    cuts <- integer(0)
    return(cuts)
  }
  len <- attr(hits, "match.length")
  keep <- top[hits]
  ops <- stats::setNames(as.integer(hits[keep]), len[keep])
  return(ops)
}

#' @keywords internal
#' @noRd
.cond_leaves <- function(cond) {
  ops <- .cond_logic_ops(cond)
  starts <- c(1L, ops + as.integer(names(ops)))
  ends <- c(ops - 1L, nchar(cond))
  leaves <- trimws(substring(cond, starts, ends))
  leaves <- leaves[nzchar(leaves)]
  repeat {
    wrapped <- grepl("^[[({].*[])}]$", leaves) &
      vapply(leaves, \(l) isTRUE(.match_bracket(l, 1L) == nchar(l)), logical(1))
    if (!any(wrapped)) {
      break
    }
    leaves[wrapped] <- trimws(substr(leaves[wrapped], 2L, nchar(leaves[wrapped]) - 1L))
    inner <- unlist(lapply(leaves, \(l) {
      if (length(.cond_logic_ops(l)) > 0L) .cond_leaves(l) else l
    }))
    leaves <- inner
  }
  return(leaves)
}

#' @importFrom stats setNames
#' @keywords internal
#' @noRd
.is_index_cond <- function(cond,
                           scope,
                           maps) {
  idx_set <- stats::setNames(unname(scope), toupper(names(scope)))
  leaves <- .cond_leaves(cond)
  index_only <- length(leaves) > 0L && all(vapply(leaves, \(l) {
    sp <- .split_comparison(l)
    if (is.null(sp)) {
      return(FALSE)
    }
    a <- .classify(sp$lhs, idx_set, maps)
    b <- .classify(sp$rhs, idx_set, maps)
    !a$kind %=% "expr" && !b$kind %=% "expr" && !(a$kind %=% "elem" && b$kind %=% "elem")
  }, logical(1)))
  return(index_only)
}
