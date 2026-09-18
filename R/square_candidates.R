#' Candidate variables whose element counts could close the squaring
#' gap: exact matches first, nearest counts otherwise
#'
#' @keywords internal
#' @noRd
.square_candidates <- function(gap,
                               var_extract,
                               sets,
                               closure) {
  nelem <- vapply(
    var_extract$ls_upper_idx,
    \(var_sets) {
      if (var_sets %=% NA) {
        return(1)
      }
      prod(lengths(with(sets$ele, mget(var_sets, ifnotfound = ""))))
    },
    numeric(1)
  )
  names(nelem) <- var_extract$name

  exo_count <- rep(0, length(nelem))
  names(exo_count) <- tolower(var_extract$name)
  for (entry in closure) {
    vn <- tolower(attr(entry, "var_name"))
    ele <- attr(entry, "ele")
    n <- if (ele %=% NA) {
      1
    } else {
      nrow(ele)
    }
    if (vn %in% names(exo_count)) {
      exo_count[vn] <- exo_count[vn] + n
    }
  }

  if (gap > 0) {
    # need more exogenous elements: how many endogenous elements each
    # variable still has
    avail <- nelem - exo_count[tolower(names(nelem))]
    verb <- "exogenizing"
  } else {
    # too many exogenous: what each variable currently contributes
    avail <- exo_count[tolower(names(nelem))]
    names(avail) <- names(nelem)
    verb <- "endogenizing"
  }
  avail <- avail[avail > 0]
  if (length(avail) == 0L) {
    return("No single-variable candidate closes the gap.")
  }
  exact <- avail[avail == abs(gap)]
  if (length(exact) > 0L) {
    picks <- utils::head(names(exact), 5L)
    msg <- paste0(
      "Candidates: ", verb, " ", abs(gap),
      " element", if (abs(gap) != 1) {
        "s"
      }, " of one of ",
      paste(picks, collapse = ", "), " closes the gap exactly."
    )
    return(msg)
  }
  near <- utils::head(names(avail)[order(abs(avail - abs(gap)))], 5L)
  msg <- paste0(
    "No single variable matches the gap exactly; nearest by element ",
    "count: ",
    paste(paste0(near, " (", avail[near], ")"), collapse = ", "), "."
  )
  return(msg)
}
