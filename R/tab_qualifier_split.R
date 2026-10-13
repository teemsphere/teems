#' @keywords internal
#' @noRd
.split_tab_qualifiers <- function(remainder) {
  quals <- rep(NA_character_, length(remainder))
  rest <- trimws(remainder)
  for (i in seq_along(rest)) {
    r <- rest[i]
    groups <- character(0)
    repeat {
      if (!startsWith(r, "(") || grepl("^\\(\\s*all\\s*,", r, ignore.case = TRUE)) {
        break
      }
      close <- .match_bracket(r, 1L)
      if (is.na(close)) {
        break
      }
      groups <- c(groups, substr(r, 1L, close))
      r <- trimws(substring(r, close + 1L))
    }
    if (length(groups) > 0L) {
      quals[i] <- paste(groups, collapse = "")
    }
    rest[i] <- r
  }
  split <- list(qualifier_list = quals, rest = rest)
  return(split)
}

#' @keywords internal
#' @noRd
.split_tab_quantifiers <- function(remainder) {
  quants <- rep(NA_character_, length(remainder))
  rest <- trimws(remainder)
  for (i in seq_along(rest)) {
    r <- rest[i]
    groups <- character(0)
    repeat {
      if (!grepl("^\\(\\s*all\\s*,", r, ignore.case = TRUE)) {
        break
      }
      close <- .match_bracket(r, 1L)
      if (is.na(close)) {
        break
      }
      groups <- c(groups, substr(r, 1L, close))
      r <- trimws(substring(r, close + 1L))
    }
    if (length(groups) > 0L) {
      quants[i] <- gsub("\\s", "", paste(groups, collapse = ""))
    }
    rest[i] <- r
  }
  split <- list(full_set = quants, rest = rest)
  return(split)
}

#' @keywords internal
#' @noRd
.split_tab_sides <- function(definition) {
  comp1 <- definition
  comp2 <- rep(NA_character_, length(definition))
  for (i in seq_along(definition)) {
    d <- definition[i]
    if (!nzchar(d)) {
      next
    }
    scan <- .tab_scan(d)
    n <- length(scan$chs)
    prev <- c("", scan$chs[-n])
    nxt <- c(scan$chs[-1], "")
    eq <- which(scan$chs == "=" & scan$depth_before == 0L & !scan$in_quote &
      !prev %in% c("<", ">", "=") & nxt != "=")
    if (length(eq) == 0L) {
      next
    }
    comp1[i] <- trimws(substr(d, 1L, eq[1] - 1L))
    comp2[i] <- trimws(substring(d, eq[1] + 1L))
  }
  sides <- list(comp1 = comp1, comp2 = comp2)
  return(sides)
}

#' @keywords internal
#' @noRd
.qual_is_parameter <- function(qualifier_list) {
  hit <- !is.na(qualifier_list) &
    grepl("\\bparameter\\b", qualifier_list, ignore.case = TRUE) &
    !grepl("\\bnon_parameter\\b", qualifier_list, ignore.case = TRUE)
  return(hit)
}
