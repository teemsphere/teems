#' @importFrom purrr map2 pmap map map_chr pluck
#' 
#' @keywords internal
#' @note Replace all sapply with purrr::map
#' @noRd
.parse_tab_maths <- function(tab_extract,
                             call) {
  maths <- subset(tab_extract, tolower(type) %in% c("equation", "formula"))

  maths$name <- unlist(purrr::map2(
    maths$type,
    maths$remainder,
    function(t, r) {
      if (t %=% "Equation") {
        lapply(strsplit(r, " "), "[[", 1)
      } else {
        NA
      }
    }
  ))

  maths$remainder <- unlist(purrr::map2(
    maths$remainder,
    maths$name,
    function(r, n) {
      if (!is.na(n)) {
        return(trimws(sub(n, "", r, fixed = TRUE)))
      } else {
        return(r)
      }
    }
  ))

  maths$label <- sapply(
    strsplit(
      maths$remainder,
      split = "#"
    ),
    FUN = function(spl) {
      if (length(spl) > 1) {
        trimws(spl[2])
      } else {
        NA
      }
    }
  )

  maths$remainder <- unlist(purrr::pmap(
    list(
      maths$remainder,
      maths$label,
      maths$type
    ),
    function(rem, info, t) {
      if (t %=% "Equation") {
        if (grepl(pattern = "#", rem)) {
          rem <- strsplit(rem, "#")[[1]][3]
          trimws(rem)
        } else {
          return(rem)
        }
      } else {
        return(rem)
      }
    }
  ))

  qual <- c(
    "nonzero_by_zero",
    "zero_by_zero",
    "\\bchange\\b",
    "linear",
    "orig_level",
    "ge [[:digit:]]",
    "gt [[:digit:]]",
    "integer",
    "parameter",
    "initial",
    "\\breal\\b",
    "levels"
  )

  first_enclosure <- lapply(strsplit(maths$remainder, ")"), "[[", 1)

  maths$qualifier_list <- ifelse(grepl(paste(qual, collapse = "|"), first_enclosure, ignore.case = TRUE),
    paste0(first_enclosure, ")"),
    NA
  )

  maths$remainder <- .advance_remainder(
    remainder = maths$remainder,
    pattern = maths$qualifier_list
  )

  maths$full_set <- purrr::map_chr(maths$remainder, function(r) {
    return_comp <- regmatches(
      r,
      regexec(
        "^((?:\\([^()]*(?:\\([^()]*\\)[^()]*)*\\)\\s*)+)(.*)$",
        r
      )
    )
    return_comp <- purrr::pluck(return_comp, 1, 2)
    if (is.null(return_comp)) {
      return_comp <- NA
    } else {
      return_comp <- gsub("\\s", "", trimws(return_comp))
    }
    return(return_comp)
  })

  maths$math <- purrr::map_chr(maths$remainder, function(r) {
    return_comp <- regmatches(
      r,
      regexec(
        "^((?:\\([^()]*(?:\\([^()]*\\)[^()]*)*\\)\\s*)+)(.*)$",
        r
      )
    )
    return_comp <- purrr::pluck(return_comp, 1, 3)

    if (is.null(return_comp)) {
      return_comp <- r
    } else {
      return_comp <- trimws(return_comp)
    }
    return(return_comp)
  })


  maths$LHS <- trimws(purrr::map_chr(purrr::map(maths$math, strsplit, split = "="), purrr::pluck, 1, 1))
  maths$RHS <- trimws(purrr::map_chr(purrr::map(maths$math, strsplit, split = "="), purrr::pluck, 1, 2))

  return(maths)
}
