#' @importFrom purrr map2 pmap map_chr pluck
#' @keywords internal
#' @noRd
.parse_tab_maths <- function(extract,
                             call) {

  maths <- extract[tolower(extract$type) %in% c("equation", "formula"),]

  eq_qual <- rep(NA_character_, nrow(maths))
  lead_grp <- tolower(maths$type) == "equation" &
    grepl("^\\s*\\((?!\\s*all\\b)", maths$remainder, perl = TRUE, ignore.case = TRUE)
  eq_qual[lead_grp] <- sub(
    "^\\s*(\\([^)]*\\)).*$", "\\1",
    maths$remainder[lead_grp]
  )
  maths$remainder[lead_grp] <- trimws(sub(
    "^\\s*\\([^)]*\\)", "",
    maths$remainder[lead_grp]
  ))

  maths$name <- unlist(purrr::map2(
    maths$type,
    maths$remainder,
    \(t, r) {
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
    \(r, n) {
      if (!is.na(n)) {
        maths <- trimws(sub(n, "", r, fixed = TRUE))
        return(maths)
      } else {
        return(r)
      }
    }
  ))

  maths$label <- purrr::map_chr(
    strsplit(maths$remainder, split = "#"),
    \(spl) if (length(spl) > 1) {
      trimws(spl[2])
    } else {
      NA_character_
    }
  )

  maths$remainder <- unlist(purrr::pmap(
    list(
      maths$remainder,
      maths$label,
      maths$type
    ),
    \(rem, info, t) {
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

  maths$qualifier_list[!is.na(eq_qual)] <- eq_qual[!is.na(eq_qual)]

  maths$remainder <- .advance_remainder(
    remainder = maths$remainder,
    pattern = maths$qualifier_list
  )

  maths$full_set <- purrr::map_chr(maths$remainder, \(r) {
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

  maths$definition <- purrr::map_chr(maths$remainder, \(r) {
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

  def_split <- strsplit(maths$definition, split = "=")
  maths$comp1 <- trimws(purrr::map_chr(def_split, 1))
  maths$comp2 <- trimws(purrr::map_chr(def_split, \(s) {
    if (length(s) < 2L) {
      return(NA_character_)
    }
    paste(s[-1], collapse = "=")
  }))

  maths$ls_upper_idx <- NA
  maths$ls_mixed_idx <- NA
  maths$header <- NA
  maths$file <- NA
  maths$subsets <- NA

  maths <- maths[, c("type",
                     "name",
                     "label",
                     "qualifier_list",
                     "ls_upper_idx",
                     "ls_mixed_idx",
                     "header",
                     "file",
                     "definition",
                     "subsets",
                     "comp1",
                     "comp2",
                     "row_id")]
  
  return(maths)
}
