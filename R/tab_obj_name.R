#' @importFrom purrr pluck map_chr map2_chr
#' @keywords internal
#' @noRd
.name_tab_obj <- function(obj) {
  first_enclosure <- purrr::map_chr(
    obj$remainder,
    \(r) {
      purrr::map_chr(strsplit(r, ")"), 1)
    }
  )

  obj$qualifier_list <- ifelse(grepl(
    paste(tab_qual, collapse = "|"),
    first_enclosure,
    ignore.case = TRUE
  ),
  paste0(first_enclosure, ")"),
  NA
  )

  obj$remainder <- .advance_remainder(
    remainder = obj$remainder,
    pattern = obj$qualifier_list
  )

  parsed_remander <- regmatches(obj$remainder,
                                regexec("^((?:\\([^()]*(?:\\([^()]*\\)[^()]*)*\\)\\s*)+)(.*)$",
                                        obj$remainder))

  obj$name <- purrr::map2_chr(
    parsed_remander,
    obj$remainder,
    \(pr, r) {
      if (length(pr) != 0) {
        purrr::pluck(pr, length(pr))
      } else {
        r
      }
    }
  )
  
  obj$remainder <- .advance_remainder(
    remainder = obj$remainder,
    pattern = obj$name
  )
  
  obj$name <- gsub("\\s", "", obj$name)
  return(obj)
}
