# file and header of every Read statement, keyed by the name it reads
# into
#' @keywords internal
#' @noRd
.parse_read_targets <- function(r) {
  r$name <- purrr::map_chr(strsplit(r$remainder, " "), 1)

  r$remainder <- gsub("from file",
                      "from file",
                      r$remainder,
                      ignore.case = TRUE)

  r$remainder <- .advance_remainder(
    remainder = r$remainder,
    pattern = paste(r$name, "from file")
  )

  r$file <- .get_element(input = r$remainder, split = " ", index = 1)
  r$header <- gsub(
    pattern = "\"",
    replacement = "",
    x = .get_element(input = r$remainder, split = " ", index = 3)
  )
  return(r)
}
