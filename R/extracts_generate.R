#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.generate_extracts <- function(tab,
                               call) {
  extract <- tibble::tibble(
    type = purrr::map_chr(strsplit(tab, split = " ", perl = TRUE), 1),
    remainder = sub("^\\S+\\s*", "", tab)
  )

  extract$row_id <- seq_len(nrow(extract))

  set_extract <- .parse_tab_sets(
    extract = extract,
    call = call
  )

  extract <- list(model = extract, set = set_extract)
  return(extract)
}