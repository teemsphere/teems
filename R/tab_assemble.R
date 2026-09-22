#' @importFrom tibble tibble as_tibble
#' @importFrom purrr pluck
#' @importFrom tools toTitleCase
#' @keywords internal
#' @noRd
.assemble_tab <- function(tab,
                          extract,
                          var_extract,
                          coeff_extract,
                          math_extract,
                          read_extract,
                          mapping_extract) {
  tab <- paste0(tab, ";")

  tab <- tibble::tibble(
    tab = tab,
    row_id = seq_along(tab)
  )

  tab_parsed <- rbind(var_extract, coeff_extract, extract$set, math_extract, read_extract, mapping_extract)
  tab <- tibble::as_tibble(merge(tab_parsed, tab, by = "row_id", all = TRUE))
  tab <- tab[order(tab$row_id), ]

  tab$type <- ifelse(is.na(tab$type),
    purrr::pluck(extract, "model", "type"),
    tab$type
  )

  tab$row_id <- NULL
  tab$type <- tools::toTitleCase(tolower(tab$type))
  tab <- tab[tolower(tab$type) != "write",]
  tab <- tab[!(tolower(tab$type) == "file" & grepl("(new)", tab$tab, ignore.case = TRUE)),]
  return(tab)
}
