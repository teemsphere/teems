#' @importFrom purrr map_lgl
#' @keywords internal
#' @noRd
.lower_tab_elements <- function(tab,
                                extract,
                                call) {
  ele_names <- extract$set[with(extract$set,
    expr = {is.na(header) &
            qualifier_list == "(non_intertemporal)" &
            is.na(comp1) &
            is.na(comp2)}
    ), ]$definition

  if (any(purrr::map_lgl(
    ele_names,
    \(e) {
      any(tolower(e) != e)
    }
  ))) {
    upper_ele <- unlist(ele_names[tolower(ele_names) != ele_names])

    for (nme in unique(upper_ele)) {
      pattern <- paste0("\\b", nme, "\\b")
      tab <- gsub(pattern, tolower(nme), tab)
    }

    extract <- .generate_extracts(
      tab = tab,
      call = call
    )
  }

  if (any(grepl("\"CGDS\"", tab, ignore.case = TRUE))) {
    tab <- gsub("\"CGDS\"", "\"cgds\"", tab, ignore.case = TRUE)
    extract <- .generate_extracts(
      tab = tab,
      call = call
    )
  }
  lowered <- list(tab = tab, extract = extract)
  return(lowered)
}
