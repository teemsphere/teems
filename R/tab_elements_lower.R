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

    is_set <- grepl("^set\\b", tab, ignore.case = TRUE)
    is_io <- grepl("^(read|write|file)\\b", tab, ignore.case = TRUE)
    for (nme in unique(upper_ele)) {
      tab[!is_io] <- gsub(paste0("\"", nme, "\""), paste0("\"", tolower(nme), "\""), tab[!is_io], fixed = TRUE)
      tab[is_set] <- gsub(paste0("\\b", nme, "\\b"), tolower(nme), tab[is_set])
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
