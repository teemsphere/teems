#' @importFrom data.table data.table rbindlist
#' @importFrom purrr map_chr
#'
#' @noRd
#' @keywords internal
.infer_set_mappings <- function(set_mappings, set_data) {
  
  set_names <- vapply(set_data, \(s) {
    nm <- class(s)[2]
    if (nm %=% "set") NA_character_ else nm
  }, character(1))

  sets <- set_data[!is.na(set_names)]
  names(sets) <- purrr::map_chr(sets, \(s) {
    class(s)[[2]]
  })

  provided_names <- names(set_mappings)
  unspecified <- setdiff(names(sets), provided_names)

  if (length(unspecified) %=% 0L) {
    return(set_mappings)
  }

  provided_elements <- lapply(set_mappings, \(m) tolower(m[[1]]))

  for (set_name in unspecified) {
    set_ele <- tolower(as.character(sets[[set_name]]))

    parent <- NULL
    for (p in provided_names) {
      if (all(set_ele %in% provided_elements[[p]])) {
        parent <- p
        break
      }
    }

    if (!is.null(parent)) {
      parent_map <- set_mappings[[parent]]
      derived <- parent_map[tolower(parent_map[[1]]) %in% set_ele, ]
      colnames(derived)[1] <- set_name
      set_mappings[[set_name]] <- derived
      next
    }

    parts <- list()
    covered <- character(0)

    for (p in provided_names) {
      matching <- set_ele[set_ele %in% provided_elements[[p]]]
      if (length(matching) > 0L) {
        parent_map <- set_mappings[[p]]
        part <- parent_map[tolower(parent_map[[1]]) %in% matching, ]
        colnames(part)[1] <- set_name
        parts <- c(parts, list(part))
        covered <- c(covered, matching)
      }
    }

    uncovered <- setdiff(set_ele, covered)
    if (length(uncovered) > 0L) {
      identity <- data.table::data.table(x = uncovered, mapping = uncovered)
      colnames(identity)[1] <- set_name
      parts <- c(parts, list(identity))
    }

    if (length(parts) > 0L) {
      set_mappings[[set_name]] <- data.table::rbindlist(parts, use.names = FALSE)
    } else {
      identity <- data.table::data.table(x = set_ele, mapping = set_ele)
      colnames(identity)[1] <- set_name
      set_mappings[[set_name]] <- identity
    }
  }

  return(set_mappings)
}