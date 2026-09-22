#' @keywords internal
#' @noRd
.chk_set_ele_lists <- function(sets,
                               call) {
  is_ele_list <- !is.na(sets$definition) &
    !grepl("^\\s*=", sets$definition) &
    tolower(sets$qualifier_list) != "(intertemporal)" &
    is.na(sets$full_read)
  for (i in which(is_ele_list)) {
    bad_set <- sets$name[i]
    inner <- sub("^\\s*\\(", "", sub("\\)\\s*$", "", trimws(sets$definition[i])))
    bad_def <- trimws(sets$definition[i])
    eles <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
    ranged <- grepl("-", eles, fixed = TRUE)
    if (any(ranged)) {
      bad_ele <- eles[ranged][1]
      .cli_action(model_err$set_ele_range,
        action = c("abort", "inform"),
        call = call
      )
    }
    collapsed <- gsub("[[:space:]]", "", inner)
    empty <- !nzchar(collapsed) || grepl("^,|,,|,$", collapsed)
    malformed <- any(grepl("[[:space:]]", eles))
    if (empty || malformed) {
      empty_or_malformed <- if (empty) {
        "empty"
      } else {
        "malformed"
      }
      .cli_action(model_err$set_ele_list,
        action = "abort",
        call = call
      )
    }
  }
  return(invisible(NULL))
}
