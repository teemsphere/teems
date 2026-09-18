# In-TAB OMIT / SUBSTITUTE / BACKSOLVE statements (GEMPACK manual 10.16).
#' @keywords internal
#' @noRd
.parse_intab_cndns <- function(tab) {
  first <- tolower(purrr::map_chr(strsplit(trimws(tab), "\\s+"), 1))
  rows <- which(first %in% c("omit", "substitute", "backsolve"))
  actions <- list()

  for (r in rows) {
    words <- strsplit(trimws(tab[[r]]), "\\s+")[[1]]
    kind <- tolower(words[[1]])
    if (kind %=% "omit") {
      for (v in words[-1]) {
        actions[[length(actions) + 1L]] <- list(
          action = "omit",
          var = v,
          eq = NA_character_,
          substitute = FALSE
        )
      }
    } else {
      using_at <- which(tolower(words) == "using")
      var <- words[[2]]
      eq <- if (length(using_at) == 1L && using_at == 3L && length(words) >= 4L) {
        words[[4]]
      } else {
        NA_character_
      }
      actions[[length(actions) + 1L]] <- list(
        action = "backsolve",
        var = var,
        eq = eq,
        substitute = kind %=% "substitute"
      )
    }
  }

  condense <- list(rows = rows, actions = actions)
  return(condense)
}
