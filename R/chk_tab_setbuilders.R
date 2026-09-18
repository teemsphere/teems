#' Conditional set builders: the condition operands must be file-Read
#' or indicators assigned only constants (solver
#' tab_setbuilder_transform fatal; GEMPACK manual 10.1.2)
#'
#' @keywords internal
#' @noRd
.chk_tab_setbuilders <- function(model,
                                 call) {
  typ <- tolower(model$type)
  read_names <- tolower(model$name[typ == "read"])
  byele <- typ == "read" & !is.na(model$qualifier_list) &
    grepl("by_elements", model$qualifier_list, ignore.case = TRUE)
  map_read <- tolower(model$name[byele])
  map_names <- tolower(model$name[typ == "mapping"])
  for (i in which(typ == "set")) {
    d <- model$definition[[i]]
    if (!.is_set_builder(d)) {
      next
    }
    b <- .parse_set_builder(d)
    bad_set <- model$name[i]
    cond_coef <- b$coef
    if (!tolower(b$coef) %in% read_names && is.null(.indicator_formulas(model, b$coef))) {
      .cli_action(model_err$set_builder_noread,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (b$form == "mapsum") {
      cond_map <- b$map
      if (!tolower(b$map) %in% map_names || !tolower(b$map) %in% map_read) {
        .cli_action(model_err$set_builder_nomap,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  return(invisible(NULL))
}
