# the definition text reduced to what each kind of definition needs:
# an expression keeps its operators, an equality and a builder keep
# their "=", and an explicit list becomes a character vector
#' @keywords internal
#' @noRd
.normalize_set_defs <- function(sets,
                                is_builder,
                                is_set_eq) {
  is_expr <- .is_set_expr(sets$definition) &
    sets$qualifier_list != "(intertemporal)" & !is_builder
  sets$definition <- ifelse(is_expr & !is_set_eq,
    trimws(sub("^\\s*=\\s*", "", sets$definition)),
    ifelse(is_set_eq | is_builder,
      trimws(sets$definition),
      trimws(gsub("\\(|=|\\)", "", sets$definition))
    )
  )
  sets$definition <- ifelse(!is_expr & !is_builder & grepl(",", sets$definition),
    strsplit(sets$definition, ","),
    sets$definition
  )
  sets$definition <- lapply(sets$definition, trimws)
  names(sets$definition) <- sets$name
  normalized <- list(sets = sets, is_expr = is_expr)
  return(normalized)
}
