#' @importFrom purrr map_chr map_dbl
#' @keywords internal
#' @noRd
.comp_active_count <- function(model,
                               closure,
                               var_extract,
                               sets,
                               call) {
  comp_stmts <- model$tab[tolower(model$type) == "complementarity"]
  if (length(comp_stmts) == 0L) {
    return(0)
  }
  cls_vars <- tolower(purrr::map_chr(closure, attr, "var_name"))
  n_active <- 0
  for (statement in comp_stmts) {
    cp <- .parse_comp_stmt(statement, call = call)
    v_row <- which(tolower(var_extract$name) == tolower(cp$comp_var))[1]
    if (is.na(v_row)) {
      next
    }
    comp_var <- var_extract$name[v_row]
    var_sets <- var_extract$ls_upper_idx[[v_row]]
    if (var_sets %=% NA || is.null(var_sets)) {
      n_ele <- 1L
    } else {
      n_ele <- prod(lengths(with(sets$ele, mget(var_sets, ifnotfound = ""))))
    }
    entries <- closure[cls_vars == tolower(comp_var)]
    n_exo <- sum(purrr::map_dbl(
      entries,
      \(entry) {
        ele <- attr(entry, "ele")
        if (ele %=% NA || is.null(ele)) {
          return(1)
        }
        nrow(ele)
      }
    ))
    n_active <- n_active + max(n_ele - n_exo, 0)
  }
  return(n_active)
}
