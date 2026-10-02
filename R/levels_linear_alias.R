#' @keywords internal
#' @noRd
.levels_linear_alias <- function(names,
                                 var_extract) {
  lv <- .levels_linear_names(var_extract)
  to_decl <- lv$kind != "var"
  alias <- c(lv$linear[to_decl], lv$decl[!to_decl])
  target <- c(lv$decl[to_decl], lv$linear[!to_decl])
  hit <- match(tolower(names), tolower(alias))
  names[!is.na(hit)] <- target[hit[!is.na(hit)]]
  declared <- match(tolower(names), tolower(var_extract$name))
  names[!is.na(declared)] <- var_extract$name[declared[!is.na(declared)]]
  return(names)
}
