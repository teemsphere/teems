#' @keywords internal
#' @noRd
.levels_linear_alias <- function(names,
                                 var_extract) {
  qual <- var_extract[["qualifier_list"]]
  lev <- !is.na(qual) & grepl("\\blevels\\b", qual, ignore.case = TRUE)
  chg <- lev & grepl("(^|[(,\\s])change\\b", qual, ignore.case = TRUE, perl = TRUE)
  alias <- c(paste0("c_", var_extract$name[chg]), paste0("p_", var_extract$name[lev & !chg]))
  target <- c(var_extract$name[chg], var_extract$name[lev & !chg])
  hit <- match(tolower(names), tolower(alias))
  names[!is.na(hit)] <- target[hit[!is.na(hit)]]
  return(names)
}
