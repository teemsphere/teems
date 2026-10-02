#' @keywords internal
#' @noRd
.levels_linear_names <- function(var_extract) {
  qual <- tolower(gsub("[()[:space:]]", "", var_extract[["qualifier_list"]]))
  qual[is.na(qual)] <- ""
  toks <- strsplit(qual, ",", fixed = TRUE)
  lev <- vapply(toks, \(q) "levels" %in% q, logical(1))
  chg <- vapply(toks, \(q) "change" %in% q, logical(1))
  pick <- function(q, key) {
    hit <- q[startsWith(q, key)]
    if (length(hit) == 0L) NA_character_ else sub(key, "", hit[1], fixed = TRUE)
  }
  lin_name <- vapply(toks, pick, character(1), key = "linear_name=")
  lin_var <- vapply(toks, pick, character(1), key = "linear_var=")
  kind <- ifelse(!is.na(lin_name), "name", ifelse(!is.na(lin_var), "var", "plain"))
  linear <- ifelse(kind == "name", lin_name,
    ifelse(kind == "var", lin_var, paste0(ifelse(chg, "c_", "p_"), var_extract$name))
  )
  lv <- data.frame(
    decl = var_extract$name[lev],
    linear = linear[lev],
    kind = kind[lev],
    change = chg[lev]
  )
  return(lv)
}
