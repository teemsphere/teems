#' @keywords internal
#' @noRd
.levels_linear_expand <- function(var_extract) {
  if (is.null(var_extract) || nrow(var_extract) == 0L) {
    return(var_extract)
  }
  lv <- .levels_linear_names(var_extract)
  lv <- lv[lv$kind != "plain", ]
  drop <- character(0)
  for (i in seq_len(nrow(lv))) {
    d <- match(lv$decl[i], var_extract$name)
    l <- match(tolower(lv$linear[i]), tolower(var_extract$name))
    if (is.na(l)) {
      l <- d
      var_extract$name[l] <- lv$linear[i]
    } else {
      drop <- c(drop, lv$decl[i])
    }
    var_extract$qualifier_list[l] <- paste0(
      "(levels,", if (lv$change[i]) "change," else "", "orig_level=", tolower(lv$decl[i]), ")"
    )
  }
  var_extract <- var_extract[!var_extract$name %in% drop, ]
  return(var_extract)
}
