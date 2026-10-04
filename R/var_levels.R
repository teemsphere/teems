#' @keywords internal
#' @noRd
.var_presim_levels <- function(sol_prefix,
                               var_tbl,
                               var_extract) {
  if (is.null(var_extract) || nrow(var_extract) == 0L ||
    !.has_coefficient_dump(sol_prefix, presim = TRUE)) {
    pre <- list()
    return(pre)
  }
  decl <- tolower(var_extract$name)
  quals <- tolower(var_extract$qualifier_list)
  target <- .orig_level_target(var_extract$qualifier_list)
  is_levels <- grepl("\\blevels\\b", quals)
  target[is_levels & is.na(target)] <- decl[is_levels & is.na(target)]
  names(target) <- decl
  target <- target[!is.na(target)]
  if (length(target) == 0L) {
    pre <- list()
    return(pre)
  }
  is_change <- grepl("\\bchange\\b", quals)
  names(is_change) <- decl
  lookup <- function(v) {
    hit <- target[v]
    base <- sub("^[pc]_", "", v)
    if (is.na(hit) && base != v && !v %in% decl && base %in% decl &&
      is_levels[match(base, decl)] && startsWith(v, "c_") == is_change[[base]]) {
      hit <- target[base]
    }
    unname(hit)
  }
  wanted <- vapply(var_tbl$cofname, lookup, character(1))
  numeric_target <- suppressWarnings(as.numeric(wanted))
  coeffs <- unique(wanted[!is.na(wanted) & is.na(numeric_target)])
  cof <- NULL
  if (length(coeffs) > 0L) {
    cof <- .parse_coefficient_bins(
      sol_prefix = sol_prefix,
      coeff_names = coeffs,
      presim = TRUE
    )
  }
  var_sets <- function(i) {
    ids <- strsplit(var_tbl$setid[i], ",", fixed = TRUE)[[1]]
    ids[seq_len(var_tbl$size[i])]
  }
  pre <- list()
  for (i in which(!is.na(wanted))) {
    n <- var_tbl$matsize[i]
    if (!is.na(numeric_target[i])) {
      values <- rep(numeric_target[i], n)
    } else {
      j <- match(wanted[i], cof$cof_union$cofname)
      if (is.na(j) || cof$cof_union$matsize[j] != n) {
        next
      }
      cof_sets <- strsplit(cof$cof_union$setid[j], ",", fixed = TRUE)[[1]]
      cof_sets <- cof_sets[seq_len(cof$cof_union$size[j])]
      if (!identical(cof_sets, var_sets(i))) {
        next
      }
      start <- cof$cof_union$pack_begadd[j]
      values <- cof$xc$Value[start + seq_len(n)]
    }
    attr(values, "change") <- isTRUE(var_tbl$change_real[i])
    pre[[var_tbl$cofname[i]]] <- values
  }
  return(pre)
}

#' @keywords internal
#' @noRd
.add_levels <- function(dt,
                        pre) {
  x <- dt$Value
  dt$PreLevel <- as.numeric(pre)
  if (isTRUE(attr(pre, "change"))) {
    dt$PostLevel <- dt$PreLevel + x
    dt$PercentChange <- ifelse(dt$PreLevel == 0, NA_real_, 100 * x / dt$PreLevel)
  } else {
    dt$PostLevel <- dt$PreLevel * (1 + x / 100)
    dt$Change <- dt$PostLevel - dt$PreLevel
  }
  return(dt)
}
