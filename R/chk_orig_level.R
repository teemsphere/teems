#' @keywords internal
#' @noRd
.orig_level_target <- function(qualifier_list) {
  quals <- tolower(gsub("[()[:space:]]", "", qualifier_list))
  target <- vapply(strsplit(quals, ",", fixed = TRUE), \(q) {
    hit <- q[startsWith(q, "orig_level=")]
    if (length(hit) == 0L) NA_character_ else sub("^orig_level=", "", hit[1])
  }, character(1))
  return(target)
}

#' @keywords internal
#' @noRd
.check_orig_level <- function(var_extract,
                              coeff_extract,
                              call) {
  target <- .orig_level_target(var_extract$qualifier_list)
  for (i in which(!is.na(target))) {
    if (!is.na(suppressWarnings(as.numeric(target[i])))) {
      next
    }
    orig_var <- var_extract$name[i]
    orig_coeff <- target[i]
    j <- match(orig_coeff, tolower(coeff_extract$name))
    if (is.na(j)) {
      .cli_action(model_err$orig_level_unknown,
        action = "abort",
        call = call
      )
    }
    if (grepl("\\binteger\\b", tolower(coeff_extract$qualifier_list[j]))) {
      .cli_action(model_err$orig_level_integer,
        action = "abort",
        call = call
      )
    }
    var_sets <- toupper(unlist(var_extract$ls_upper_idx[[i]]))
    coeff_sets <- toupper(unlist(coeff_extract$ls_upper_idx[[j]]))
    var_sets <- var_sets[!is.na(var_sets)]
    coeff_sets <- coeff_sets[!is.na(coeff_sets)]
    if (!identical(unname(var_sets), unname(coeff_sets))) {
      .cli_action(model_err$orig_level_sets,
        action = "abort",
        call = call
      )
    }
  }
  return(invisible(NULL))
}
