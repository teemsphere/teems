#' @importFrom purrr map2 map_chr
#' @noRd
#' @keywords internal
.check_closure <- function(closure,
                           var_extract,
                           call) {
  closure <- closure[!grepl("!", closure)]
  temp <- gsub("\\([^)]*\\)", "", closure)

  closure <- unlist(purrr::map2(
    closure,
    temp,
    \(cls, t) {
      if (grepl("\\s", t)) {
        strsplit(t, " ")
      } else {
        cls
      }
    }
  ))

  cls_var <- purrr::map_chr(strsplit(closure, "\\("), 1)
  aliased <- .levels_linear_alias(cls_var, var_extract)
  renamed <- aliased != cls_var
  closure[renamed] <- paste0(aliased[renamed], substring(closure[renamed], nchar(cls_var[renamed]) + 1L))
  cls_var <- aliased

  backsolve_vars <- var_extract$name[var_extract$condense %in% "backsolve"]

  if (any(tolower(backsolve_vars) %in% tolower(cls_var))) {
    bs_exo <- backsolve_vars[tolower(backsolve_vars) %in% tolower(cls_var)]
    .cli_action(model_err$condense_endo,
      action = c("abort", "inform"),
      call = call
    )
  }

  if (!all(cls_var %in% var_extract$name)) {
    var_discrepancy <- unique(setdiff(cls_var, var_extract$name))
    candidates <- .nearest_names(var_discrepancy, var_extract$name)
    msg <- cls_err$unknown_var
    action <- c("abort", "inform")
    if (length(candidates) == 0L) {
      msg <- msg[1]
      action <- action[1]
    }
    .cli_action(msg,
      action = action,
      call = call
    )
  }
  return(closure)
}

#' @importFrom utils adist head
#' @keywords internal
#' @noRd
.nearest_names <- function(unknown,
                           declared,
                           max_dist = 3L,
                           cap = 5L) {
  declared <- unique(declared)
  d <- utils::adist(tolower(unknown), tolower(declared))
  hits <- lapply(seq_along(unknown), \(i) {
    j <- which(d[i, ] <= max_dist)
    j[order(d[i, j])]
  })
  candidates <- declared[unique(unlist(hits))]
  nearest <- utils::head(candidates, cap)
  return(nearest)
}
