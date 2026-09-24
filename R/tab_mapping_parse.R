#' @keywords internal
#' @noRd
.canonical <- function(s, set_names) {
  r_idx <- match(tolower(s), tolower(set_names))
  canon <- ifelse(is.na(r_idx), s, set_names[r_idx])
  return(canon)
}

#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.parse_tab_mapping <- function(extract,
                               set_names,
                               call) {
  maps <- extract[tolower(extract$type) %in% "mapping", ]
  maps$type <- "Mapping"

  parsed <- regmatches(
    maps$remainder,
    regexec(
      "^\\s*((?:\\(\\s*(?:onto|project)\\s*\\)\\s*)*)([A-Za-z][A-Za-z0-9_@]*)\\s+from\\s+([A-Za-z][A-Za-z0-9_@]*)\\s+to\\s+([A-Za-z][A-Za-z0-9_@]*)\\s*$",
      maps$remainder,
      ignore.case = TRUE,
      perl = TRUE
    )
  )

  bad <- lengths(parsed) == 0L
  if (any(bad)) {
    bad_stmt <- trimws(paste("Mapping", maps$remainder[bad][1]))
    .cli_action(model_err$map_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }

  maps$qualifier_list <- purrr::map_chr(parsed, \(p) {
    q <- tolower(gsub("\\s", "", p[2]))
    if (nzchar(q)) {
      q
    } else {
      NA_character_
    }
  })
  maps$name <- purrr::map_chr(parsed, 3)
  maps$comp1 <- purrr::map_chr(parsed, 4)
  maps$comp2 <- purrr::map_chr(parsed, 5)

  maps$comp1 <- .canonical(maps$comp1, set_names)
  maps$comp2 <- .canonical(maps$comp2, set_names)

  undecl <- !tolower(maps$comp1) %in% tolower(set_names) |
    !tolower(maps$comp2) %in% tolower(set_names)
  if (any(undecl)) {
    map_name <- maps$name[undecl][1]
    bad_sets <- setdiff(
      unique(c(maps$comp1[undecl], maps$comp2[undecl])),
      set_names
    )
    .cli_action(model_err$map_undeclared_set,
      action = "abort",
      call = call
    )
  }

  maps$definition <- paste("from", maps$comp1, "to", maps$comp2)
  maps$remainder <- NULL
  maps$label <- NA
  maps$ls_upper_idx <- NA
  maps$ls_mixed_idx <- NA
  maps$header <- NA
  maps$file <- NA
  maps$subsets <- NA

  maps <- maps[, c(
    "type",
    "name",
    "label",
    "qualifier_list",
    "ls_upper_idx",
    "ls_mixed_idx",
    "header",
    "file",
    "definition",
    "subsets",
    "comp1",
    "comp2",
    "row_id"
  )]
  return(maps)
}
