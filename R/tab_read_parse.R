#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.parse_tab_read <- function(extract,
                            call) {

  reads <- extract[tolower(extract$type) %in% "read", ]
  qual <- .read_qualifier(reads$remainder)
  reads$remainder <- .strip_read_qualifier(reads$remainder)
  reads$remainder <- gsub("\\bheader\\s*\"", "header \"", reads$remainder, ignore.case = TRUE)
  part <- grepl("^\\s*\\(\\s*all\\s*,", reads$remainder, ignore.case = TRUE)
  decls <- paste("Coefficient", extract$remainder[tolower(extract$type) == "coefficient"])
  full <- part & .read_is_full(paste("Read", reads$remainder), decls)
  part_set <- rep(NA_character_, nrow(reads))
  part_set[part & !full] <- trimws(sub("^\\s*\\(\\s*all\\s*,[^,]*,([^)]*)\\).*$", "\\1", reads$remainder[part & !full], ignore.case = TRUE))
  reads$remainder[part] <- sub("^\\s*(\\([^)]*\\)\\s*)+([^ (]+)\\s*\\([^)]*\\)", "\\2", reads$remainder[part])
  m <- regmatches(reads$remainder, regexec(
    "^\\s*([A-Za-z_][A-Za-z0-9_@]*)\\s+from\\s+file\\s+([A-Za-z_][A-Za-z0-9_@]*)\\s+header\\s+\"([^\"]*)\"\\s*$",
    reads$remainder,
    ignore.case = TRUE
  ))
  bad <- lengths(m) == 0L
  if (any(bad)) {
    bad_reads <- trimws(paste("Read", reads$remainder[bad]))
    .cli_action(model_err$read_malformed,
      action = "abort",
      call = call
    )
  }
  reads$type <- "Read"
  reads$name <- purrr::map_chr(m, 2)
  reads$file <- purrr::map_chr(m, 3)
  reads$header <- purrr::map_chr(m, 4)
  reads$remainder <- NULL
  reads$label <- NA
  reads$qualifier_list <- qual
  reads$ls_upper_idx <- NA
  reads$ls_mixed_idx <- NA
  reads$definition <- NA
  reads$subsets <- NA
  reads$comp1 <- part_set
  reads$comp2 <- NA

  reads <- reads[, c(
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
  return(reads)
}
#' @keywords internal
#' @noRd
.read_qualifier <- function(remainder) {
  m <- regmatches(
    remainder,
    regexpr("^\\s*\\(\\s*(by_elements|ifheaderexists)\\s*\\)", remainder, ignore.case = TRUE)
  )
  out <- rep(NA_character_, length(remainder))
  hit <- grepl("^\\s*\\(\\s*(by_elements|ifheaderexists)\\s*\\)", remainder, ignore.case = TRUE)
  out[hit] <- paste0("(", tolower(gsub("[\\s()]", "", m, perl = TRUE)), ")")
  return(out)
}

#' @keywords internal
#' @noRd
.strip_read_qualifier <- function(remainder) {
  stripped <- sub("^\\s*\\(\\s*(by_elements|ifheaderexists)\\s*\\)\\s*", "", remainder, ignore.case = TRUE)
  return(stripped)
}
