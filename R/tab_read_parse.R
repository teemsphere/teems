#' @importFrom purrr map_chr map2_chr
#' @importFrom utils tail
#' @keywords internal
#' @noRd
.parse_tab_read <- function(extract,
                            call) {

  reads <- extract[tolower(extract$type) %in% "read", ]
  qual <- .read_qualifier(reads$remainder)
  reads$remainder <- .strip_read_qualifier(reads$remainder)
  if (any(grepl("\\(", reads$remainder))) {
    .cli_action(model_err$invalid_read,
      action = "abort",
      call = call
    )
  }
  reads$type <- "Read"
  reads$name <- purrr::map_chr(strsplit(reads$remainder, " "), 1)
  reads$header <- purrr::map_chr(strsplit(reads$remainder, " "), \(r) {
    utils::tail(r, 1)
  })

  reads$remainder <- purrr::map2_chr(
    reads$name,
    reads$remainder,
    \(n, r) {
      sub(n, "", r)
    }
  )

  reads$remainder <- purrr::map2_chr(
    reads$header,
    reads$remainder,
    \(h, r) {
      gsub(h, "", r)
    }
  )

  reads$header <- gsub("\"", "", reads$header)
  reads$remainder <- gsub("from file", "", reads$remainder, ignore.case = TRUE)
  reads$remainder <- gsub("header", "", reads$remainder, ignore.case = TRUE)
  reads$file <- trimws(reads$remainder)
  reads$remainder <- NULL
  reads$label <- NA
  reads$qualifier_list <- qual
  reads$ls_upper_idx <- NA
  reads$ls_mixed_idx <- NA
  reads$definition <- NA
  reads$subsets <- NA
  reads$comp1 <- NA
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
