#' @keywords internal
#' @noRd
.parse_tab_read <- function(extract,
                            call) {

  reads <- extract[tolower(extract$type) %in% "read", ]
  if (any(grepl("\\(", reads$remainder))) {
    .cli_action(model_err$invalid_read,
      action = "abort",
      call = call
    )
  }
  reads$type <- "Read"
  reads$name <- purrr::map_chr(strsplit(reads$remainder, " "), 1)
  reads$header <- purrr::map_chr(strsplit(reads$remainder, " "), function(r) {
    tail(r, 1)
  })

  reads$remainder <- purrr::map2_chr(
    reads$name,
    reads$remainder,
    function(n, r) {
      sub(n, "", r)
    }
  )

  reads$remainder <- purrr::map2_chr(
    reads$header,
    reads$remainder,
    function(h, r) {
      gsub(h, "", r)
    }
  )

  reads$header <- gsub("\"", "", reads$header)
  reads$remainder <- gsub("from file", "", reads$remainder, ignore.case = TRUE)
  reads$remainder <- gsub("header", "", reads$remainder, ignore.case = TRUE)
  reads$file <- trimws(reads$remainder)
  reads$remainder <- NULL
  reads$label <- NA
  reads$qualifier_list <- NA
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