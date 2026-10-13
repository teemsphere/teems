#' @keywords internal
#' @noRd
.parse_tab_maths <- function(extract,
                             call) {

  maths <- extract[tolower(extract$type) %in% c("equation", "formula"), ]
  if (nrow(maths) == 0L) {
    maths <- maths[, c("type", "row_id")]
    maths$name <- character(0)
    maths$label <- character(0)
    maths$qualifier_list <- character(0)
    maths$ls_upper_idx <- logical(0)
    maths$ls_mixed_idx <- logical(0)
    maths$header <- logical(0)
    maths$file <- logical(0)
    maths$definition <- character(0)
    maths$subsets <- logical(0)
    maths$comp1 <- character(0)
    maths$comp2 <- character(0)
    maths <- maths[, c("type", "name", "label", "qualifier_list", "ls_upper_idx",
                       "ls_mixed_idx", "header", "file", "definition", "subsets",
                       "comp1", "comp2", "row_id")]
    return(maths)
  }

  is_eq <- tolower(maths$type) == "equation"
  split <- .split_tab_qualifiers(maths$remainder)
  maths$qualifier_list <- split$qualifier_list
  rem <- split$rest

  maths$name <- NA_character_
  maths$name[is_eq] <- sub("^([A-Za-z_][A-Za-z0-9_@]*).*$", "\\1", rem[is_eq])
  rem[is_eq] <- trimws(substring(rem[is_eq], nchar(maths$name[is_eq]) + 1L))

  has_label <- grepl("#[^#]*#", rem)
  maths$label <- NA_character_
  maths$label[has_label] <- trimws(sub("^[^#]*#([^#]*)#.*$", "\\1", rem[has_label]))
  rem[has_label] <- trimws(gsub("\\s{2,}", " ", sub("#[^#]*#", "", rem[has_label])))

  split <- .split_tab_qualifiers(rem)
  late <- !is.na(split$qualifier_list)
  maths$qualifier_list[late] <- paste0(
    ifelse(is.na(maths$qualifier_list[late]), "", maths$qualifier_list[late]),
    split$qualifier_list[late]
  )
  rem <- split$rest

  quant <- .split_tab_quantifiers(rem)
  maths$full_set <- quant$full_set
  maths$definition <- quant$rest

  sides <- .split_tab_sides(maths$definition)
  maths$comp1 <- sides$comp1
  maths$comp2 <- sides$comp2

  maths$ls_upper_idx <- NA
  maths$ls_mixed_idx <- NA
  maths$header <- NA
  maths$file <- NA
  maths$subsets <- NA

  maths <- maths[, c("type",
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
                     "row_id")]
  
  return(maths)
}
