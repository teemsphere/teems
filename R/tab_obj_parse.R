#' @keywords internal
#' @noRd
.parse_tab_obj <- function(extract,
                           obj_type,
                           call) {
  obj <- extract[tolower(extract$type) == obj_type, ]

  if (nrow(obj) == 0L) {
    obj <- obj[, c("type", "row_id")]
    obj$name <- character(0)
    obj$label <- character(0)
    obj$qualifier_list <- character(0)
    obj$ls_upper_idx <- list()
    obj$ls_mixed_idx <- list()
    obj$header <- character(0)
    obj$file <- character(0)
    obj$definition <- logical(0)
    obj$subsets <- logical(0)
    obj$comp1 <- logical(0)
    obj$comp2 <- logical(0)
    obj <- obj[, c("type", "name", "label", "qualifier_list", "ls_upper_idx",
                   "ls_mixed_idx", "header", "file", "definition", "subsets",
                   "comp1", "comp2", "row_id")]
    return(obj)
  }

  obj <- .label_tab_obj(obj)
  obj <- .name_tab_obj(obj)
  obj <- .index_tab_obj(obj)
  obj <- .bind_tab_obj_read(
    obj = obj,
    extract = extract,
    obj_type = obj_type,
    call = call
  )

  names(obj$ls_upper_idx) <- obj$name
  names(obj$ls_mixed_idx) <- obj$name
  
  obj$definition <- NA
  obj$comp1 <- NA
  obj$comp2 <- NA
  obj$subsets <- NA
  
  obj <- obj[, c("type",
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

  return(obj)
}
