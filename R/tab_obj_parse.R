#' @keywords internal
#' @noRd
.parse_tab_obj <- function(extract,
                           obj_type,
                           call) {
  obj <- extract[tolower(extract$type) == obj_type, ]

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
  
  # some permutations not yet utilized
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
