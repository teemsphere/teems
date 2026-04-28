#' @importFrom purrr pluck map
#' @importFrom utils type.convert
#' @importFrom data.table CJ setkey setnames fintersect
#' 
#' @method .exp_cls_entry ele
#' @export
.exp_cls_entry.ele <- function(cls_entry,
                               var_extract,
                               sets,
                               call) {
  entry_ele <- sub(")", "", purrr::pluck(strsplit(cls_entry, "\\("), 1, 2))
  entry_ele <- strsplit(entry_ele, ",")[[1]]
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))
  idx_sets <- purrr::pluck(var_extract, "ls_mixed_idx", attr(cls_entry, "var_name"))
  
  entry_ele <- purrr::map(entry_ele, function(e) {
    utils::type.convert(gsub("\"", "", e), as.is = TRUE)
  })
  
  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )
  data.table::setnames(full_entry, new = idx_sets)
  
  entry_ele <- data.table::setnames(do.call(data.table::CJ, entry_ele), var_sets)
  data.table::setnames(entry_ele, new = idx_sets)
  data.table::setkey(entry_ele)
  if (nrow(data.table::fintersect(entry_ele, full_entry)) %!=% 1L) {
    .cli_action(model_err$ele_invalid,
                action = "abort",
                call = call
    )
  }
  
  data.table::setnames(entry_ele, new = var_sets)
  data.table::setkey(entry_ele)
  attr(cls_entry, "comp") <- unlist(entry_ele)
  attr(cls_entry, "ele") <- entry_ele
  return(cls_entry)
}