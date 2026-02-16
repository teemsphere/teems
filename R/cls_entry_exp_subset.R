#' @importFrom purrr pluck map
#' @importFrom data.table CJ setkey setnames fsetdiff
#' 
#' @method .exp_cls_entry subset
#' @export
.exp_cls_entry.subset <- function(cls_entry,
                                  var_extract,
                                  sets,
                                  call) {
  entry_subset <- sub(")", "", purrr::pluck(strsplit(cls_entry, "\\("), 1, 2))
  entry_subset <- strsplit(entry_subset, ",")[[1]]
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))
  
  if (all(entry_subset == var_sets)) {
    class(cls_entry) <- c("full", class(cls_entry))
    UseMethod(".exp_cls_entry", cls_entry)
  }
  
  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )
  attr(cls_entry, "comp") <- entry_subset
  entry_subset <- with(sets, mget(entry_subset))
  entry_subset <- data.table::setnames(do.call(data.table::CJ, entry_subset), var_sets)
  data.table::setkey(entry_subset)
  if (nrow(data.table::fsetdiff(entry_subset, full_entry)) %!=% 0L) {
    .cli_action(cls_err$subset_invalid,
                action = "abort",
                call = call
    )
  }
  
  attr(cls_entry, "ele") <- entry_subset
  return(cls_entry)
}