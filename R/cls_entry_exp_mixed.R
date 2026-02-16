#' @importFrom purrr pluck map
#' @importFrom data.table CJ setkey setnames fsetdiff
#' @importFrom utils type.convert
#' 
#' @method .exp_cls_entry mixed
#' @export
.exp_cls_entry.mixed <- function(cls_entry,
                                 var_extract,
                                 sets,
                                 call) {
  entry_mixed <- sub(")", "", purrr::pluck(strsplit(cls_entry, "\\("), 1, 2))
  entry_mixed <- strsplit(entry_mixed, ",")[[1]]
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))
  
  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )
  
  entry_mixed <- purrr::map(entry_mixed, function(e) {
    if (!grepl("\"", e)) {
      # we could check subsets directly but need better set-subset mapping
      with(sets, get(e))
    } else {
      utils::type.convert(gsub("\"", "", e), as.is = TRUE)
    }
  })
  
  entry_mixed <- data.table::setnames(do.call(data.table::CJ, entry_mixed), var_sets)
  data.table::setkey(entry_mixed)
  if (nrow(data.table::fsetdiff(entry_mixed, full_entry)) %!=% 0L) {
    invalid_entries <- data.table::fsetdiff(entry_mixed, full_entry)
    n_invalid_entries <- nrow(invalid_entries)
    .cli_action(cls_err$mixed_invalid,
                action = "abort",
                call = call
    )
  }
  
  attr(cls_entry, "comp") <- strsplit(gsub("\"|\\)", "", pluck(strsplit(cls_entry, "\\("), 1, 2)), ",")[[1]]
  attr(cls_entry, "ele") <- entry_mixed
  return(cls_entry)
}
