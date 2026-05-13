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
  var_name <- attr(cls_entry, "var_name")
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", var_name)
  idx_sets <- purrr::pluck(var_extract, "ls_mixed_idx", var_name)

  if (all(entry_subset == var_sets)) {
    class(cls_entry) <- c("full", class(cls_entry))
    UseMethod(".exp_cls_entry", cls_entry)
  }

  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )

  data.table::setnames(full_entry, idx_sets)
  data.table::setkey(full_entry)

  attr(cls_entry, "comp") <- entry_subset
  entry_subset <- with(sets, mget(entry_subset))

  attr(cls_entry, "sets") <- var_sets
  if (any(duplicated(names(entry_subset)))) {
    var_sets <- make.unique(names(entry_subset))
    names(entry_subset) <- var_sets
  }

  entry_subset <- do.call(data.table::CJ, entry_subset)
  data.table::setnames(entry_subset, idx_sets)

  if (nrow(data.table::fsetdiff(entry_subset, full_entry)) %!=% 0L) {
    .cli_action(model_err$subset_invalid,
      action = "abort",
      call = call
    )
  }

  data.table::setnames(entry_subset, var_sets)
  attr(cls_entry, "ele") <- entry_subset
  return(cls_entry)
}