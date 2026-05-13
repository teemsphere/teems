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
  var_name <- attr(cls_entry, "var_name")
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", var_name)
  idx_sets <- purrr::pluck(var_extract, "ls_mixed_idx", var_name)
  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )
  data.table::setnames(full_entry, idx_sets)
  data.table::setkey(full_entry)

  entry_mixed <- purrr::map(entry_mixed, function(e) {
    if (!grepl("\"", e)) {
      # name search of sets more robust?
      with(sets, get(e))
    } else {
      utils::type.convert(gsub("\"", "", e), as.is = TRUE)
    }
  })

  entry_mixed <- do.call(data.table::CJ, entry_mixed)
  data.table::setnames(entry_mixed, idx_sets)

  if (nrow(data.table::fsetdiff(entry_mixed, full_entry)) %!=% 0L) {
    invalid_entries <- data.table::fsetdiff(entry_mixed, full_entry)
    n_invalid_entries <- nrow(invalid_entries)
    invalid_entries <- utils::capture.output(print(invalid_entries))[-c(1:3)]
    .cli_action(model_err$mixed_invalid,
      action = "abort",
      call = call
    )
  }

  attr(cls_entry, "comp") <- strsplit(
    gsub(
      "\"|\\)",
      "",
      purrr::pluck(strsplit(cls_entry, "\\("), 1, 2)
    ),
    ","
  )[[1]]

  attr(cls_entry, "sets") <- var_sets
  if (any(duplicated(var_sets))) {
    var_sets <- make.unique(var_sets)
  }

  data.table::setnames(entry_mixed, var_sets)
  attr(cls_entry, "ele") <- entry_mixed
  return(cls_entry)
}