#' @keywords internal
#' @noRd
.exp_cls_entry <- function(cls_entry,
                           var_extract,
                           sets,
                           call) {
  UseMethod(".exp_cls_entry")
}

#' @importFrom data.table CJ setkey
#' @importFrom purrr pluck
#' 
#' @method .exp_cls_entry full
#' @export
.exp_cls_entry.full <- function(cls_entry,
                                var_extract,
                                sets,
                                call) {
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))

  if (var_sets %!=% NA) {
    exp_entry <- do.call(
      data.table::CJ,
      c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
    )
    data.table::setkey(exp_entry)
  } else {
    exp_entry <- NA
  }

  attr(cls_entry, "comp") <- colnames(exp_entry)
  attr(cls_entry, "ele") <- exp_entry
  return(cls_entry)
}