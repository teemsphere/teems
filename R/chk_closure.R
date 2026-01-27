#' @importFrom purrr map2 map_chr
#' 
#' @noRd
#' @keywords internal
.check_closure <- function(closure,
                           var_omit,
                           var_extract) {
  closure <- closure[!grepl("!", closure)]
  temp <- gsub("\\([^)]*\\)", "", closure)

  closure <- unlist(purrr::map2(
    closure,
    temp,
    function(cls, t) {
      if (grepl("\\s", t)) {
        strsplit(t, " ")
      } else {
        cls
      }
    }
  ))

  closure <- closure[!closure %in% var_omit]
  cls_var <- purrr::map_chr(strsplit(closure, "\\("), 1)

  if (!all(cls_var %in% var_extract$name)) {
    var_discrepancy <- setdiff(tolower(cls_var), tolower(var_extract$name))
    l_var <- length(var_discrepancy)
    .cli_action(cls_err$no_var,
      action = "abort",
      call = call
    )
  }
  return(closure)
}