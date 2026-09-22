#' @keywords internal
#' @noRd
.drop_excluded_headers <- function(tab,
                                   call) {
  if (any(tab$header %in% .o_full_exclude())) {
    x_header <- intersect(tab$header, .o_full_exclude())
    for (h in unique(x_header)) {
      x_coeff <- tab$name[match(h, tab$header)]
      hit <- grepl(paste0("\\b", x_coeff, "\\b"), tab$tab, ignore.case = TRUE)
      dep <- hit & tolower(tab$type) %in% c("set", "subset")
      if (any(dep)) {
        bad_set <- tab$name[dep][1]
        excl_coeff <- x_coeff
        excl_header <- h
        .cli_action(model_err$exclude_set_dep,
          action = c("abort", "inform"),
          call = call
        )
      }
      tab <- tab[!hit, ]
    }
  }
  return(tab)
}
