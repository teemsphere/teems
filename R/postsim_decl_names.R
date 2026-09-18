# PostSim declarations captured by name from the raw statements: the
# extraction merge can shift marker rows by one, so declaration rows
# are tagged by name while executables use the region flag
#' @keywords internal
#' @noRd
.postsim_decl_names <- function(tab) {
  ps_region <- cumsum(grepl("^\\s*postsim\\s*\\(\\s*begin", tab, ignore.case = TRUE)) -
    cumsum(grepl("^\\s*postsim\\s*\\(\\s*end", tab, ignore.case = TRUE))
  ps_raw <- tab[ps_region > 0 & !grepl("^\\s*postsim", tab, ignore.case = TRUE)]
  ps_decl_names <- toupper(unlist(lapply(
    ps_raw[grepl("^\\s*(coefficient|set|subset|file)\\b", ps_raw, ignore.case = TRUE)],
    \(x) {
      x <- sub("^\\s*(coefficient|set|subset|file)\\s*", "", x, ignore.case = TRUE)
      x <- gsub("\\(all\\s*,[^)]*\\)", "", x, ignore.case = TRUE)
      x <- gsub("\\([^)]*\\)", "", x)
      x <- trimws(sub("[#(].*$", "", x))
      strsplit(trimws(x), "\\s+")[[1]][1]
    }
  )))
  return(ps_decl_names)
}
