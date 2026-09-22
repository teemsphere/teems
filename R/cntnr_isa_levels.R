#' @keywords internal
#' @noRd
.cntnr_isa_levels <- function(images = .local_teems_images()) {
  for (image in images) {
    out <- .cntnr_ld_so_help(image)
    hits <- regmatches(
      x = out,
      m = regexpr("x86-64-v[0-9]+(?= \\(supported, searched\\))", out, perl = TRUE)
    )
    levels <- sort(unique(unlist(hits)), decreasing = TRUE)
    if (length(levels)) {
      return(levels)
    }
  }
  isa_levels <- character(0)
  return(isa_levels)
}
