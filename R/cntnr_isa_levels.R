#' @keywords internal
#' @noRd
.cntnr_isa_levels <- function(images = .local_teems_images()) {
  # The x86-64 psABI levels are what the image was compiled for, so
  # the authority on what the CPU supports is the image's own glibc:
  # `ld.so --help` lists the levels it will search. Asking inside the
  # container works identically under Linux, Docker Desktop on
  # Windows (WSL2 VM) and macOS, where the host has no ld.so at all.
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
