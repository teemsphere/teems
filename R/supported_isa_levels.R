#' @keywords internal
#' @noRd
.supported_isa_levels <- function(machine = Sys.info()[["machine"]]) {
  if (is.element(machine, c("aarch64", "arm64"))) {
    return("armv8-a")
  }
  if (!is.element(machine, c("x86_64", "x86-64", "AMD64"))) {
    isa_levels <- character(0)
    return(isa_levels)
  }

  # the CPU does not change within a session; probe the container once
  if (is.null(.isa_cache$levels)) {
    levels <- .cntnr_isa_levels()
    if (!length(levels)) {
      levels <- "x86-64-v2"
    }
    .isa_cache$levels <- levels
  }
  return(.isa_cache$levels)
}
