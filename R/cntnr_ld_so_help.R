#' @keywords internal
#' @noRd
.cntnr_ld_so_help <- function(image) {
  help <- tryCatch(
    suppressWarnings(system2("docker",
      c("run", "--rm", "--entrypoint", "ld.so", image, "--help"),
      stdout = TRUE, stderr = FALSE
    )),
    error = \(e) character(0)
  )
  return(help)
}
