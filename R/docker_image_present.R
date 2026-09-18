#' @keywords internal
#' @noRd
.docker_image_present <- function(image_name) {
  if (!nzchar(Sys.which("docker"))) {
    return(FALSE)
  }
  out <- tryCatch(
    suppressWarnings(system2("docker", c("images", "-q", image_name),
      stdout = TRUE, stderr = FALSE
    )),
    error = \(e) character(0)
  )
  present <- any(nzchar(out))
  return(present)
}
