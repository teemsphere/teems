#' @keywords internal
#' @noRd
.local_teems_images <- function() {
  if (!nzchar(Sys.which("docker"))) {
    images <- character(0)
    return(images)
  }
  out <- tryCatch(
    suppressWarnings(system2("docker",
      c("images", "--format", "{{.Repository}}:{{.Tag}}", "teems"),
      stdout = TRUE, stderr = FALSE
    )),
    error = \(e) character(0)
  )
  out <- out[nzchar(out) & !grepl("<none>", out, fixed = TRUE)]
  # the fallback image is the one every install has; probe it first
  images <- unique(c(intersect("teems:latest", out), out))
  return(images)
}
