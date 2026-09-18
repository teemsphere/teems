#' @keywords internal
#' @noRd
.isa_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.resolve_docker_tag <- function(quiet = FALSE) {
  explicit <- ems_options$docker_tag
  if (!is.null(explicit)) {
    return(explicit)
  }

  for (level in .supported_isa_levels()) {
    candidates <- unique(c(level, sub("^x86-64-", "", level)))
    for (tag in candidates) {
      if (.docker_image_present(paste0("teems:", tag))) {
        if (!quiet) {
          .cli_action(solve_info$docker_tag_auto,
            action = "inform"
          )
        }
        return(tag)
      }
    }
  }
  return("latest")
}
