#' @keywords internal
#' @noRd
.isa_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.local_teems_images <- function() {
  if (!nzchar(Sys.which("docker"))) {
    return(character(0))
  }
  out <- tryCatch(
    suppressWarnings(system2("docker",
      c("images", "--format", "{{.Repository}}:{{.Tag}}", "teems"),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(e) character(0)
  )
  out <- out[nzchar(out) & !grepl("<none>", out, fixed = TRUE)]
  # the fallback image is the one every install has; probe it first
  return(unique(c(intersect("teems:latest", out), out)))
}

#' @keywords internal
#' @noRd
.container_ld_so_help <- function(image) {
  tryCatch(
    suppressWarnings(system2("docker",
      c("run", "--rm", "--entrypoint", "ld.so", image, "--help"),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(e) character(0)
  )
}

#' @keywords internal
#' @noRd
.container_isa_levels <- function(images = .local_teems_images()) {
  # The x86-64 psABI levels are what the image was compiled for, so
  # the authority on what the CPU supports is the image's own glibc:
  # `ld.so --help` lists the levels it will search. Asking inside the
  # container works identically under Linux, Docker Desktop on
  # Windows (WSL2 VM) and macOS, where the host has no ld.so at all.
  for (image in images) {
    out <- .container_ld_so_help(image)
    hits <- regmatches(
      x = out,
      m = regexpr("x86-64-v[0-9]+(?= \\(supported, searched\\))", out, perl = TRUE)
    )
    levels <- sort(unique(unlist(hits)), decreasing = TRUE)
    if (length(levels)) {
      return(levels)
    }
  }
  return(character(0))
}

#' @keywords internal
#' @noRd
.supported_isa_levels <- function(machine = Sys.info()[["machine"]]) {
  if (is.element(machine, c("aarch64", "arm64"))) {
    return("armv8-a")
  }
  if (!is.element(machine, c("x86_64", "x86-64", "AMD64"))) {
    return(character(0))
  }

  # the CPU does not change within a session; probe the container once
  if (is.null(.isa_cache$levels)) {
    levels <- .container_isa_levels()
    if (!length(levels)) {
      levels <- "x86-64-v2"
    }
    .isa_cache$levels <- levels
  }
  return(.isa_cache$levels)
}

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
    error = function(e) character(0)
  )
  return(any(nzchar(out)))
}

#' @keywords internal
#' @noRd
.resolve_docker_tag <- function() {
  explicit <- ems_options$docker_tag
  if (!is.null(explicit)) {
    return(explicit)
  }

  for (level in .supported_isa_levels()) {
    candidates <- unique(c(level, sub("^x86-64-", "", level)))
    for (tag in candidates) {
      if (.docker_image_present(paste0("teems:", tag))) {
        .cli_action(solve_info$docker_tag_auto,
          action = "inform"
        )
        return(tag)
      }
    }
  }
  return("latest")
}
