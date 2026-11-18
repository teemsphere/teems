#' @keywords internal
#' @noRd
.solver_release_api <- function() {
  api <- "https://api.github.com/repos/teemsphere/teems-solver/releases/latest"
  return(api)
}

#' @importFrom jsonlite fromJSON
#' @keywords internal
#' @noRd
.solver_release_latest <- function(timeout = 2) {
  old <- options(timeout = timeout)
  on.exit(options(old), add = TRUE)
  txt <- tryCatch(
    {
      con <- url(.solver_release_api())
      on.exit(close(con), add = TRUE)
      readLines(con, warn = FALSE)
    },
    error = \(e) NULL,
    warning = \(w) NULL
  )
  if (is.null(txt)) {
    return(NULL)
  }
  release <- tryCatch(
    jsonlite::fromJSON(paste(txt, collapse = "\n")),
    error = \(e) NULL
  )
  tag <- release[["tag_name"]]
  if (!is.character(tag) || length(tag) != 1L) {
    return(NULL)
  }
  version <- sub("^v", "", tag)
  if (is.na(.solver_version_core(version))) {
    return(NULL)
  }
  latest <- list(version = version, url = release[["html_url"]])
  return(latest)
}

#' @keywords internal
#' @noRd
.solver_release_cache <- new.env(parent = emptyenv())

#' @importFrom utils packageVersion
#' @keywords internal
#' @noRd
.solver_release_notice <- function(image,
                                   installed,
                                   call = NULL) {
  told <- isTRUE(.solver_release_cache[[image]])
  if (is.null(.solver_release_cache[["latest"]])) {
    .solver_release_cache[["latest"]] <- .solver_release_latest() %|||% NA
  }
  .solver_release_cache[[image]] <- TRUE
  latest <- .solver_release_cache[["latest"]]
  newer <- !told && is.list(latest) &&
    .solver_version_key(installed) < .solver_version_key(latest$version)
  if (newer) {
    releases_url <- "https://github.com/teemsphere/teems-solver/releases"
    rebuild_url <- "https://github.com/teemsphere/teems-solver#updating-an-existing-installation"
    release_url <- latest$url %|||% releases_url
    pkg_major <- as.integer(.solver_version_core(as.character(utils::packageVersion("teems")))[[1, 1]])
    latest_major <- as.integer(.solver_version_core(latest$version)[[1, 1]])
    latest <- latest$version
    if (latest_major > pkg_major) {
      msg <- solve_info$solver_release_major
    } else {
      msg <- solve_info$solver_release_newer
    }
    .cli_action(msg,
      action = "inform",
      call = call
    )
  }
  return(invisible(newer))
}
