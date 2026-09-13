#' @keywords internal
#' @noRd
.retrieve_cmf <- function(file,
                          cmf_path) {

  docker_path <- grep(
    pattern = file,
    readLines(cmf_path),
    value = TRUE
  )

  file_entry <- sub("/opt/teems",
                    normalizePath(dirname(cmf_path), "/"),
                    docker_path,
                    fixed = TRUE)
  
  # the path is the quoted field of the entry. Taking the second
  # whitespace-delimited token truncated any path containing a space at
  # that space, and the run then failed on a file name that was never
  # written ("cannot open the connection", naming a prefix of the path)
  quoted <- regmatches(file_entry, regexpr('"[^"]*"', file_entry))
  file_name <- if (length(quoted) > 0L) {
    quoted[[1]]
  } else {
    strsplit(file_entry, " ")[[1]][2]
  }
  file_name <- gsub(
    pattern = "\"|;",
    replacement = "",
    x = file_name
  )

  return(file_name)
}