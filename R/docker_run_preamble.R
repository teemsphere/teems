#' @keywords internal
#' @noRd
.container_name <- function(timeID) {
  name <- paste0("teems_", timeID)
  return(name)
}

#' @keywords internal
#' @noRd
.docker_run_preamble <- function(timeID,
                                 run_dir) {
  preamble <- paste(
    paste0(
      "docker run --rm --pull never --init -e TINI_KILL_PROCESS_GROUP=1 --name ",
      .container_name(timeID), " ", .docker_user_flag(), "--mount"
    ),
    .shell_quote(paste("type=bind", paste0("src=", run_dir), "dst=/opt/teems", sep = ",")),
    paste0("teems", ":", .o_docker_tag()),
    "/bin/bash -c"
  )
  return(preamble)
}

#' @keywords internal
#' @noRd
.container_remove <- function(timeID) {
  suppressWarnings(tryCatch(
    system2("docker", c("rm", "-f", .container_name(timeID)),
      stdout = FALSE, stderr = FALSE
    ),
    error = \(e) NULL
  ))
  return(invisible(NULL))
}
