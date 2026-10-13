#' @keywords internal
#' @noRd
.check_docker <- function(image_name,
                          call) {
  docker_installed <- nzchar(Sys.which("docker"))
  if (!docker_installed) {
    .cli_action(solve_err$docker_installed,
      action = "abort",
      call = call
    )
  }

  info <- suppressWarnings(system2("docker", "info", stdout = TRUE, stderr = TRUE))
  docker_access <- (attr(info, "status") %|||% 0L) == 0L
  if (!docker_access) {
    if (Sys.info()[["sysname"]] == "Linux" && any(grepl("permission denied", info, ignore.case = TRUE))) {
      .cli_action(solve_err$docker_sudo,
        action    = "abort",
        url       = "https://docs.docker.com/engine/install/linux-postinstall/",
        hyperlink = "Manage Docker as a non-root user",
        call      = call
      )
    } else {
      .cli_action(solve_err$docker_not_running,
        action = "abort",
        call   = call
      )
    }
  }

  image <- paste0(image_name, ":", .o_docker_tag())
  if (!.docker_image_present(image)) {
    image_name <- image
    .cli_action(solve_err$docker_x_image,
      action = "abort",
      url = "https://github.com/teemsphere/teems-solver",
      hyperlink = "teems-solver repository",
      call = call
    )
  }

  .check_solver_version(
    image = image,
    call = call
  )

  return(invisible(NULL))
}
