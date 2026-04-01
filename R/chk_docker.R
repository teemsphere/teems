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

  docker_access <- system2("docker", "info", stdout = FALSE, stderr = FALSE) == 0
  if (!docker_access) {
    if (Sys.info()[["sysname"]] == "Linux") {
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

  image_out <- system2("docker", c("images", "-q", image_name), stdout = TRUE, stderr = FALSE)
  if (!any(nzchar(image_out))) {
    .cli_action(solve_err$docker_x_image,
      action = "abort",
      url = "https://github.com/teemsphere/teems-solver",
      hyperlink = "teems-solver repository",
      call = call
    )
  }

  return(invisible(NULL))
}