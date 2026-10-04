#' @keywords internal
#' @noRd
.docker_user_flag <- function(sysname = Sys.info()[["sysname"]],
                              rootless = .docker_rootless()) {
  if (!sysname %=% "Linux" || isTRUE(rootless)) {
    return("")
  }
  uid <- system2("id", "-u", stdout = TRUE)
  gid <- system2("id", "-g", stdout = TRUE)
  flag <- paste0("--user ", uid, ":", gid, " -e HOME=/tmp ")
  return(flag)
}

#' @keywords internal
#' @noRd
.docker_rootless <- function() {
  quiet <- \(args) suppressWarnings(tryCatch(
    system2("docker", args, stdout = TRUE, stderr = TRUE),
    error = \(e) character(0)
  ))
  podman <- any(grepl("podman", quiet("--version"), ignore.case = TRUE))
  security <- any(grepl("rootless", quiet(c("info", "--format", "{{json .SecurityOptions}}")), ignore.case = TRUE))
  rootless <- security || podman
  return(rootless)
}
