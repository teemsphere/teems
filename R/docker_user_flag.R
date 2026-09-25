#' @keywords internal
#' @noRd
.docker_user_flag <- function(sysname = Sys.info()[["sysname"]]) {
  if (!sysname %=% "Linux") {
    return("")
  }
  uid <- system2("id", "-u", stdout = TRUE)
  gid <- system2("id", "-g", stdout = TRUE)
  flag <- paste0("--user ", uid, ":", gid, " -e HOME=/tmp ")
  return(flag)
}
