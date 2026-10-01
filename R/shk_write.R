#' @noRd  
#' @keywords internal
.write_shk <- function(shock,
                       write_path) {
  UseMethod(".write_shk")
}

#' @keywords internal
#' @noRd
#' @method .write_shk logical
#' @export
.write_shk.logical <- function(shock,
                                 write_path) {
  cat("Shock ;",
    file = write_path,
    sep = "\n",
    append = FALSE
  )

  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .write_shk uniform
#' @export
.write_shk.uniform <- function(shock,
                                 write_path) {
  cat(shock$shock,
    file = write_path,
    sep = "\n",
    append = TRUE
  )
  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .write_shk user
#' @export
.write_shk.user <- function(shock,
                              write_path) {
  file.copy(shock, write_path)
  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .write_shk custom
#' @export
.write_shk.custom <- function(shock,
                                write_path) {

  NextMethod()
}

#' @keywords internal
#' @noRd
#' @method .write_shk full
#' @export
.write_shk.full <- function(shock,
                              write_path) {

  .shk_ragged_write(
    input = shock$dt,
    lead = attr(shock, "lead"),
    write_path = write_path
  )

  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .write_shk ele
#' @export
.write_shk.ele <- function(shock,
                             write_path) {
  
  con <- file(write_path, open = "a")
  writeLines(shock$ele, con, sep = "\n\n")
  close(con)
  
  return(write_path)
}
