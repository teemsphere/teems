#' @noRd  
#' @keywords internal
.write_shock <- function(shock,
                         write_path) {
  UseMethod(".write_shock")
}

#' @keywords internal
#' @noRd
#' @method .write_shock logical
#' @export
.write_shock.logical <- function(shock,
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
#' @method .write_shock uniform
#' @export
.write_shock.uniform <- function(shock,
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
#' @method .write_shock user
#' @export
.write_shock.user <- function(shock,
                              write_path) {
  cat(
    shock,
    file = write_path,
    append = TRUE
  )

  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .write_shock custom
#' @export
.write_shock.custom <- function(shock,
                                write_path) {

  NextMethod()
}

#' @keywords internal
#' @noRd
#' @method .write_shock full
#' @export
.write_shock.full <- function(shock,
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
#' @method .write_shock ele
#' @export
.write_shock.ele <- function(shock,
                             write_path) {
  
  con <- file(write_path, open = "a")
  writeLines(shock$ele, con, sep = "\n\n")
  close(con)
  
  return(write_path)
}