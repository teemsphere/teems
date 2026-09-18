#' @noRd
#' @keywords internal
.o_verbose <- function() {
  verbose <- ems_option_get("verbose")
  return(verbose)
}

#' @noRd
#' @keywords internal
.o_tempdir <- function() {
  dir <- ems_option_get("tempdir")
  return(dir)
}

#' @noRd
#' @keywords internal
.o_ndigits <- function() {
  ndigits <- ems_option_get("ndigits")
  return(ndigits)
}

#' @noRd
#' @keywords internal
.o_check_shock_status <- function() {
  shock_status <- ems_option_get("check_shock_status")
  return(shock_status)
}

#' @noRd
#' @keywords internal
.o_timestep_header <- function() {
  timestep_header <- ems_option_get("timestep_header")
  return(timestep_header)
}

#' @noRd
#' @keywords internal
.o_n_timestep_header <- function() {
  n_timestep_header <- ems_option_get("n_timestep_header")
  return(n_timestep_header)
}

#' @noRd
#' @keywords internal
.o_full_exclude <- function() {
  full_exclude <- ems_option_get("full_exclude")
  return(full_exclude)
}

#' @noRd
#' @keywords internal
.o_accuracy_threshold <- function() {
  accuracy_threshold <- ems_option_get("accuracy_threshold")
  return(accuracy_threshold)
}

#' @noRd
#' @keywords internal
.o_write_sub_dir <- function() {
  sub_dir <- ems_option_get("write_sub_dir")
  return(sub_dir)
}