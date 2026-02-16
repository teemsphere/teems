#' @noRd
#' @keywords internal
.o_verbose <- function() {
  ems_option_get("verbose")
}

#' @noRd
#' @keywords internal
.o_ndigits <- function() {
  ems_option_get("ndigits")
}

#' @noRd
#' @keywords internal
.o_check_shock_status <- function() {
  ems_option_get("check_shock_status")
}

#' @noRd
#' @keywords internal
.o_timestep_header <- function() {
  ems_option_get("timestep_header")
}

#' @noRd
#' @keywords internal
.o_n_timestep_header <- function() {
  ems_option_get("n_timestep_header")
}

#' @noRd
#' @keywords internal
.o_full_exclude <- function() {
  ems_option_get("full_exclude")
}

#' @noRd
#' @keywords internal
.o_docker_tag <- function() {
  ems_option_get("docker_tag")
}

#' @noRd
#' @keywords internal
.o_margin_sectors <- function() {
  ems_option_get("margin_sectors")
}

#' @noRd
#' @keywords internal
.o_accuracy_threshold <- function() {
  ems_option_get("accuracy_threshold")
}

#' @noRd
#' @keywords internal
.o_expand_ETRE <- function() {
  ems_option_get("expand_ETRE")
}

#' @noRd
#' @keywords internal
.o_write_sub_dir <- function() {
  ems_option_get("write_sub_dir")
}