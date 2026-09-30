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
.o_accuracy_threshold <- function() {
  accuracy_threshold <- ems_option_get("accuracy_threshold")
  return(accuracy_threshold)
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
.o_docker_tag <- function() {
  docker_tag <- ems_option_get("docker_tag")
  return(docker_tag)
}

#' @noRd
#' @keywords internal
.o_version_check <- function() {
  version_check <- ems_option_get("version_check")
  return(version_check)
}

#' @noRd
#' @keywords internal
.o_assertions <- function() {
  assertions <- ems_option_get("assertions")
  return(assertions)
}

#' @noRd
#' @keywords internal
.o_range_test_initial <- function() {
  range_test_initial <- ems_option_get("range_test_initial")
  return(range_test_initial)
}

#' @noRd
#' @keywords internal
.o_range_test_updated <- function() {
  range_test_updated <- ems_option_get("range_test_updated")
  return(range_test_updated)
}

#' @noRd
#' @keywords internal
.o_random_seed <- function() {
  random_seed <- ems_option_get("random_seed")
  return(random_seed)
}

#' @noRd
#' @keywords internal
.o_refine <- function() {
  refine <- ems_option_get("refine")
  return(refine)
}
