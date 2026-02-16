#' Set advanced package options
#'
#' @description `ems_option_set()` allows the user to customize
#'   advanced options.
#'
#' @param verbose Logical of length 1 (default is `TRUE`). If
#'   `FALSE`, function-specific diagnostics are silenced.
#' @param ndigits Integer (default is `6`). Exact number of
#'   digits to the right of the decimal point to be written to
#'   file for numeric type double (GEMPack equivalent "real").
#'   This value is passed to the `format()` nsmall argument and
#'   `round()` digits argument.
#' @param check_shock_status Logical of length 1 (default is
#'   `TRUE`). If `FALSE`, no check on shock element
#'   endogenous/exogenous status is conducted.
#' @param timestep_header A character vector length 1 (default is
#'   `"YEAR"`). Coefficient containing a numeric vector of
#'   timestep intervals. For novel intertemporal models - modify
#'   with caution.
#' @param n_timestep_header A character vector length 1 (default
#'   is `"NTSP"`). Coefficient containing a numeric vector length
#'   one with sum of timestep intervals. For novel intertemporal
#'   models - modify with caution.
#' @param full_exclude A character vector (default is `c("DREL",
#'   "DVER", "XXCR", "XXCD", "XXCP", "SLUG", "EFLG")`). Specifies
#'   headers to fully exclude from all aspects of the model run.
#'   Failure to designate these headers properly will result in
#'   errors because some headers cannot be aggregated or mapped.
#'   Modify with caution.
#' @param docker_tag Character length 1 (default `"latest"`).
#'   Docker tag to specify the which Docker image is use.
#' @param margin_sectors placeholder
#' @param accuracy_threshold Numeric length 1 (default 0.8),
#'   converted to a percentage. 4 digit precision will be
#'   compared against this threshold, generating a warning if it
#'   is not met.
#' @param expand_ETRE Logical of length 1 (default `TRUE`). If
#'   `FALSE`, no expansion to the ETRE (ETRAE) data header are
#'   carried out. When `TRUE`, if ETRE header data does not
#'   contain the full endowment set, missing tuples will be added
#'   with value -1e-05. The sluggish endowment set will be
#'   replaced with the general endowment set. ETRE header
#'   elements are not consistent across databases regarding
#'   inclusion of non-sluggish endowments, leading to issues with
#'   the ETRAE coefficient which is read-in with the full set of
#'   endowments.
#' @param write_sub_dir A character vector (default `"teems"`).
#'   Where [ems_deploy()] `"write_dir"` specifies a base
#'   directory for model files, `"write_sub_dir"` sets the name
#'   of the subdirectory within the base directory.
#'
#' @seealso [`ems_option_get()`] for retrieving package options.
#' @seealso [`ems_option_reset()`] for resetting package options.
#' 
#' @importFrom cli cli_abort
#' @export
ems_option_set <- function(verbose = NULL,
                           ndigits = NULL,
                           check_shock_status = NULL,
                           timestep_header = NULL,
                           n_timestep_header = NULL,
                           full_exclude = NULL,
                           docker_tag = NULL,
                           margin_sectors = NULL,
                           accuracy_threshold = NULL,
                           expand_ETRE = NULL,
                           write_sub_dir = NULL) {
  if (!is.null(verbose)) ems_options$set_verbose(verbose)
  if (!is.null(ndigits)) ems_options$set_ndigits(ndigits)
  if (!is.null(check_shock_status)) ems_options$set_check_shock_status(check_shock_status)
  if (!is.null(timestep_header)) ems_options$set_timestep_header(timestep_header)
  if (!is.null(n_timestep_header)) ems_options$set_n_timestep_header(n_timestep_header)
  if (!is.null(full_exclude)) ems_options$set_full_exclude(full_exclude)
  if (!is.null(docker_tag)) ems_options$set_docker_tag(docker_tag)
  if (!is.null(margin_sectors)) ems_options$set_margin_sectors(margin_sectors)
  if (!is.null(accuracy_threshold)) ems_options$set_accuracy_threshold(accuracy_threshold)
  if (!is.null(expand_ETRE)) ems_options$set_expand_ETRE(expand_ETRE)
  if (!is.null(write_sub_dir)) ems_options$set_write_sub_dir(write_sub_dir)
  invisible(NULL)
}

#' Get default package options
#'
#' @description `ems_option_get()` returns default package
#'   options. If `name` is `NULL` (the default), all option
#'   values are returned as a list.
#'
#' @param name Name of the option for which to retrieve a
#'   value. One of:
#'   * `NULL` Returns all option values.
#'   * `"verbose"` Logical. If `FALSE`, function-specific
#'     diagnostics are silenced.
#'   * `"ndigits"` Integer. Exact number of digits to the
#'     right of the decimal point written to file for numeric
#'     type double.
#'   * `"check_shock_status"` Logical. If `FALSE`, no
#'     check on shock element endogenous/exogenous status is
#'     conducted.
#'   * `"timestep_header"` Character. Coefficient
#'     containing a numeric vector of timestep intervals.
#'   * `"n_timestep_header"` Character. Coefficient
#'     containing a numeric vector length one with sum of
#'     timestep intervals.
#'   * `"full_exclude"` Character vector. Headers to
#'     fully exclude from all aspects of the model run.
#'   * `"docker_tag"` Character. Docker tag specifying
#'     which Docker image to use.
#'   * `"margin_sectors"` Character vector. Default
#'     margin sectors.
#'   * `"accuracy_threshold"` Numeric. Threshold
#'     (converted to a percentage) against which 4 digit
#'     precision is compared.
#'   * `"expand_ETRE"` Logical. If `TRUE`, missing
#'     tuples in the ETRE header are added with value
#'     -1e-05 and the sluggish endowment set is replaced
#'     with the general endowment set.
#'   * `"write_sub_dir"` Character. Name of the
#'     subdirectory within the base directory set by
#'     `ems_deploy()`.
#'
#' @seealso [`ems_option_set()`] for setting package options.
#' @seealso [`ems_option_reset()`] for resetting package options.
#' 
#' @importFrom cli cli_abort
#' @export
ems_option_get <- function(name = NULL) {
  if (is.null(name)) {
    # Return all options
    return(ems_options$export())
  }
  
  # Validate name against ems_option_set formals
  valid <- names(formals(ems_option_set))
  if (!name %in% valid) {
    cli::cli_abort("{.arg name} must be one of {.val {valid}}, not {.val {name}}.")
  }

  # Return specific option
  switch(name,
         verbose = ems_options$get_verbose(),
         ndigits = ems_options$get_ndigits(),
         check_shock_status = ems_options$get_check_shock_status(),
         timestep_header = ems_options$get_timestep_header(),
         n_timestep_header = ems_options$get_n_timestep_header(),
         full_exclude = ems_options$get_full_exclude(),
         docker_tag = ems_options$get_docker_tag(),
         margin_sectors = ems_options$get_margin_sectors(),
         accuracy_threshold = ems_options$get_accuracy_threshold(),
         expand_ETRE = ems_options$get_expand_ETRE(),
         write_sub_dir = ems_options$get_write_sub_dir()
  )
}

#' Reset to default package options
#'
#' @description `ems_option_reset()` resets all package
#'   options to default values.
#'   
#' @seealso [`ems_option_set()`] for setting package options.
#' @seealso [`ems_option_get()`] for retrieving package options.
#' 
#' @export
ems_option_reset <- function() {
  ems_options$reset()
  invisible(NULL)
}
