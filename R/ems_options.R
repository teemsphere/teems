#' Set advanced package options
#'
#' @description `ems_option_set()` allows the user to customize
#'   advanced options.
#'
#' @param verbose Logical of length 1 (default is `TRUE`). If
#'   `FALSE`, function-specific diagnostics are silenced.
#' @param write_sub_dir A character vector (default `"teems"`).
#'   Where [`ems_deploy()`] `"write_dir"` specifies a base
#'   directory for model files, `"write_sub_dir"` sets the name
#'   of the subdirectory within the base directory.
#' @param ndigits Integer (default is `6`). Exact number of
#'   digits to the right of the decimal point to be written to
#'   file for numeric type double.
#'   This value is passed to the `format()` nsmall argument and
#'   `round()` digits argument.
#' @param docker_tag Character length 1 (default `"latest"`).
#'   Docker tag specifying which Docker image to use.
#' @param accuracy_threshold Numeric length 1 (default `0.8`),
#'   converted to a percentage. 4-digit precision is compared
#'   against this threshold; a warning is generated if it is not
#'   met.
#' @param check_shock_status Logical of length 1 (default is
#'   `TRUE`). If `FALSE`, no check on shock element
#'   endogenous/exogenous status is conducted.
#' @param timestep_header A character vector length 1 (default is
#'   `"YEAR"`). Coefficient containing a numeric vector of
#'   timestep intervals. For novel intertemporal models — modify
#'   with caution.
#' @param n_timestep_header A character vector length 1 (default
#'   is `"NTSP"`). Coefficient containing a numeric vector length
#'   one with sum of timestep intervals. For novel intertemporal
#'   models — modify with caution.
#' @param full_exclude A character vector (default is `c("DREL",
#'   "DVER", "XXCR", "XXCD", "XXCP", "SLUG", "EFLG")`). Headers
#'   to fully exclude from all aspects of the model run. Failure
#'   to designate these headers properly will result in errors.
#'   Modify with caution.
#'
#' @return `invisible(NULL)`, called for its side effects.
#'
#' @seealso [`ems_option_get()`] for retrieving package options.
#'   [`ems_option_reset()`] for resetting package options.
#'
#' @examples
#' ems_option_set(verbose = FALSE)
#' ems_option_set(ndigits = 8, write_sub_dir = "my_run")
#' ems_option_reset()
#'
#' @importFrom cli cli_abort
#' @export
ems_option_set <- function(verbose = NULL,
                           write_sub_dir = NULL,
                           ndigits = NULL,
                           docker_tag = NULL,
                           accuracy_threshold = NULL,
                           check_shock_status = NULL,
                           timestep_header = NULL,
                           n_timestep_header = NULL,
                           full_exclude = NULL) {
  if (!is.null(verbose)) ems_options$set_verbose(verbose)
  if (!is.null(write_sub_dir)) ems_options$set_write_sub_dir(write_sub_dir)
  if (!is.null(ndigits)) ems_options$set_ndigits(ndigits)
  if (!is.null(docker_tag)) ems_options$set_docker_tag(docker_tag)
  if (!is.null(accuracy_threshold)) ems_options$set_accuracy_threshold(accuracy_threshold)
  if (!is.null(check_shock_status)) ems_options$set_check_shock_status(check_shock_status)
  if (!is.null(timestep_header)) ems_options$set_timestep_header(timestep_header)
  if (!is.null(n_timestep_header)) ems_options$set_n_timestep_header(n_timestep_header)
  if (!is.null(full_exclude)) ems_options$set_full_exclude(full_exclude)
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
#'   * `"write_sub_dir"` Character. Name of the
#'     subdirectory within the base directory set by
#'     [`ems_deploy()`].
#'   * `"ndigits"` Integer. Exact number of digits to the
#'     right of the decimal point written to file for numeric
#'     type double.
#'   * `"docker_tag"` Character. Docker tag specifying
#'     which Docker image to use.
#'   * `"accuracy_threshold"` Numeric. Threshold
#'     (converted to a percentage) against which 4-digit
#'     precision is compared.
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
#'
#' @return If `name` is `NULL`, a named list of all current option values.
#'   Otherwise, the value of the requested option.
#'
#' @seealso [`ems_option_set()`] for setting package options.
#'   [`ems_option_reset()`] for resetting package options.
#'
#' @examples
#' ems_option_get()
#' ems_option_get("ndigits")
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
         verbose            = ems_options$get_verbose(),
         write_sub_dir      = ems_options$get_write_sub_dir(),
         ndigits            = ems_options$get_ndigits(),
         docker_tag         = ems_options$get_docker_tag(),
         accuracy_threshold = ems_options$get_accuracy_threshold(),
         check_shock_status = ems_options$get_check_shock_status(),
         timestep_header    = ems_options$get_timestep_header(),
         n_timestep_header  = ems_options$get_n_timestep_header(),
         full_exclude       = ems_options$get_full_exclude()
  )
}

#' Reset to default package options
#'
#' @description `ems_option_reset()` resets all package
#'   options to default values.
#'   
#' @return `invisible(NULL)`, called for its side effects.
#'
#' @seealso [`ems_option_set()`] for setting package options.
#'   [`ems_option_get()`] for retrieving package options.
#'
#' @examples
#' ems_option_set(verbose = FALSE, ndigits = 8)
#' ems_option_reset()
#' ems_option_get("verbose")  # TRUE
#'
#' @export
ems_option_reset <- function() {
  ems_options$reset()
  invisible(NULL)
}
