#' @title Set advanced package options
#' @export
#' @description `ems_option_set()` allows the user to customize
#'   advanced options.
#' @return `invisible(NULL)`, called for its side effects.
#' @param verbose Logical of length 1 (default is `TRUE`). If
#'   `FALSE`, function-specific diagnostics are silenced.
#' @param tempdir A character vector length 1. Default is what is
#'   returned by `tempdir()`.
#' @param ndigits Integer (default is `6`). Number of digits to
#'   the right of the decimal point kept when numeric type double
#'   data are written to file. Values smaller than 0.1 in
#'   magnitude keep `ndigits` significant digits instead, so small
#'   nonzero data are never rounded to zero.
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
#' @param full_exclude A character vector of variable length
#'   (default is `c("DREL", "DVER", "XXCR", "XXCD", "XXCP",
#'   "SLUG", "EFLG")`). Headers to fully exclude from all aspects
#'   of the model run. Failure to designate these headers
#'   properly will result in errors. Modify with caution.
#' @param docker_tag Character length 1. Docker tag specifying
#'   which `teems` image to use. When unset, the tag is selected
#'   automatically: the highest CPU capability level the host
#'   supports (e.g. `"x86-64-v3"`, probed by running `ld.so` inside a
#'   local `teems` image, so the answer is the same on Linux, Windows
#'   and macOS hosts) with a matching local image `teems:<level>` (or
#'   its short form, e.g. `teems:v3`) is preferred, falling back to
#'   `"latest"`.
#' @param version_check Character length 1 (default `"abort"`). What
#'   the solve pre-flight does with the image's answer to
#'   `teems-solver -version`, asked once per image per session. The
#'   solver's major version must equal this package's (the interface
#'   contract) and be at least the package's minimum (`1.1.0`, the
#'   first solver that answers at all); `"abort"` stops on an old,
#'   silent or mismatched image, `"warn"` reports it and runs anyway
#'   (bit-exact reproduction against a pinned old image), `"off"`
#'   never asks.
#' @param assertions Character length 1, `"fatal"` (default),
#'   `"warn"` or `"off"`. Severity of TAB `Assertion` statement
#'   failures in [`ems_solve()`] (GEMPACK manual 25.3): `"warn"`
#'   reports and continues, `"off"` skips the checks.
#' @param range_test_initial Character length 1, `"warn"` (default),
#'   `"fatal"` or `"off"`. Severity of declared-range violations
#'   (e.g. `(ge 0)`) on initial values in [`ems_solve()`] (GEMPACK
#'   manual 25.4.4, `range test initial values`; GEMPACK's
#'   `yes`/`warn`/`no` are `"fatal"`/`"warn"`/`"off"` here).
#' @param range_test_updated Character length 1, `"warn"` (default),
#'   `"fatal"` or `"off"`. As `range_test_initial`, for updated
#'   values (`range test updated values`).
#' @param random_seed Integer (default is `1`). Seed of the TAB
#'   function `RANDOM(a,b)` (GEMPACK manual 11.5.2) in
#'   [`ems_solve()`]. The same seed reproduces every draw; each
#'   Formula element keeps its draw across steps and extrapolation
#'   passes. GEMPACK draws a new sequence per run by default
#'   (`randomize = yes`); change the seed for a new sequence.
#' @seealso [`ems_option_get()`] for retrieving package options.
#'   [`ems_option_reset()`] for resetting package options.
#' @examples
#' # Set multiple options
#' ems_option_set(verbose = FALSE, ndigits = 8)
#' 
#' # Retrieve value of `verbose`
#' ems_option_get("verbose")
#' 
#' # Reset options to default values
#' ems_option_reset()
ems_option_set <- function(verbose = NULL,
                           tempdir = NULL,
                           ndigits = NULL,
                           accuracy_threshold = NULL,
                           check_shock_status = NULL,
                           timestep_header = NULL,
                           n_timestep_header = NULL,
                           full_exclude = NULL,
                           docker_tag = NULL,
                           version_check = NULL,
                           assertions = NULL,
                           range_test_initial = NULL,
                           range_test_updated = NULL,
                           random_seed = NULL) {
  call <- match.call()
  if (!is.null(verbose)) {
    ems_options$set_verbose(verbose, call = call)
  }
  if (!is.null(tempdir)) {
    ems_options$set_tempdir(tempdir, call = call)
  }
  if (!is.null(ndigits)) {
    ems_options$set_ndigits(ndigits, call = call)
  }
  if (!is.null(accuracy_threshold)) {
    ems_options$set_accuracy_threshold(accuracy_threshold, call = call)
  }
  if (!is.null(check_shock_status)) {
    ems_options$set_check_shock_status(check_shock_status, call = call)
  }
  if (!is.null(timestep_header)) {
    ems_options$set_timestep_header(timestep_header, call = call)
  }
  if (!is.null(n_timestep_header)) {
    ems_options$set_n_timestep_header(n_timestep_header, call = call)
  }
  if (!is.null(full_exclude)) {
    ems_options$set_full_exclude(full_exclude, call = call)
  }
  if (!is.null(docker_tag)) {
    ems_options$set_docker_tag(docker_tag, call = call)
  }
  if (!is.null(version_check)) {
    ems_options$set_version_check(version_check, call = call)
  }
  if (!is.null(assertions)) {
    ems_options$set_assertions(assertions, call = call)
  }
  if (!is.null(range_test_initial)) {
    ems_options$set_range_test_initial(range_test_initial, call = call)
  }
  if (!is.null(range_test_updated)) {
    ems_options$set_range_test_updated(range_test_updated, call = call)
  }
  if (!is.null(random_seed)) {
    ems_options$set_random_seed(random_seed, call = call)
  }
  invisible(NULL)
}

#' @title Get default package options
#' @export
#' @return If `name` is `NULL`, a named list of all current
#'   option values. Otherwise, the value of the requested option.
#' @description `ems_option_get()` returns default package
#'   options. If `name` is `NULL` (the default), all option
#'   values are returned as a list.
#' @param name Name of the option for which to retrieve a
#'   value. One of:
#'   * `NULL` Returns all option values.
#'   * `"verbose"` Logical. If `FALSE`, function-specific
#'     diagnostics are silenced.
#'   * `"tempdir"` Character. Directory used for temporary
#'     file storage during a model run.
#'   * `"ndigits"` Integer. Number of digits to the right of
#'     the decimal point written to file for numeric type double
#'     (at least `ndigits` significant digits for small values).
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
#'   * `"docker_tag"` Character. Docker tag specifying
#'     which Docker image to use. `"latest"` when unset; note
#'     the solve command auto-selects a host-matched variant
#'     tag when the option is unset (see [`ems_option_set()`]).
#'   * `"version_check"` Character. `"abort"` (the default),
#'     `"warn"` or `"off"`: what the solve pre-flight does with the
#'     image's solver version (see [`ems_option_set()`]).
#'   * `"assertions"` Character. `"fatal"` (the default), `"warn"`
#'     or `"off"`: severity of TAB `Assertion` failures in
#'     [`ems_solve()`].
#'   * `"range_test_initial"` Character. `"warn"` (the default),
#'     `"fatal"` or `"off"`: severity of declared-range violations on
#'     initial values.
#'   * `"range_test_updated"` Character. `"warn"` (the default),
#'     `"fatal"` or `"off"`: as `"range_test_initial"`, for updated
#'     values.
#'   * `"random_seed"` Integer. Seed of `RANDOM(a,b)` (the default
#'     is `1`).
#' @seealso [`ems_option_set()`] for setting package options.
#'   [`ems_option_reset()`] for resetting package options.
#' @examples
#' # Retrieve all options values
#' ems_option_get()
#' 
#' # Retrieve option value for `ndigits`
#' ems_option_get("ndigits")
#' @importFrom cli cli_abort
ems_option_get <- function(name = NULL) {
  if (is.null(name)) {
    return(ems_options$export())
  }
  
  valid <- names(formals(ems_option_set))
  if (!name %in% valid) {
    cli::cli_abort(gen_err$opt_name)
  }

  switch(name,
         verbose            = ems_options$get_verbose(),
         tempdir            = ems_options$get_tempdir(),
         ndigits            = ems_options$get_ndigits(),
         accuracy_threshold = ems_options$get_accuracy_threshold(),
         check_shock_status = ems_options$get_check_shock_status(),
         timestep_header    = ems_options$get_timestep_header(),
         n_timestep_header  = ems_options$get_n_timestep_header(),
         full_exclude       = ems_options$get_full_exclude(),
         docker_tag         = ems_options$get_docker_tag(),
         version_check      = ems_options$get_version_check(),
         assertions         = ems_options$get_assertions(),
         range_test_initial = ems_options$get_range_test_initial(),
         range_test_updated = ems_options$get_range_test_updated(),
         random_seed        = ems_options$get_random_seed()
  )
}

#' @title Reset to default package options
#' @export
#' @description `ems_option_reset()` resets all package options
#'   to default values.
#' @return `invisible(NULL)`, called for its side effects.
#' @seealso [`ems_option_set()`] for setting package options.
#'   [`ems_option_get()`] for retrieving package options.
#' @examples
#' # Set multiple options
#' ems_option_set(verbose = FALSE, ndigits = 8)
#' 
#' # Retrieve modified option value for `verbose`
#' ems_option_get("verbose")
#' 
#' # Reset options to default values
#' ems_option_reset()
#' 
#' # Retrieve default option value for `verbose`
#' ems_option_get("verbose")
ems_option_reset <- function() {
  ems_options$reset()
  invisible(NULL)
}
