#' @title Set advanced package options
#' @export
#' @description `ems_option_set()` allows the user to customize
#'   advanced options.
#' @return `invisible(NULL)`, called for its side effects.
#' @param tempdir A character vector length 1. Default is what is
#'   returned by `tempdir()`. Directory where input files, model
#'   diagnostics, and output files are written as well as where
#'   the model run takes place (via Docker bind mount).
#' @param verbose Logical of length 1 (default is `TRUE`). If
#'   `FALSE`, function-specific diagnostics are silenced.
#' @param ndigits Integer (default is `6`). Number of digits to
#'   the right of the decimal point kept when numeric type double
#'   data are written to file. Values smaller than 0.1 in
#'   magnitude keep `ndigits` significant digits instead, so
#'   small nonzero data are never rounded to zero.
#' @param accuracy_threshold Numeric length 1 (default `0.8`).
#'   The share of results that must be accurate to at least
#'   `accuracy_figures` significant figures under
#'   `accuracy_criterion`; a warning is generated if the share is
#'   lower.
#' @param accuracy_figures Integer length 1 from 1 to 6 (default
#'   `4`). The significant figures a result must reach to count as
#'   accurate.
#' @param accuracy_criterion Character length 1 (default
#'   `"solution"`). Which accuracy summary is judged: `"solution"`
#'   the levels values of the endogenous results, `"data"` the
#'   values read from data files and updated, `"both"` the two
#'   together. The criteria of GEMPACK's automatic accuracy (GEMPACK
#'   manual 26.4), whose default is `"data"`. A run that updates no
#'   data is judged on the solution under `"data"` and `"both"`.
#' @param required_figures Integer length 1 from 1 to 6 (default
#'   `NULL`, none). Results of a three-solution `"Gragg"`,
#'   `"Midpoint"` or `"Euler"` run whose two extrapolations agree to
#'   at least this many figures get the convergence code `XRA` in the
#'   accuracy summary (GEMPACK manual 26.2.6, RQF). Reporting only.
#' @param full_exclude A character vector of variable length
#'   (default is `c("DREL", "DVER", "XXCR", "XXCD", "XXCP",
#'   "SLUG", "EFLG")`). Headers to fully exclude from all aspects
#'   of the model run. Failure to designate these headers
#'   properly will result in errors. Modify with caution.
#' @param check_shock_status Logical of length 1 (default is
#'   `TRUE`). If `FALSE`, no check on the endogenous/exogenous
#'   status of a variable designated for a shock is conducted.
#' @param timestep_header A character vector length 1 (default is
#'   `"YEAR"`). Header written with the time of each period, as
#'   given in the `time_steps` argument of [`ems_data()`] (years,
#'   or steps from the initial period); the model takes the
#'   differences between consecutive periods as interval lengths.
#'   For novel intertemporal models — modify with caution.
#' @param n_timestep_header A character vector length 1 (default
#'   is `"NTSP"`). Header written with the number of periods (the
#'   length of `time_steps`). For novel intertemporal models —
#'   modify with caution.
#' @param docker_tag Character length 1 (default `"latest"`). Tag
#'   of the `teems` Docker image to use.
#' @param assertions Character length 1, `"fatal"` (default),
#'   `"warn"` or `"off"`. Severity of TAB `Assertion` statement
#'   failures: `"warn"` reports and continues, `"off"` skips the
#'   checks.
#' @param range_test_initial Character length 1, `"fatal"`
#'   (default), `"warn"` or `"off"`. Severity of declared-range
#'   violations (e.g. `(ge 0)`) on initial values: every
#'   coefficient, read or computed, is tested once the initial
#'   Reads and Formulas are done.
#' @param range_test_updated Character length 1, `"warn"`
#'   (default), `"fatal"` or `"off"`. As `range_test_initial`,
#'   for updated values (`range test updated values`).
#' @param random_seed Integer (default is `1`). Seed of the TAB
#'   function `RANDOM(a,b)`. The same seed reproduces every draw;
#'   each Formula element keeps its draw across steps and
#'   extrapolation passes. Change the seed for a new sequence.
#' @param refine Logical length 1 (default `TRUE`). One step of
#'   iterative refinement after every linear solve of
#'   `matrix_method = "DBBD"` in [`ems_solve()`], which brings its
#'   accuracy to that of `"LU"`. Refinement holds the system matrix
#'   and the block factorizations through each solve, so it raises
#'   peak memory; the memory estimate counts it. `"LU"` and
#'   `"SBBD"` are backward-stable and need no refinement;
#'   `"NDBBD"` is not refined, as it cannot yet keep its
#'   factorization for a second solve.
#' @param converge Logical length 1 (default `FALSE`). If `TRUE`,
#'   [`ems_solve()`] applies GEMPACK's treatment of poorly
#'   converging results (see GEMPACK manual 26.2.5) to a run
#'   extrapolated from three multi-step solutions: a component
#'   whose three solutions lie very close together or near zero
#'   reports their average, one whose solutions do not converge
#'   cleanly reports the solution with the most steps, and the rest
#'   are extrapolated. Not available with subtotals.
#' @seealso [`ems_option_get()`] for retrieving package options.
#'   [`ems_option_reset()`] for resetting package options.
#' @examples
#' # Set multiple options
#' ems_option_set(verbose = FALSE, ndigits = 8)
#' 
#' # Retrieve value of `tempdir`
#' ems_option_get("tempdir")
#' 
#' # Reset options to default values
#' ems_option_reset()
ems_option_set <- function(tempdir = NULL,
                           verbose = NULL,
                           ndigits = NULL,
                           accuracy_threshold = NULL,
                           accuracy_figures = NULL,
                           accuracy_criterion = NULL,
                           required_figures = NULL,
                           full_exclude = NULL,
                           check_shock_status = NULL,
                           timestep_header = NULL,
                           n_timestep_header = NULL,
                           docker_tag = NULL,
                           assertions = NULL,
                           range_test_initial = NULL,
                           range_test_updated = NULL,
                           random_seed = NULL,
                           refine = NULL,
                           converge = NULL) {
  call <- match.call()
  if (!is.null(tempdir)) {
    ems_options$set_tempdir(tempdir, call = call)
  }
  if (!is.null(verbose)) {
    ems_options$set_verbose(verbose, call = call)
  }
  if (!is.null(ndigits)) {
    ems_options$set_ndigits(ndigits, call = call)
  }
  if (!is.null(accuracy_threshold)) {
    ems_options$set_accuracy_threshold(accuracy_threshold, call = call)
  }
  if (!is.null(accuracy_figures)) {
    ems_options$set_accuracy_figures(accuracy_figures, call = call)
  }
  if (!is.null(accuracy_criterion)) {
    ems_options$set_accuracy_criterion(accuracy_criterion, call = call)
  }
  if (!is.null(required_figures)) {
    ems_options$set_required_figures(required_figures, call = call)
  }
  if (!is.null(full_exclude)) {
    ems_options$set_full_exclude(full_exclude, call = call)
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
  if (!is.null(docker_tag)) {
    ems_options$set_docker_tag(docker_tag, call = call)
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
  if (!is.null(refine)) {
    ems_options$set_refine(refine, call = call)
  }
  if (!is.null(converge)) {
    ems_options$set_converge(converge, call = call)
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
#'   * `"tempdir"` Character. Directory used for temporary
#'     file storage during a model run.
#'   * `"verbose"` Logical. If `FALSE`, function-specific
#'     diagnostics are silenced.
#'   * `"ndigits"` Integer. Number of digits to the right of
#'     the decimal point written to file for numeric type double
#'     (at least `ndigits` significant digits for small values).
#'   * `"accuracy_threshold"` Numeric. Share of results that must
#'     be accurate to at least `accuracy_figures` figures.
#'   * `"accuracy_figures"` Integer. Significant figures a result
#'     must reach to count as accurate (the default is `4`).
#'   * `"accuracy_criterion"` Character. `"solution"` (the
#'     default), `"data"` or `"both"`: the accuracy summary judged.
#'   * `"required_figures"` Integer or `NULL` (the default). Figures
#'     for the convergence code `XRA`.
#'   * `"full_exclude"` Character vector. Headers to
#'     fully exclude from all aspects of the model run.
#'   * `"check_shock_status"` Logical. If `FALSE`, no
#'     check on shock element endogenous/exogenous status is
#'     conducted.
#'   * `"timestep_header"` Character. Header written with the
#'     time of each period (`time_steps` of [`ems_data()`]).
#'   * `"n_timestep_header"` Character. Header written with the
#'     number of periods.
#'   * `"docker_tag"` Character. Tag of the `teems` Docker image
#'     to use. `"latest"` when unset.
#'   * `"assertions"` Character. `"fatal"` (the default), `"warn"`
#'     or `"off"`: severity of TAB `Assertion` failures in
#'     [`ems_solve()`].
#'   * `"range_test_initial"` Character. `"fatal"` (the default),
#'     `"warn"` or `"off"`: severity of declared-range violations on
#'     initial values.
#'   * `"range_test_updated"` Character. `"warn"` (the default),
#'     `"fatal"` or `"off"`: as `"range_test_initial"`, for updated
#'     values.
#'   * `"random_seed"` Integer. Seed of `RANDOM(a,b)` (the default
#'     is `1`).
#'   * `"refine"` Logical. `TRUE` (the default): one refinement
#'     step per `"DBBD"` solve (see
#'     [`ems_option_set()`]).
#'   * `"converge"` Logical. `FALSE` (the default); `TRUE`
#'     applies GEMPACK's treatment of poorly converging results (see
#'     [`ems_option_set()`]).
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
         tempdir            = ems_options$get_tempdir(),
         verbose            = ems_options$get_verbose(),
         ndigits            = ems_options$get_ndigits(),
         accuracy_threshold = ems_options$get_accuracy_threshold(),
         accuracy_figures   = ems_options$get_accuracy_figures(),
         accuracy_criterion = ems_options$get_accuracy_criterion(),
         required_figures   = ems_options$get_required_figures(),
         full_exclude       = ems_options$get_full_exclude(),
         check_shock_status = ems_options$get_check_shock_status(),
         timestep_header    = ems_options$get_timestep_header(),
         n_timestep_header  = ems_options$get_n_timestep_header(),
         docker_tag         = ems_options$get_docker_tag(),
         assertions         = ems_options$get_assertions(),
         range_test_initial = ems_options$get_range_test_initial(),
         range_test_updated = ems_options$get_range_test_updated(),
         random_seed        = ems_options$get_random_seed(),
         refine             = ems_options$get_refine(),
         converge           = ems_options$get_converge()
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
