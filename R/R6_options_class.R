#' @importFrom R6 R6Class
#' @importFrom rlang is_integerish caller_env
#' @importFrom cli cli_abort
#' @noRd
#' @keywords internal
options_class <- R6::R6Class(
  classname = "ems_option",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    verbose = NULL,
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
    random_seed = NULL,

    initialize = function(verbose = NULL,
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
      self$verbose <- verbose
      self$tempdir <- tempdir
      self$ndigits <- ndigits
      self$accuracy_threshold <- accuracy_threshold
      self$check_shock_status <- check_shock_status
      self$timestep_header <- timestep_header
      self$n_timestep_header <- n_timestep_header
      self$full_exclude <- full_exclude
      self$docker_tag <- docker_tag
      self$version_check <- version_check
      self$assertions <- assertions
      self$range_test_initial <- range_test_initial
      self$range_test_updated <- range_test_updated
      self$random_seed <- random_seed
    },

    export = function() {
      list(
        verbose = self$get_verbose(),
        tempdir = self$get_tempdir(),
        ndigits = self$get_ndigits(),
        accuracy_threshold = self$get_accuracy_threshold(),
        check_shock_status = self$get_check_shock_status(),
        timestep_header = self$get_timestep_header(),
        n_timestep_header = self$get_n_timestep_header(),
        full_exclude = self$get_full_exclude(),
        docker_tag = self$get_docker_tag(),
        version_check = self$get_version_check(),
        assertions = self$get_assertions(),
        range_test_initial = self$get_range_test_initial(),
        range_test_updated = self$get_range_test_updated(),
        random_seed = self$get_random_seed()
      )
    },

    import = function(list) {
      self$set_verbose(list$verbose)
      self$set_tempdir(list$tempdir)
      self$set_ndigits(list$ndigits)
      self$set_accuracy_threshold(list$accuracy_threshold)
      self$set_check_shock_status(list$check_shock_status)
      self$set_timestep_header(list$timestep_header)
      self$set_n_timestep_header(list$n_timestep_header)
      self$set_full_exclude(list$full_exclude)
      self$set_docker_tag(list$docker_tag)
      self$set_version_check(list$version_check)
      self$set_assertions(list$assertions)
      self$set_range_test_initial(list$range_test_initial)
      self$set_range_test_updated(list$range_test_updated)
      self$set_random_seed(list$random_seed)
    },

    reset = function() {
      self$verbose <- NULL
      self$tempdir <- NULL
      self$ndigits <- NULL
      self$accuracy_threshold <- NULL
      self$check_shock_status <- NULL
      self$timestep_header <- NULL
      self$n_timestep_header <- NULL
      self$full_exclude <- NULL
      self$docker_tag <- NULL
      self$version_check <- NULL
      self$assertions <- NULL
      self$range_test_initial <- NULL
      self$range_test_updated <- NULL
      self$random_seed <- NULL
    },

    get_verbose = function() {
      self$verbose %|||% TRUE
    },

    get_tempdir = function() {
      self$tempdir %|||% tempdir()
    },

    get_ndigits = function() {
      self$ndigits %|||% 6L
    },

    get_random_seed = function() {
      self$random_seed %|||% 1L
    },

    get_accuracy_threshold = function() {
      self$accuracy_threshold %|||% 0.8
    },

    get_check_shock_status = function() {
      self$check_shock_status %|||% TRUE
    },

    get_timestep_header = function() {
      self$timestep_header %|||% "YEAR"
    },

    get_n_timestep_header = function() {
      self$n_timestep_header %|||% "NTSP"
    },

    get_full_exclude = function() {
      self$full_exclude %|||% c("DREL", "DVER", "XXCR", "XXCD", "XXCP", "SLUG", "EFLG")
    },

    get_docker_tag = function() {
      self$docker_tag %|||% "latest"
    },

    get_version_check = function() {
      self$version_check %|||% "abort"
    },

    get_assertions = function() {
      self$assertions %|||% "fatal"
    },

    get_range_test_initial = function() {
      self$range_test_initial %|||% "warn"
    },

    get_range_test_updated = function() {
      self$range_test_updated %|||% "warn"
    },

    set_verbose = function(verbose, call = rlang::caller_env()) {
      self$validate_verbose(verbose, call = call)
      self$verbose <- verbose
    },

    set_tempdir = function(tempdir, call = rlang::caller_env()) {
      self$validate_tempdir(tempdir, call = call)
      self$tempdir <- tempdir
    },

    set_ndigits = function(ndigits, call = rlang::caller_env()) {
      self$validate_ndigits(ndigits, call = call)
      self$ndigits <- ndigits
    },

    set_random_seed = function(random_seed, call = rlang::caller_env()) {
      self$validate_random_seed(random_seed, call = call)
      self$random_seed <- as.integer(random_seed)
    },

    set_accuracy_threshold = function(accuracy_threshold, call = rlang::caller_env()) {
      self$validate_accuracy_threshold(accuracy_threshold, call = call)
      self$accuracy_threshold <- accuracy_threshold
    },

    set_check_shock_status = function(check_shock_status, call = rlang::caller_env()) {
      self$validate_check_shock_status(check_shock_status, call = call)
      self$check_shock_status <- check_shock_status
    },

    set_timestep_header = function(timestep_header, call = rlang::caller_env()) {
      self$validate_timestep_header(timestep_header, call = call)
      self$timestep_header <- timestep_header
    },

    set_n_timestep_header = function(n_timestep_header, call = rlang::caller_env()) {
      self$validate_n_timestep_header(n_timestep_header, call = call)
      self$n_timestep_header <- n_timestep_header
    },

    set_full_exclude = function(full_exclude, call = rlang::caller_env()) {
      self$validate_full_exclude(full_exclude, call = call)
      self$full_exclude <- full_exclude
    },

    set_docker_tag = function(docker_tag, call = rlang::caller_env()) {
      self$validate_docker_tag(docker_tag, call = call)
      self$docker_tag <- docker_tag
    },

    set_version_check = function(version_check, call = rlang::caller_env()) {
      self$validate_version_check(version_check, call = call)
      self$version_check <- version_check
    },

    set_assertions = function(assertions, call = rlang::caller_env()) {
      self$validate_assertions(assertions, call = call)
      self$assertions <- assertions
    },

    set_range_test_initial = function(range_test_initial, call = rlang::caller_env()) {
      self$validate_range_test_initial(range_test_initial, call = call)
      self$range_test_initial <- range_test_initial
    },

    set_range_test_updated = function(range_test_updated, call = rlang::caller_env()) {
      self$validate_range_test_updated(range_test_updated, call = call)
      self$range_test_updated <- range_test_updated
    },

    validate_verbose = function(verbose, call = rlang::caller_env()) {
      if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        cli::cli_abort(gen_err$opt_verbose, call = call)
      }
    },

    validate_tempdir = function(tempdir, call = rlang::caller_env()) {
      if (!is.character(tempdir) || length(tempdir) != 1 || !dir.exists(tempdir)) {
        cli::cli_abort(gen_err$opt_tempdir, call = call)
      }
    },

    validate_ndigits = function(ndigits, call = rlang::caller_env()) {
      if (!rlang::is_integerish(ndigits)) {
        cli::cli_abort(gen_err$opt_ndigits, call = call)
      }
    },

    validate_random_seed = function(random_seed, call = rlang::caller_env()) {
      if (!rlang::is_integerish(random_seed, n = 1L, finite = TRUE) ||
        random_seed < 0 || random_seed > .Machine$integer.max) {
        cli::cli_abort(gen_err$opt_random_seed, call = call)
      }
    },

    validate_accuracy_threshold = function(accuracy_threshold, call = rlang::caller_env()) {
      if (!is.numeric(accuracy_threshold) || accuracy_threshold > 1 || accuracy_threshold < 0) {
        cli::cli_abort(gen_err$opt_accuracy_threshold, call = call)
      }
    },

    validate_check_shock_status = function(check_shock_status, call = rlang::caller_env()) {
      if (!is.logical(check_shock_status) || length(check_shock_status) != 1 || is.na(check_shock_status)) {
        cli::cli_abort(gen_err$opt_check_shock_status, call = call)
      }
    },

    validate_timestep_header = function(timestep_header, call = rlang::caller_env()) {
      if (!is.character(timestep_header) || toupper(timestep_header) %!=% timestep_header) {
        cli::cli_abort(gen_err$opt_timestep_header, call = call)
      }
    },

    validate_n_timestep_header = function(n_timestep_header, call = rlang::caller_env()) {
      if (!is.character(n_timestep_header) || toupper(n_timestep_header) %!=% n_timestep_header) {
        cli::cli_abort(gen_err$opt_n_timestep_header, call = call)
      }
    },

    validate_full_exclude = function(full_exclude, call = rlang::caller_env()) {
      if (!is.character(full_exclude)) {
        cli::cli_abort(gen_err$opt_full_exclude, call = call)
      }
    },

    validate_docker_tag = function(docker_tag, call = rlang::caller_env()) {
      if (!is.character(docker_tag)) {
        cli::cli_abort(gen_err$opt_docker_tag, call = call)
      }
    },

    validate_version_check = function(version_check, call = rlang::caller_env()) {
      if (!is.character(version_check) || length(version_check) != 1L ||
        !version_check %in% c("abort", "warn", "off")) {
        cli::cli_abort(gen_err$opt_version_check, call = call)
      }
    },

    validate_assertions = function(assertions, call = rlang::caller_env()) {
      if (!is.character(assertions) || length(assertions) != 1L ||
        !assertions %in% c("fatal", "warn", "off")) {
        cli::cli_abort(gen_err$opt_assertions, call = call)
      }
    },

    validate_range_test_initial = function(range_test_initial, call = rlang::caller_env()) {
      if (!is.character(range_test_initial) || length(range_test_initial) != 1L ||
        !range_test_initial %in% c("fatal", "warn", "off")) {
        cli::cli_abort(gen_err$opt_range_test_initial, call = call)
      }
    },

    validate_range_test_updated = function(range_test_updated, call = rlang::caller_env()) {
      if (!is.character(range_test_updated) || length(range_test_updated) != 1L ||
        !range_test_updated %in% c("fatal", "warn", "off")) {
        cli::cli_abort(gen_err$opt_range_test_updated, call = call)
      }
    },

    validate = function() {
      self$validate_verbose(self$get_verbose())
      self$validate_tempdir(self$get_tempdir())
      self$validate_ndigits(self$get_ndigits())
      self$validate_accuracy_threshold(self$get_accuracy_threshold())
      self$validate_check_shock_status(self$get_check_shock_status())
      self$validate_timestep_header(self$get_timestep_header())
      self$validate_n_timestep_header(self$get_n_timestep_header())
      self$validate_full_exclude(self$get_full_exclude())
      self$validate_docker_tag(self$get_docker_tag())
      self$validate_version_check(self$get_version_check())
      self$validate_assertions(self$get_assertions())
      self$validate_range_test_initial(self$get_range_test_initial())
      self$validate_range_test_updated(self$get_range_test_updated())
    }
  )
)

options_new <- function(verbose = NULL,
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
                        range_test_updated = NULL) {
  return(options_class$new(
    verbose = verbose,
    tempdir = tempdir,
    ndigits = ndigits,
    accuracy_threshold = accuracy_threshold,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude,
    docker_tag = docker_tag,
    version_check = version_check,
    assertions = assertions,
    range_test_initial = range_test_initial,
    range_test_updated = range_test_updated
  ))
}

ems_options <- options_new()
