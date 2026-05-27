#' @importFrom R6 R6Class
#' @importFrom rlang is_integerish caller_env
#' @importFrom cli cli_abort
#'
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

    initialize = function(verbose = NULL,
                          tempdir = NULL,
                          ndigits = NULL,
                          accuracy_threshold = NULL,
                          check_shock_status = NULL,
                          timestep_header = NULL,
                          n_timestep_header = NULL,
                          full_exclude = NULL,
                          docker_tag = NULL) {
      self$verbose <- verbose
      self$tempdir <- tempdir
      self$ndigits <- ndigits
      self$accuracy_threshold <- accuracy_threshold
      self$check_shock_status <- check_shock_status
      self$timestep_header <- timestep_header
      self$n_timestep_header <- n_timestep_header
      self$full_exclude <- full_exclude
      self$docker_tag <- docker_tag
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
        docker_tag = self$get_docker_tag()
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

    validate_verbose = function(verbose, call = rlang::caller_env()) {
      if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        cli::cli_abort("{.arg verbose} must be TRUE or FALSE.", call = call)
      }
    },

    validate_tempdir = function(tempdir, call = rlang::caller_env()) {
      if (!is.character(tempdir) || length(tempdir) != 1 || !dir.exists(tempdir)) {
        cli::cli_abort("{.path {tempdir}} does not exist.", call = call)
      }
    },

    validate_ndigits = function(ndigits, call = rlang::caller_env()) {
      if (!rlang::is_integerish(ndigits)) {
        cli::cli_abort("{.arg ndigits} must be an integer or coercible to one.", call = call)
      }
    },

    validate_accuracy_threshold = function(accuracy_threshold, call = rlang::caller_env()) {
      if (!is.numeric(accuracy_threshold) || accuracy_threshold > 1 || accuracy_threshold < 0) {
        cli::cli_abort("{.arg accuracy_threshold} must be a numeric between 0 and 1.", call = call)
      }
    },

    validate_check_shock_status = function(check_shock_status, call = rlang::caller_env()) {
      if (!is.logical(check_shock_status) || length(check_shock_status) != 1 || is.na(check_shock_status)) {
        cli::cli_abort("{.arg check_shock_status} must be TRUE or FALSE.", call = call)
      }
    },

    validate_timestep_header = function(timestep_header, call = rlang::caller_env()) {
      if (!is.character(timestep_header) || toupper(timestep_header) %!=% timestep_header) {
        cli::cli_abort("{.arg timestep_header} must be an upper case character vector.", call = call)
      }
    },

    validate_n_timestep_header = function(n_timestep_header, call = rlang::caller_env()) {
      if (!is.character(n_timestep_header) || toupper(n_timestep_header) %!=% n_timestep_header) {
        cli::cli_abort("{.arg n_timestep_header} must be an upper case character vector.", call = call)
      }
    },

    validate_full_exclude = function(full_exclude, call = rlang::caller_env()) {
      if (!is.character(full_exclude)) {
        cli::cli_abort("{.arg full_exclude} must be a character vector.", call = call)
      }
    },

    validate_docker_tag = function(docker_tag, call = rlang::caller_env()) {
      if (!is.character(docker_tag)) {
        cli::cli_abort("{.arg docker_tag} must be a character vector.", call = call)
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
                        docker_tag = NULL) {
  options_class$new(
    verbose = verbose,
    tempdir = tempdir,
    ndigits = ndigits,
    accuracy_threshold = accuracy_threshold,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude,
    docker_tag = docker_tag
  )
}

ems_options <- options_new()
