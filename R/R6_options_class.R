#' @importFrom R6 R6Class
#' 
#' @noRd
#' @keywords internal
options_class <- R6::R6Class(
  classname = "ems_option",
  class = FALSE,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    # Option fields (start as NULL)
    verbose = NULL,
    ndigits = NULL,
    check_shock_status = NULL,
    timestep_header = NULL,
    n_timestep_header = NULL,
    full_exclude = NULL,
    docker_tag = NULL,
    margin_sectors = NULL,
    accuracy_threshold = NULL,
    expand_ETRE = NULL,
    write_sub_dir = NULL,

    # Initialize method
    initialize = function(verbose = NULL,
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
      self$verbose <- verbose
      self$ndigits <- ndigits
      self$check_shock_status <- check_shock_status
      self$timestep_header <- timestep_header
      self$n_timestep_header <- n_timestep_header
      self$full_exclude <- full_exclude
      self$docker_tag <- docker_tag
      self$margin_sectors <- margin_sectors
      self$accuracy_threshold <- accuracy_threshold
      self$expand_ETRE <- expand_ETRE
      self$write_sub_dir <- write_sub_dir
    },
    
    # Export all options as a list
    export = function() {
      list(
        verbose = self$get_verbose(),
        ndigits = self$get_ndigits(),
        check_shock_status = self$get_check_shock_status(),
        timestep_header = self$get_timestep_header(),
        n_timestep_header = self$get_n_timestep_header(),
        full_exclude = self$get_full_exclude(),
        docker_tag = self$get_docker_tag(),
        margin_sectors = self$get_margin_sectors(),
        accuracy_threshold = self$get_accuracy_threshold(),
        expand_ETRE = self$get_expand_ETRE(),
        write_sub_dir = self$get_write_sub_dir()
      )
    },
    
    # Import options from a list
    import = function(list) {
      self$set_verbose(list$verbose)
      self$set_ndigits(list$ndigits)
      self$set_check_shock_status(list$check_shock_status)
      self$set_timestep_header(list$timestep_header)
      self$set_n_timestep_header(list$n_timestep_header)
      self$set_full_exclude(list$full_exclude)
      self$set_docker_tag(list$docker_tag)
      self$set_margin_sectors(list$margin_sectors)
      self$set_accuracy_threshold(list$accuracy_threshold)
      self$set_expand_ETRE(list$expand_ETRE)
      self$set_write_sub_dir(list$write_sub_dir)
    },
    
    # Reset all options to NULL
    reset = function() {
      self$verbose <- NULL
      self$ndigits <- NULL
      self$check_shock_status <- NULL
      self$timestep_header <- NULL
      self$n_timestep_header <- NULL
      self$full_exclude <- NULL
      self$docker_tag <- NULL
      self$margin_sectors <- NULL
      self$accuracy_threshold <- NULL
      self$expand_ETRE <- NULL
      self$write_sub_dir <- NULL
    },
    
    # Getter methods with defaults (using %|||% operator)
    get_verbose = function() {
      self$verbose %|||% TRUE
    },
    
    get_ndigits = function() {
      self$ndigits %|||% 6L
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
    
    get_margin_sectors = function() {
      self$margin_sectors %|||% c("atp", "otp", "wtp")
    },
    
    get_accuracy_threshold = function() {
      self$accuracy_threshold %|||% 0.8
    },
    
    get_expand_ETRE = function() {
      self$expand_ETRE %|||% TRUE
    },
    
    get_write_sub_dir = function() {
      self$write_sub_dir %|||% "teems"
    },
    
    # Setter methods with validation
    set_verbose = function(verbose) {
      if (!is.null(verbose)) {
        self$validate_verbose(verbose)
      }
      self$verbose <- verbose
    },
    
    set_ndigits = function(ndigits) {
      if (!is.null(ndigits)) {
        self$validate_ndigits(ndigits)
      }
      self$ndigits <- ndigits
    },
    
    set_check_shock_status = function(check_shock_status) {
      if (!is.null(check_shock_status)) {
        self$validate_check_shock_status(check_shock_status)
      }
      self$check_shock_status <- check_shock_status
    },

    set_timestep_header = function(timestep_header) {
      if (!is.null(timestep_header)) {
        self$validate_timestep_header(timestep_header)
      }
      self$timestep_header <- timestep_header
    },

    set_n_timestep_header = function(n_timestep_header) {
      if (!is.null(n_timestep_header)) {
        self$validate_n_timestep_header(n_timestep_header)
      }
      self$n_timestep_header <- n_timestep_header
    },
    
    set_full_exclude = function(full_exclude) {
      if (!is.null(full_exclude)) {
        self$validate_full_exclude(full_exclude)
      }
      self$full_exclude <- full_exclude
    },
    
    set_docker_tag = function(docker_tag) {
      if (!is.null(docker_tag)) {
        self$validate_docker_tag(docker_tag)
      }
      self$docker_tag <- docker_tag
    },
    
    set_margin_sectors = function(margin_sectors) {
      if (!is.null(margin_sectors)) {
        self$validate_margin_sectors(margin_sectors)
      }
      self$margin_sectors <- margin_sectors
    },
    
    set_accuracy_threshold = function(accuracy_threshold) {
      if (!is.null(accuracy_threshold)) {
        self$validate_accuracy_threshold(accuracy_threshold)
      }
      self$accuracy_threshold <- accuracy_threshold
    },
    
    set_expand_ETRE = function(expand_ETRE) {
      if (!is.null(expand_ETRE)) {
        self$validate_expand_ETRE(expand_ETRE)
      }
      self$expand_ETRE <- expand_ETRE
    },
    
    set_write_sub_dir = function(write_sub_dir) {
      if (!is.null(write_sub_dir)) {
        self$validate_write_sub_dir(write_sub_dir)
      }
      self$write_sub_dir <- write_sub_dir
    },
    
    # Validation methods
    validate_verbose = function(verbose) {
      if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        cli::cli_abort("{.arg verbose} must be TRUE or FALSE.")
      }
    },
    
    validate_ndigits = function(ndigits) {
      if (!is.numeric(ndigits)) {
        cli::cli_abort("{.arg ndigits} must be numeric.")
      }
    },
    
    validate_check_shock_status = function(check_shock_status) {
      if (!is.logical(check_shock_status) || length(check_shock_status) != 1 || is.na(check_shock_status)) {
        cli::cli_abort("{.arg check_shock_status} must be TRUE or FALSE.")
      }
    },
    
    validate_timestep_header = function(timestep_header) {
      if (!is.character(timestep_header) || !toupper(timestep_header) %=% timestep_header) {
        cli::cli_abort("{.arg timestep_header} must be an upper case character vector.")
      }
    },
    
    validate_n_timestep_header = function(n_timestep_header) {
      if (!is.character(n_timestep_header) || !toupper(n_timestep_header) %=% n_timestep_header) {
        cli::cli_abort("{.arg n_timestep_header} must be an upper case character vector.")
      }
    },

    validate_full_exclude = function(full_exclude) {
      if (!is.character(full_exclude)) {
        cli::cli_abort("{.arg full_exclude} must be a character vector.")
      }
    },
    
    validate_docker_tag = function(docker_tag) {
      if (!is.character(docker_tag)) {
        cli::cli_abort("{.arg docker_tag} must be a character vector.")
      }
    },
    
    validate_margin_sectors = function(margin_sectors) {
      if (!is.character(margin_sectors)) {
        cli::cli_abort("{.arg margin_sectors} must be a character vector.")
      }
    },
    
    validate_accuracy_threshold = function(accuracy_threshold) {
      if (!is.numeric(accuracy_threshold) && accuracy_threshold <= 1) {
        cli::cli_abort("{.arg accuracy_threshold} must be a numeric less than or equal to 1.")
      }
    },
    
    validate_expand_ETRE = function(expand_ETRE) {
      if (!is.logical(expand_ETRE) || length(expand_ETRE) != 1 || is.na(expand_ETRE)) {
        cli::cli_abort("{.arg expand_ETRE} must be TRUE or FALSE.")
      }
    },
    
    validate_write_sub_dir = function(write_sub_dir) {
      if (!is.character(write_sub_dir) || length(write_sub_dir) != 1) {
        cli::cli_abort("{.arg write_sub_dir} must be a character vector length 1")
      }
    },
    
    # Validate all current options
    validate = function() {
      self$validate_verbose(self$get_verbose())
      self$validate_ndigits(self$get_ndigits())
      self$validate_check_shock_status(self$get_check_shock_status())
      self$validate_timestep_header(self$get_timestep_header())
      self$validate_n_timestep_header(self$get_n_timestep_header())
      self$validate_full_exclude(self$get_full_exclude())
      self$validate_docker_tag(self$get_docker_tag())
      self$validate_margin_sectors(self$get_margin_sectors())
      self$validate_accuracy_threshold(self$get_accuracy_threshold())
      self$validate_expand_ETRE(self$get_expand_ETRE())
      self$validate_write_sub_dir(self$get_write_sub_dir())
    }
  )
)

# fix cli and call forwarding

# Create constructor functions
options_new <- function(verbose = NULL,
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
  options_class$new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude,
    docker_tag = docker_tag,
    margin_sectors = margin_sectors,
    accuracy_threshold = accuracy_threshold,
    expand_ETRE = expand_ETRE,
    write_sub_dir = write_sub_dir
  )
}

options_init <- function(verbose = NULL,
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
  options_new(
    verbose = verbose,
    ndigits = ndigits,
    check_shock_status = check_shock_status,
    timestep_header = timestep_header,
    n_timestep_header = n_timestep_header,
    full_exclude = full_exclude,
    docker_tag = docker_tag,
    margin_sectors = margin_sectors,
    accuracy_threshold = accuracy_threshold,
    expand_ETRE = expand_ETRE,
    write_sub_dir = write_sub_dir
  )
}

# Create the global options object (like tar_options)
ems_options <- options_init()
