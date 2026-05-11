#' @importFrom cli cli_warn
#' @importFrom purrr map_lgl
#' @importFrom tools file_path_sans_ext
#'
#' @keywords internal
#' @noRd
.implement_compose <- function(args_list,
                               call) {

  v <- .validate_compose_args(
    a = args_list,
    call = call
  )

  meta <- .parse_solution_meta(sol_prefix = v$sol_prefix)
  avail_vars <- meta$var_union$cofname
  paths <- .get_output_paths(cmf_path = v$cmf_path)
  avail_coeffs <- if (!is.null(paths$coeff)) {
    tools::file_path_sans_ext(basename(paths$coeff))
  } else {
    character(0)
  }

  var_names_sel <- character(0)
  coeff_names_sel <- character(0)

  select_all <- args_list$which %=% "all"

  if (!select_all) {
    invalid <- v$which[!v$which %in% c(avail_vars, avail_coeffs)]

    if (length(invalid) > 0) {
      name <- invalid
      .cli_action(compose_err$invalid_name,
        action = c("abort", "inform"),
        call = call
      )
    }
    var_names_sel <- v$which[v$which %in% avail_vars]
    coeff_names_sel <- v$which[v$which %in% avail_coeffs]
  }

  compose_variable <- select_all || length(var_names_sel) > 0
  compose_coefficient <- (select_all && length(avail_coeffs) > 0) ||
    length(coeff_names_sel) > 0

  if (!select_all && length(coeff_names_sel) > 0) {
    paths$coeff <- paths$coeff[
      tools::file_path_sans_ext(basename(paths$coeff)) %in% coeff_names_sel
    ]
  } else if (!compose_coefficient) {
    paths$coeff <- NULL
  }

  type <- if (compose_variable && compose_coefficient) {
    "all"
  } else if (compose_variable) {
    "variable"
  } else {
    "coefficient"
  }

  sets <- .check_sets(
    sets = meta$set_union,
    set_ele = meta$setele,
    model_dir = paths$model,
    set_path = paths$sets,
    call = call
  )

  comp_extract <- .retrieve_tab_comp(
    tab_path = paths[["tab"]],
    type = type,
    call = call
  )

  timesteps <- NULL
  metadata <- !is.null(paths$metadata)

  if (any(purrr::map_lgl(sets, function(s) {
    isTRUE(attr(s, "intertemporal"))
  })) && metadata) {
    timesteps <- .get_timesteps(
      paths = paths,
      cmf_path = v$cmf_path,
      timestep_header = .o_timestep_header(),
      call = call
    )
  }

  if (length(var_names_sel) > 0 && !is.null(comp_extract$variable)) {
    comp_extract$variable <- comp_extract$variable[
      tolower(comp_extract$variable$name) %in% var_names_sel, ,
      drop = FALSE
    ]
  }
  if (length(coeff_names_sel) > 0 && !is.null(comp_extract$coefficient)) {
    comp_extract$coefficient <- comp_extract$coefficient[
      comp_extract$coefficient$name %in% coeff_names_sel, ,
      drop = FALSE
    ]
  }

  if (compose_variable) {
    bins <- .parse_solution_bins(
      sol_prefix = v$sol_prefix,
      var_names  = var_names_sel
    )
  } else {
    bins <- list(var_union = NULL, xc = NULL)
  }

  output <- .retrieve_output(
    var_tbl      = bins$var_union,
    var_data     = bins$xc,
    type         = type,
    comp_extract = comp_extract,
    paths        = paths,
    sets         = sets,
    time_steps   = timesteps,
    call         = call
  )

  return(output)
}