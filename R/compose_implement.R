#' @importFrom cli cli_warn
#' @importFrom purrr map_lgl
#' 
#' @keywords internal
#' @noRd
.implement_compose <- function(args_list,
                               call) {
  checklist <- list(
    cmf_path = "character",
    type = "character",
    name = c("NULL", "character"),
    minimal = "logical"
  )
  
  args_list[["..."]] <- NULL
  v <- .validate_compose_args(
    a = args_list,
    checklist = checklist,
    call = call
  )
  parsed <- .parse_solution(sol_prefix = v$sol_prefix)
  paths <- .get_output_paths(
    cmf_path = v$cmf_path,
    type = v$type,
    select = v$name,
    call = call
  )
  sets <- .check_sets(
    sets = parsed$set_union,
    set_ele = parsed$setele,
    model_dir = paths$model,
    set_path = paths$sets,
    minimal = v$minimal,
    call = call
  )
  timesteps <- NULL
  if (!v$minimal) {
    comp_extract <- .retrieve_tab_comp(
      tab_path = paths[["tab"]],
      type = v$type,
      call = call
    )
    
    metadata <- !is.null(paths$metadata)
    if (!metadata) {
      cli::cli_warn("No metadata file detected.")
    }
    
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
  } else {
    comp_extract <- NULL
  }
  output <- .retrieve_output(
    var_tbl = parsed$var_union,
    var_data = parsed$xc,
    type = v$type,
    comp_extract = comp_extract,
    name = v$name,
    paths = paths,
    sets = sets,
    time_steps = timesteps,
    minimal = v$minimal,
    call = call
  )
  
  return(output)
}