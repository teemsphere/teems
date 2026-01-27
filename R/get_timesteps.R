#' @importFrom data.table fread
#' 
#' @keywords internal
#' @noRd
.get_timesteps <- function(paths,
                           cmf_path,
                           timestep_header,
                           call) {
  t0 <- readRDS(paths$metadata)$reference_year

  # there is an inconsequential error on math parsing (avoided with quiet = T) on this pass with inaccurate implicit statement at end
  model <- .process_tablo(
    tab_file = paths$tab,
    quiet = TRUE,
    call = call
  )
  timestep_coeff <- model$name[match(.o_timestep_header(), model$header)]
  timestep_file <- .get_output_paths(
    cmf_path = cmf_path,
    type = "coefficient",
    select = timestep_coeff
  )$coeff
  timesteps <- data.table::fread(timestep_file,
                                 skip = 1,
                                 col.names = timestep_header)
  timesteps <- timesteps[!is.na(get(timestep_header))]
  timesteps[, let(CYRS = t0 + unlist(timesteps))]
  timesteps[, let(all_time = seq(0, nrow(timesteps) - 1))]
  return(timesteps)
}