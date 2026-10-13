#' @importFrom data.table data.table setnames
#' @keywords internal
#' @noRd
.get_timesteps <- function(paths,
                           cmf_path,
                           timestep_header,
                           call) {
  metadata <- readRDS(paths$metadata)
  t0 <- metadata$reference_year

  if (!is.null(metadata$time_steps)) {
    timesteps <- data.table::data.table(metadata$time_steps)
    data.table::setnames(timesteps, timestep_header)
    timesteps[, let(CYRS = t0 + unlist(timesteps))]
    timesteps[, let(all_time = seq(0, nrow(timesteps) - 1))]
    return(timesteps)
  }

  model <- .process_tablo(
    tab_file = paths$tab,
    quiet = TRUE,
    call = call
  )

  timestep_coeff <- model$name[match(.o_timestep_header(), model$header)]

  sol_prefix <- file.path(dirname(cmf_path), "out", "variables", "bin", "sol.")
  if (!.has_coefficient_dump(sol_prefix)) {
    .cli_action(compose_err$no_coef_dump,
      action = c("abort", "inform"),
      call = call
    )
  }
  cof <- .parse_coefficient_bins(
    sol_prefix = sol_prefix,
    coeff_names = tolower(timestep_coeff)
  )
  timesteps <- data.table::data.table(cof$xc$Value)
  data.table::setnames(timesteps, timestep_header)
  
  timesteps <- timesteps[!is.na(get(timestep_header))]
  timesteps[, let(CYRS = t0 + unlist(timesteps))]
  timesteps[, let(all_time = seq(0, nrow(timesteps) - 1))]
  
  return(timesteps)
}
