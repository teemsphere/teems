#' @keywords internal
#' @noRd
.get_output_paths <- function(cmf_path,
                              type,
                              # select = NULL,
                              call) {
  model_dir <- dirname(normalizePath(cmf_path))
  if ("tab_path" %in% names(attributes(cmf_path))) {
    tab_path <- attr(cmf_path, "tab_path")
  } else {
    tab_path <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = cmf_path
    )
  }

  metadata_path <- file.path(model_dir, "metadata.rds")

  if (!file.exists(metadata_path) || !inherits(readRDS(metadata_path), "teems_metadata")) {
    metadata_path <- NULL
  }

  set_paths <- list.files(
    path = file.path(
      model_dir,
      "out",
      "sets"
    ),
    pattern = "csv",
    full.names = TRUE
  )

  if (type %in% c("all", "coefficient")) {
    coeff_paths <- list.files(
      path = file.path(
        model_dir,
        "out",
        "coefficients"
      ),
      pattern = "csv",
      full.names = TRUE
    )

    # reintegrate this
    # if (!is.null(select)) {
    #   coeff_paths <- coeff_paths[tools::file_path_sans_ext(basename(coeff_paths)) %in% select]
    # }
  } else {
    coeff_paths <- NULL
  }

  paths <- list(
    tab = tab_path,
    model = model_dir,
    metadata = metadata_path,
    coeff = coeff_paths,
    sets = set_paths
  )

  return(paths)
}