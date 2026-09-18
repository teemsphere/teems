#' @keywords internal
#' @noRd
.deploy_metadata <- function(cmf_path) {
  metadata_path <- file.path(dirname(cmf_path), "metadata.rds")
  if (!file.exists(metadata_path)) {
    return(NULL)
  }
  metadata <- readRDS(metadata_path)
  return(metadata)
}
