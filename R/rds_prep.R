#' @keywords internal
#' @noRd
.prep_rds <- function(path,
                      input,
                      prefix) {
  input_RDS <- file.path(path, paste0(prefix, ".RDS"))
  input_RDS <- normalizePath(input_RDS, "/", mustWork = FALSE)
  saveRDS(input, input_RDS)
  rds_call <- call("readRDS", input_RDS)
  return(rds_call)
}