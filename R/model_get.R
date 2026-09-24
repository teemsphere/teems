#' @keywords internal
#' @noRd
.get_model <- function(model,
                       path) {
  model_file <- paste0(model, ".tab")
  closure_file <- paste0(model, ".cls")
  model_dir <- system.file(file.path("models", model),
                           package = "teems",
                           mustWork = TRUE)
  origin <- list.files(model_dir, full.names = TRUE)
  file.copy(
    from = origin,
    to = path,
    overwrite = TRUE
  )
  paths <- file.path(path, c(model_file, closure_file))
  paths <- normalizePath(paths, "/")
  names(paths) <- c("model_file", "closure_file")
  return(paths)
}