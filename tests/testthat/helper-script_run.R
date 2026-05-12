run_script <- function(name) {
  path <- test_path("inst", "scripts", name)
  
  if (!file.exists(path)) {
    path <- test_path("..", "..", "inst", "scripts", name)
  }
  
  if (!file.exists(path)) {
    path <- system.file("scripts", name, package = "teems")
  }
  
  if (!file.exists(path)) {
    stop("Script not found: ", name)
  }
  
  source(path, local = parent.frame())
}