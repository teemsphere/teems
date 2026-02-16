run_script <- function(name) {
  # Try test_path first (works during interactive testing)
  path <- test_path("inst", "scripts", name)
  
  # If that doesn't exist, try from package root
  if (!file.exists(path)) {
    path <- test_path("..", "..", "inst", "scripts", name)
  }
  
  # If still not found, try system.file (works during R CMD check)
  if (!file.exists(path)) {
    path <- system.file("scripts", name, package = "teems")
  }
  
  if (!file.exists(path)) {
    stop("Script not found: ", name)
  }
  
  source(path, local = parent.frame())
}