run_script <- function(name, write_dir) {
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
  
  title <- tools::file_path_sans_ext(basename(name))
  tempdir = file.path(write_dir, title)
  dir.create(tempdir)
  ems_option_set(tempdir = tempdir)
  source(path, local = parent.frame())
}

nest_temp <- function(name,
                      write_dir) {
  tempdir <- file.path(write_dir, name)
  dir.create(tempdir)
  ems_option_set(tempdir = tempdir)
}

write_modified_model <- function(model_file, text, .fn = paste) {
  model_text <- readChar(model_file, file.info(model_file)[["size"]])
  modified <- .fn(model_text, text)
  out_path <- file.path(dirname(model_file), "error.tab")
  writeLines(modified, out_path)
  out_path
}

write_modified_closure <- function(closure_file, text, .fn = cat) {
  closure_text <- readLines(closure_file)
  modified <- capture.output(.fn(closure_text[[1]], text, tail(closure_text, -1), sep = "\n"))
  out_path <- file.path(dirname(closure_file), "error.cls")
  writeLines(modified, out_path)
  out_path
}