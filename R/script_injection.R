#' @keywords internal
#' @noRd
.inject_script <- function(template,
                           script,
                           dat_input,
                           par_input,
                           set_input,
                           target_format,
                           write_dir,
                           model_file,
                           closure_file) {
  script <- basename(script)
  write_path <- file.path(write_dir, script)
  write_path <- normalizePath(write_path, winslash = "/")
  assignments <- list(
    dat_input    = dat_input,
    par_input    = par_input,
    set_input    = set_input,
    target_format = target_format,
    write_dir    = write_dir,
    model_file   = model_file,
    closure_file = closure_file
  )
  
  assignments <- paste(names(assignments), paste0("\"", assignments, "\""), sep = " = ")
  assignments <- sub("\"NULL\"", "NULL", assignments)

  if (file.exists(write_path)) {
    unlink(write_path)
  }
  
  con <- file(write_path, open = "a")
  writeLines(assignments, con)
  writeLines("", con)
  writeLines(template, con)
  close(con)
  
  return(write_path)
}
