#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.inject_script <- function(template,
                           script,
                           dat_input,
                           par_input,
                           set_input,
                           path,
                           model_file,
                           closure_file,
                           call = call) {
  script <- basename(script)
  sub_path <- file.path(path, script)
  sub_path <- normalizePath(sub_path, "/", FALSE)

  assignments <- list(
    dat_input    = dat_input,
    par_input    = par_input,
    set_input    = set_input,
    model_file   = model_file,
    closure_file = closure_file
  )

  assignments <- paste(names(assignments), purrr::map_chr(assignments, \(x) {
    if (is.call(x)) deparse(x) else paste0('"', x, '"')
  }), sep = " = ")

  assignments <- sub("\"NULL\"", "NULL", assignments)

  if (file.exists(sub_path)) {
    existing <- list.files(sub_path, all.files = TRUE, no.. = TRUE)
    if (length(existing) > 0) {
      .cli_action(gen_info$unlink,
        action = "inform",
        call = call
      )
      unlink(file.path(sub_path, "*"), expand = TRUE)
    }
    unlink(sub_path)
  }

  con <- file(sub_path, open = "a")
  writeLines("library(teems)", con)
  writeLines(assignments, con)
  writeLines("", con)
  writeLines(template, con)
  close(con)

  return(sub_path)
}