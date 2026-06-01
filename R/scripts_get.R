#' @importFrom purrr map2_chr
#' 
#' @keywords internal
#' @noRd
.get_scripts <- function(path,
                         model,
                         model_paths,
                         dat_input,
                         par_input,
                         set_input,
                         call) {
  dir <- system.file(file.path("scripts", model),
    package = "teems",
    mustWork = TRUE
  )
  scripts <- list.files(dir, full.names = TRUE)
  scripts <- scripts[!grepl(paste0(2:5, "d", collapse = "|"), scripts)]
  # prep exported ems_meta function to get year quickly
  scripts <- scripts[!grepl("_year", scripts)]
  scripts <- scripts[!grepl("scenario", scripts)]
  templates <- lapply(scripts, readLines)

  if (is.list(dat_input)) {
    dat_input <- .prep_rds(
      path = path,
      input = dat_input,
      prefix = "dat"
    )
  }

  if (is.list(par_input)) {
    par_input <- .prep_rds(
      path = path,
      input = par_input,
      prefix = "par"
    )
  }

  if (is.list(set_input)) {
    set_input <- .prep_rds(
      path = path,
      input = set_input,
      prefix = "set"
    )
  }

  paths <- purrr::map2_chr(templates,
    scripts,
    .inject_script,
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    path = path,
    model_file = model_paths[["model_file"]],
    closure_file = model_paths[["closure_file"]],
    call = call
  )

  return(paths)
}