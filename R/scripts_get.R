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
  # the dimension rigs, the chronological-year variants and the scenario
  # script are dropped by file name, not by path: matched against the
  # full path, any directory component carrying one of these patterns (a
  # library under D:/3dwork, a home directory holding _year, an R tree
  # inside a scenarios folder) silently dropped scripts, and could drop
  # every one of them. One mask over one name vector, so the patterns
  # cannot fall out of step with the vector they subset.
  nm <- basename(scripts)
  keep <- !grepl(paste0(2:5, "d", collapse = "|"), nm) &
    # prep exported ems_meta function to get year quickly
    !grepl("_year", nm) &
    !grepl("scenario", nm)
  scripts <- scripts[keep]
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