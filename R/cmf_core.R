#' @importFrom purrr map
#'
#' @keywords internal
#' @noRd
.cmf_core <- function(input_files,
                      model_file,
                      closure_file,
                      shock_file,
                      write_dir) {

  input_files <- purrr::map(input_files, normalizePath)
  files <- c(input_files,
    model_file = model_file,
    closure_file = closure_file,
    shock_file = shock_file
  )

  .out_mkdir(write_dir = write_dir,
             sets = FALSE,
             coeff = FALSE)
  
  files <- purrr::map(
    files,
    function(f) {
      nme <- basename(f)
      new_path <- file.path(write_dir, nme)
      file.copy(f, new_path)
      return(new_path)
    }
  )

  input_files <- files[names(files) %in% names(input_files)]
  input_files <- paste(
    "iodata",
    paste0('"', names(input_files), '"'),
    paste0('"', input_files, '";')
  )

  cmf_comp <- c(
    paste("tabfile", paste0("\"", files$model_file, "\"", ";")),
    paste("closure", paste0("\"", files$closure_file, "\"", ";")),
    paste("shock", paste0("\"", files$shock_file, "\"", ";"))
  )

  var_output <- paste("soldata", '"SolFiles"', paste0(
    '"',
    file.path(write_dir, "out", "variables", "bin", "sol"),
    '";'
  ))

  cmf <- c(
    input_files,
    cmf_comp,
    var_output
  )
browser()
  cmf <- purrr::map_chr(cmf, function(c) {
    gsub(write_dir, "/opt/teems", c, fixed = TRUE)
  })

  return(cmf)
}