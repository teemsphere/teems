#' @keywords internal
#' @noRd
.write_input_files <- function(tab,
                               closure,
                               shocks,
                               cmf,
                               .data,
                               v_shock,
                               metadata,
                               sets) {
  write_dir <- .o_tempdir()
  tab_path <- .ems_write(
    input = tab,
    write_dir = write_dir
  )
  closure_path <- .ems_write(
    input = closure,
    write_dir = write_dir
  )
  shock_path <- .ems_write(
    input = shocks,
    write_dir = write_dir
  )
  cmf_path <- .ems_write(input = cmf)
  attr(cmf_path, "tab_path") <- tab_path
  input_files <- file.path(write_dir, paste0(unique(lapply(.data, attr, "file")), ".txt"))
  file.create(input_files)
  data_path <- lapply(.data,
    .ems_write,
    write_dir = write_dir
  )
  .out_mkdir(write_dir = write_dir)
  .diagnostic_output(
    tab_path = tab_path,
    cmf_path = cmf_path,
    closure_path = closure_path,
    shock_path = shock_path,
    data_path = unique(data_path),
    sets = sets,
    closure = closure,
    shocks = v_shock,
    metadata = metadata
  )
  return(cmf_path)
}