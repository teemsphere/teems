#' @importFrom tibble tibble
#' @importFrom purrr map_chr
#'
#' @keywords internal
#' @noRd
.retrieve_tab_comp <- function(tab_path,
                               type,
                               call) {

  tab <- .check_tab_file(
    tab_file = tab_path,
    call = call
  )

  extract <- tibble::tibble(
    type = purrr::map_chr(strsplit(tab, split = " ", perl = TRUE), 1),
    remainder = sub("^\\S+\\s*", "", tab)
  )

  extract$row_id <- seq(1, nrow(extract))
  
  parse_variable <- type %in% c("variable", "all")
  parse_coefficient <- type %in% c("coefficient", "all")
  comp_extract <- list()
  
  if (parse_variable) {
    comp_extract$variable <- .parse_tab_obj(
      extract = extract,
      obj_type = "variable",
      call = call
    )
  } 
  
  if (parse_coefficient) {
    comp_extract$coefficient <- .parse_tab_obj(
      extract = extract,
      obj_type = "coefficient",
      call = call
    )
  }
  
  file_state <- extract[extract$type == "File", "remainder"][[1]]
  input_files <- trimws(purrr::map_chr(strsplit(
    file_state[!grepl("\\(new\\)", file_state)],
    "#"
  ), 1))

  input_files <- file.path(dirname(tab_path), paste0(input_files, ".txt"))
  attr(comp_extract, "input_files") <- input_files
  return(comp_extract)
}