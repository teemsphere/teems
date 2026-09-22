#' @keywords internal
#' @noRd
.bind_tab_obj_read <- function(obj,
                               extract,
                               obj_type,
                               call) {
  if (obj_type %=% "coefficient") {
    r <- extract[tolower(extract$type) == "read", ]
    r$remainder <- .strip_read_qualifier(r$remainder)

    if (!all(grepl("from file", tolower(r$remainder)))) {
      .cli_action(model_err$missing_file,
                  action = "abort",
                  call = call)
    }

    r <- .parse_read_targets(r)

    r_idx <- match(obj$name, r$name)
    obj$header <- r$header[r_idx]
    obj$file <- r$file[r_idx]
  } else if (obj_type %=% "variable") {
    obj$header <- NA_character_
    obj$file <- NA_character_
    r <- extract[tolower(extract$type) == "read", ]
    r$remainder <- .strip_read_qualifier(r$remainder)
    r <- r[grepl("from file", tolower(r$remainder)), ]
    if (nrow(r) > 0L) {
      r <- .parse_read_targets(r)
      r_idx <- match(obj$name, r$name)
      obj$header <- r$header[r_idx]
      obj$file <- r$file[r_idx]
    }
  }
  return(obj)
}
