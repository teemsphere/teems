#' @keywords internal
#' @noRd
.check_set_map <- function(set_map,
                           data_format,
                           database_version,
                           call,
                           ...) {
  UseMethod(".check_set_map")
}

#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
#' @export
#' @method .check_set_map internal
.check_set_map.internal <- function(set_map,
                                    data_format,
                                    database_version,
                                    call,
                                    available_mappings,
                                    map_layers = NULL,
                                    set_data = NULL,
                                    ...) {

  map_name <- attr(set_map, "name")
  # a model layer redefines the elements of some sets and leaves the
  # rest to the core mappings, so take the first bucket carrying this
  # set rather than merging them: the two element lists differ and
  # could not share one table
  available_mappings <- NULL
  for (bucket in c(map_layers, data_format)) {
    candidate <- purrr::pluck(mappings, database_version, bucket)
    if (map_name %in% names(candidate)) {
      available_mappings <- candidate
      break
    }
  }

  if (is.null(available_mappings)) {
    .cli_action(
      data_err$no_internal_mapping,
      action = "abort",
      call = call
    )
  }

  available_map_names <- names(available_mappings[[map_name]])[-1]
  if (!set_map %in% available_map_names) {
    .cli_action(
      data_err$invalid_internal_mapping,
      action = c("abort", "inform"),
      call = call
    )
  }

  available_mappings <- available_mappings[[map_name]]
  set_mapping <- available_mappings[, c(1, which(names(available_mappings) == set_map)), with = FALSE]
  # an element the table does not know would otherwise map to NA and
  # its data would drop out silently (GDYN 11c spells NatRes in the
  # ENDW header where every mapping says natlres)
  matched_data <- .match_set_data(set_data, map_name)
  if (length(matched_data) > 0L) {
    .check_map_coverage(
      set_mapping = set_mapping,
      data_ele = matched_data[[1]],
      map_name = map_name,
      call = call
    )
  }
  class(set_mapping) <- c("internal", class(set_mapping))
  return(set_mapping)
}

#' The set headers a mapping applies to: those classed by the set's
#' name, or carrying the header the set conversion table lists for it
#' (a v6-format GTAPv11 database names its set headers H1 to H9)
#'
#' @keywords internal
#' @noRd
.match_set_data <- function(set_data,
                            map_name) {
  hit <- purrr::map_lgl(set_data, function(s) {
    if (inherits(s, map_name)) {
      return(TRUE)
    }
    fmt <- if (inherits(s, "GTAPv7")) "GTAPv7" else "GTAPv6"
    id <- match(class(s)[1], set_conversion[[paste0(fmt, "header")]])
    !is.na(id) && identical(set_conversion[[paste0(fmt, "name")]][id], map_name)
  })
  set_data[hit]
}

#' Every element the data carries for a set must have a row in its
#' mapping; both sides compare in lowercase
#'
#' @keywords internal
#' @noRd
.check_map_coverage <- function(set_mapping,
                                data_ele,
                                map_name,
                                call) {
  supplied_ele <- tolower(set_mapping[[1]])
  data_ele <- unique(tolower(as.character(data_ele)))
  if (!all(data_ele %in% supplied_ele)) {
    missing_ele <- setdiff(data_ele, supplied_ele)
    .cli_action(data_err$missing_ele_mapping,
      action = "abort",
      call = call
    )
  }
  invisible(NULL)
}

#' @importFrom data.table fread is.data.table as.data.table copy fsetequal
#' @importFrom purrr pluck map_lgl
#' 
#' @keywords internal
#' @noRd
#' @export
#' @method .check_set_map user
.check_set_map.user <- function(set_map,
                                data_format,
                                database_version,
                                call,
                                set_data,
                                ...) {

  map_name <- attr(set_map, "name")
  
  if (inherits(set_map, "character")) {
    set_map <- .check_input(
      file = set_map,
      valid_ext = "csv",
      call = call
    )
    set_mapping <- data.table::fread(set_map)
  } else if (!data.table::is.data.table(set_map)) {
    set_mapping <- data.table::as.data.table(set_map)
  } else {
    set_mapping <- data.table::copy(set_map)
  }
  
  if (ncol(set_mapping) < 2) {
    .cli_action(data_err$invalid_user_input,
                action = "abort",
                call = call)
  }
  
  if (ncol(set_mapping) > 2) {
    .cli_action(data_wrn$invalid_user_input,
                action = "warn",
                call = call)
    set_mapping <- set_mapping[, c(1,2)]
  }
  
  matched_data <- .match_set_data(set_data, map_name)
  
  if (length(matched_data) %=% 0L) {
    .cli_action(data_err$missing_data,
                action = "abort",
                call = call)
  }

  # element names are case-insensitive on input and lowercase inside
  # TEEMS, so a mapping is folded before it is compared with the data
  set_mapping[, names(set_mapping) := lapply(.SD, .fold_elements)]
  .check_map_coverage(
    set_mapping = set_mapping,
    data_ele = matched_data[[1]],
    map_name = map_name,
    call = call
  )

  if (colnames(set_mapping[,c(1,2)]) %!=% c(map_name, "mapping")) {
    colnames(set_mapping)[c(1,2)] <- c(map_name, "mapping")
  }
  
  class(set_mapping) <- c("user", class(set_mapping))
  return(set_mapping)
}
