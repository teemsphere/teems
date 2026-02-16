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
                                    ...) {

  available_mappings <- purrr::pluck(mappings, database_version, data_format)
  map_name <- attr(set_map, "name")

  if (!map_name %in% names(available_mappings)) {
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
  class(set_mapping) <- c("internal", class(set_mapping))
  return(set_mapping)
}

#' @importFrom data.table fread
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
  set_map <- .check_input(
    file = set_map,
    valid_ext = "csv",
    call = call
  )
  set_mapping <- data.table::fread(set_map)
  
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
  
  matched_data <- set_data[purrr::map_lgl(set_data, inherits, map_name)]
  
  if (length(matched_data) %=% 0L) {
    .cli_action(data_err$missing_data,
                action = "abort",
                call = call)
  } else {
    supplied_ele <- purrr::pluck(set_mapping, 1)
    if (!all(matched_data[[map_name]] %in% supplied_ele)) {
      missing_ele <- setdiff(matched_data[[map_name]], supplied_ele)
      .cli_action(data_err$missing_ele_mapping,
                  action = "abort",
                  call = call
      )
    }
  }

  if (colnames(set_mapping[,c(1,2)]) %!=% c(map_name, "mapping")) {
    colnames(set_mapping)[c(1,2)] <- c(map_name, "mapping")
  }
  
  class(set_mapping) <- c("user", class(set_mapping))
  return(set_mapping)
}
