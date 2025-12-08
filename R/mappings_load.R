#' @importFrom purrr map map2
#' @importFrom data.table data.table rbindlist
#' 
#' @noRd
#' @keywords internal
.load_mappings <- function(set_mappings,
                           set_data,
                           metadata,
                           time_steps,
                           call) {
  set_mappings <- purrr::map2(
    set_mappings,
    names(set_mappings),
    function(m, n) {
      if (!tolower(tools::file_ext(m)) %=% "csv") {
        class(m) <- c("internal", class(m))
      } else {
        class(m) <- c("user", class(m))
      }
      attr(m, "name") <- n
      return(m)
    }
  )

  if (is.null(metadata$data_format)) {
    data_format <- switch(metadata$database_version,
      "v9" = "v6.2",
      "v10" = "v6.2",
      "v11" = "v7.0"
    )
  } else {
    data_format <- metadata$data_format
  }

  set_mappings <- purrr::map(set_mappings,
    .check_set_map,
    data_format = data_format,
    database_version = metadata$database_version,
    call = call,
    set_data = set_data
  )

  if (data_format %=% "v6.2") {
    set_mappings$MARG_COMM <- set_mappings$TRAD_COMM[set_mappings$TRAD_COMM[, 1][[1]] %in% .o_margin_sectors(), ]
    CGDS <- data.table::data.table("zcgds", "zcgds")
    set_mappings$PROD_COMM <- data.table::rbindlist(list(set_mappings$TRAD_COMM, CGDS), use.names = FALSE)
  } else if (data_format %=% "v7.0") {
    set_mappings$MARG <- set_mappings$COMM[set_mappings$COMM[, 1][[1]] %in% .o_margin_sectors(), ]
  }
  
    if (.o_verbose()) {
      cli::cli_h1("Set elements")
      purrr::map2(
        names(set_mappings),
        set_mappings,
        function(set_name, ele) {
          ele <- sort(unlist(unique(ele[,2])))
          cli::cli_text("{set_name}: {.val {ele}}")
        }
      )
    }

  if (!is.null(time_steps)) {
    time_steps <- .check_time_steps(
      t0 = metadata$reference_year,
      time_steps = time_steps,
      call = call
    )
    attr(set_mappings, "time_steps") <- time_steps
  }
  
  return(set_mappings)
}