#' @importFrom purrr map map2
#' @importFrom data.table data.table rbindlist
#' @importFrom tools file_ext
#' @importFrom cli cli_h1 cli_text
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
      if (tools::file_ext(m) %=% "") {
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
      "GTAPv9" = "GTAPv6",
      "GTAPv10" = "GTAPv6",
      "GTAPv11" = "GTAPv7"
    )
  } else {
    data_format <- metadata$data_format
  }

  set_mappings <- lapply(set_mappings,
    .check_set_map,
    data_format = data_format,
    database_version = metadata$database_version,
    call = call,
    set_data = set_data
  )

  if (data_format %=% "GTAPv6") {
    set_mappings$MARG_COMM <- set_mappings$TRAD_COMM[set_mappings$TRAD_COMM[, 1][[1]] %in% .o_margin_sectors(), ]
    CGDS <- data.table::data.table("zcgds", "zcgds")
    set_mappings$PROD_COMM <- data.table::rbindlist(list(set_mappings$TRAD_COMM, CGDS), use.names = FALSE)
  } else if (data_format %=% "GTAPv7") {
    set_mappings$MARG <- set_mappings$COMM[set_mappings$COMM[, 1][[1]] %in% .o_margin_sectors(), ]
  }
  
  if (!is.null(time_steps)) {
    time_steps <- .check_time_steps(
      t0 = metadata$reference_year,
      time_steps = time_steps,
      call = call
    )
    attr(set_mappings, "time_steps") <- time_steps
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
  
  return(set_mappings)
}