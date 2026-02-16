#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.har_meta <- function(DREL,
                      DVER,
                      data_type,
                      call) {

  metadata <- list()
  if (length(DREL) %=% 1L) {
    string <- purrr::pluck(strsplit(DREL, "_"), 1, 1)
    if (string %=% "R9.0A") {
      metadata$database_version <- "GTAPv9A"
    } else {
      metadata$database_version <- paste0("GTAP", string)
    }
    metadata$reference_year <- as.numeric(sub(".*_(\\d{4}).*", "\\1", DREL))
  } else {
    metadata$database_version <- paste0("GTAP", DREL[1])
    metadata$reference_year <- as.numeric(sub("[A-Za-z]", "", DREL[2]))
  }


  if (data_type %=% "dat") {
    metadata$data_format <- switch(as.character(DVER),
      "5" = "GTAPv6",
      "6" = "GTAPv7",
      NULL
    )
  }
  return(metadata)
}