# manually pull out set names for pre v11
# no telling how robust this is
#' @keywords internal
#' @noRd
.har_set_names <- function(headers,
                           metadata) {
    ranges <- c(
      H1 = 19, H2 = 25, H3 = 25, H4 = 25, H5 = 25,
      H6 = 25, H7 = 25, H8 = 25, H9 = 25, MARG = 25, TARS = 20
    )
    
    switch(metadata$database_version,
      "GTAPv9" = , # falls through to GTAPv10
      "GTAPv10" = {
        for (key in names(ranges)) {
          headers[[key]]$name <- trimws(rawToChar(headers[[key]]$records[[2]][14:ranges[key]]))
        }
      },
      "GTAPv11" = {
        headers <- lapply(headers, \(h) {
          h$name <- h$header
          return(h)
        })
      },
      "GTAPv12" = {
        headers <- lapply(headers, \(h) {
          h$name <- h$header
          return(h)
        })
      }
    )
  return(headers)
}
