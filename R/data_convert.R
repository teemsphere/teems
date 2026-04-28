#' @keywords internal
#' @noRd
.convert_data <- function(i_data) {
  UseMethod(".convert_data")
}

#' @method .convert_data GTAPv6
#' @importFrom purrr map compact list_flatten map_lgl map_chr
#' @keywords internal
#' @noRd
#' @export
.convert_data.GTAPv6 <- function(i_data) {
  
  metadata <- attr(i_data, "metadata")
  missing_par <- coeff_conversion[
    is.na(coeff_conversion$GTAPv6header) &
      coeff_conversion$data_type == "par",
    "GTAPv7header"
  ]

  missing_par <- c(missing_par, "ESBI")
  missing_par <- purrr::map(missing_par, function(p) {
    class(p) <- c(p, class(p))
    return(p)
  })

  new_v7 <- lapply(c(i_data, missing_par),
    .GTAPv7_header,
    REG = i_data$H1,
    COMM = i_data$H2,
    ACTS = i_data$H2,
    MARG = i_data$MARG,
    OSEP = i_data$OSEP
  )

  new_v7 <- purrr::list_flatten(purrr::compact(new_v7))
  names(new_v7) <- purrr::map_chr(new_v7, function(p) {
    class(p)[1]
  })
  i_data <- lapply(i_data,
    .header2GTAPv7,
    i_data = i_data
  )

  i_data <- c(i_data, new_v7)

  drop_set <- stats::na.omit(set_conversion[is.na(set_conversion$GTAPv7name), "GTAPv6header"])
  i_data <- i_data[!names(i_data) %in% drop_set]

  names(i_data) <- purrr::map_chr(i_data, function(h) {
    header <- class(h)[1]
  })
  
  i_data$ACTS <- .GTAPv7_set(
    class = class(i_data$ACTS),
    ele = setdiff(tolower(i_data$ACTS), "cgds")
  )

  i_data$ENDF <- .GTAPv7_set(
    header = "ENDF",
    name = "ENDWF",
    ele = setdiff(tolower(i_data$ENDS), "land")
  )

  i_data$ENDL <- .GTAPv7_set(
    header = "ENDL",
    name = "ENDWL",
    ele = setdiff(tolower(i_data$ENDM), "capital")
  )

  i_data$ENDC <- .GTAPv7_set(
    header = "ENDL",
    name = "ENDWL",
    ele = setdiff(tolower(i_data$ENDM), tolower(i_data$ENDL))
  )

  i_data$ENDS <- .GTAPv7_set(
    class = class(i_data$ENDS),
    ele = setdiff(tolower(i_data$ENDS), tolower(i_data$ENDF))
  )

  i_data <- i_data[!is.na(names(i_data))]
  metadata$data_format <- "GTAPv7"
  attr(i_data, "metadata") <- metadata
  class(i_data) <- c("GTAPv7", class(i_data))
  return(i_data)
}

#' @method .convert_data GTAPv7
#' @importFrom purrr map_lgl map_chr
#' @keywords internal
#' @noRd
#' @export
.convert_data.GTAPv7 <- function(i_data) {

  metadata <- attr(i_data, "metadata")
  drop_par <- c(coeff_conversion[is.na(coeff_conversion$GTAPv6header) & coeff_conversion$data_type == "par", "GTAPv7header"], "ESBI")
  i_data <- i_data[!names(i_data) %in% drop_par]
  i_data <- lapply(i_data, .header2GTAPv6, i_data = i_data)
  i_data$OSEP <- apply(i_data$MAKS - i_data$MAKB, 3, diag)
  class(i_data$OSEP) <- c("OSEP", "dat", "GTAPv6", "array")
  i_data$TVOM <- apply(i_data$MAKB, 3, diag)
  class(i_data$TVOM) <- c("TVOM", "dat", "GTAPv6", "array")

  drop_dat <- coeff_conversion[is.na(coeff_conversion$GTAPv6header) & coeff_conversion$data_type == "dat", "GTAPv7header"]
  i_data <- i_data[!names(i_data) %in% drop_dat]

  CGDS_COMM <- structure("CGDS",
    class = c("H9", "CGDS_COMM", "set", "GTAPv6", "character")
  )
  
  i_data$H9 <- CGDS_COMM
  H5_class <- class(i_data$ACTS)
  i_data$ACTS <- append(i_data$ACTS, CGDS_COMM)
  class(i_data$ACTS) <- H5_class
  
  H7_class <- class(i_data$ENDS)
  i_data$ENDS <- append(i_data$ENDS, i_data$ENDF)
  class(i_data$ENDS) <- H7_class
  
  keep_set <- c(set_conversion[!is.na(set_conversion$GTAPv6header), "GTAPv6header"], "TARS")

  i_data <- i_data[purrr::map_lgl(i_data, function(h) {
    if (inherits(h, "set")) {
      inherits(h, keep_set)
    } else {
      TRUE
    }
  })]
  
  names(i_data) <- purrr::map_chr(i_data, function(h) {
    header <- class(h)[1]
  })

  metadata$data_format <- "GTAPv6"
  attr(i_data, "metadata") <- metadata
  class(i_data) <- c("GTAPv6", class(i_data))
  return(i_data)
}