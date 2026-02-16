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
  missing_par <- coeff_conversion[is.na(coeff_conversion$GTAPv6header) &
                                  coeff_conversion$data_type == "par",
                                  "GTAPv7header"]
  missing_par <- purrr::map(missing_par, function(p) {
    class(p) <- c(p, class(p))
    return(p)
  })

  new_v7 <- lapply(c(i_data, missing_par),
                   .GTAPv7_header,
                   REG = i_data$H1,
                   COMM = i_data$H2,
                   ACTS = i_data$H2,
                   MARG = .o_margin_sectors(),
                   OSEP = i_data$OSEP)
  new_v7 <- purrr::list_flatten(purrr::compact(new_v7))
  names(new_v7) <- purrr::map_chr(new_v7, function(p) {class(p)[1]})
  i_data <- lapply(i_data,
                   .header2GTAPv7,
                   i_data = i_data)

  i_data <- c(i_data, new_v7)
  ACTS <- i_data$H2
  class(ACTS)[1:2] <- "ACTS"
  i_data[["ACTS"]] <- ACTS
  keep_set <- set_conversion[!is.na(set_conversion$GTAPv7header), "GTAPv7header"]
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
  drop_par <- coeff_conversion[is.na(coeff_conversion$GTAPv6header) & coeff_conversion$data_type == "par", "GTAPv7header"]

  i_data <- i_data[!names(i_data) %in% drop_par]
  i_data <- lapply(i_data, .header2GTAPv6, i_data = i_data)
  drop_dat <- coeff_conversion[is.na(coeff_conversion$GTAPv6header) & coeff_conversion$data_type == "dat", "GTAPv7header"]
  i_data <- i_data[!names(i_data) %in% drop_dat]

  CGDS_COMM <- structure("CGDS",
    class = c("H9", "CGDS_COMM", "set", "GTAPv6", "character")
  )

  i_data[["H9"]] <- CGDS_COMM
  keep_set <- set_conversion[!is.na(set_conversion$GTAPv6header), "GTAPv6header"]

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