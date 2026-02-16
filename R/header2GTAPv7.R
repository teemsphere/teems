#' @keywords internal
#' @noRd
.header2GTAPv7 <- function(input,
                           ...) {
  UseMethod(".header2GTAPv7")
}

#' @method .header2GTAPv7 default
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.default <- function(input,
                                   ...) {
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 VDFM
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.VDFM <- function(input,
                                ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2GTAPv7 VDFA
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.VDFA <- function(input,
                                ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2GTAPv7 VIFM
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.VIFM <- function(input,
                                ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2GTAPv7 VIFA
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.VIFA <- function(input,
                                ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
}

#' @method .header2GTAPv7 ISEP
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.ISEP <- function(input,
                                ...) {
  classes <- class(input)
  input <- input[, length(dimnames(input)$PROD_COMM), , ]
  input <- input * -1
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 EVFA
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.EVFA <- function(input,
                                ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 VFM
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.VFM <- function(input,
                               ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 FBEP
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.FBEP <- function(input,
                                ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 FTRV
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.FTRV <- function(input,
                                ...) {
  input <- .drop_CGDS(input)
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 EVOA
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.EVOA <- function(input,
                                i_data,
                                ...) {
  classes <- class(input)
  VFM_arr <- i_data$VFM
  VFM_arr <- VFM_arr[, -length(dimnames(VFM_arr)[[2]]), ]
  VFM_arr_dimnames <- dimnames(VFM_arr)
  VFM_arr_total <- apply(VFM_arr, c(1, 3), sum)

  # calculate percentage along REG,ENDW and use share with EVOA
  # divide each [i, , k] slice by VFM_arr_total[i, k]
  phi_arr <- sweep(VFM_arr, c(1, 3), VFM_arr_total, FUN = "/")
  
  # multiply each [i, , k] slice by input[i, k]
  new_arr <- sweep(phi_arr, c(1, 3), input, FUN = "*")

  input <- new_arr
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 ESBD
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.ESBD <- function(input,
                                i_data,
                                ...) {
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 ESBM
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.ESBM <- function(input,
                                i_data,
                                ...) {
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 ETRE
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.ETRE <- function(input,
                                i_data,
                                ...) {
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 ESBT
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.ESBT <- function(input,
                                i_data,
                                ...) {
  input <- .drop_CGDS(input = input)
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}

#' @method .header2GTAPv7 ESBV
#' @keywords internal
#' @noRd
#' @export
.header2GTAPv7.ESBV <- function(input,
                                i_data,
                                ...) {
  input <- .drop_CGDS(input = input)
  classes <- class(input)
  input <- .add_REG(
    input = input,
    REG = i_data$H1
  )
  class(input) <- unique(c(classes, class(input)))
  input <- .names_rename(input = input)
  return(input)
}