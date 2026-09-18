#' Element names of SET3 = SET1 x SET2 (GEMPACK manual 11.7.11): xx_yyy
#' with the first factor varying fastest. When the longest names would
#' exceed the 12-character element limit, the elements of a factor are
#' truncated to "<first letter of the factor set><element number><leading
#' characters that fit>": both factors to 6 and 5 characters when both
#' are long, otherwise the long one to 11 minus the short one's length.
#' Mirrors set_product_names() in the solver (tab_parse.c) exactly.
#'
#' @keywords internal
#' @noRd
.set_product_names <- function(a, nm1, b, nm2, owner, call) {
  mx1 <- max(nchar(a), 0L)
  mx2 <- max(nchar(b), 0L)
  lim1 <- 0L
  lim2 <- 0L
  if (mx1 + mx2 > 11L) {
    if (mx1 <= 5L) {
      lim2 <- 11L - mx1
    } else if (mx2 <= 5L) {
      lim1 <- 11L - mx2
    } else {
      lim1 <- 6L
      lim2 <- 5L
    }
  }
  letter <- function(nm) {
    if (nzchar(nm)) {
      ch <- tolower(substr(nm, 1L, 1L))
      return(ch)
    }
    ch <- tolower(substr(owner, 1L, 1L))
    return(ch)
  }
  trunc_names <- function(x, nm, lim) {
    if (lim == 0L) {
      return(x)
    }
    pre <- paste0(letter(nm), seq_along(x))
    room <- pmax(lim - nchar(pre), 0L)
    truncated <- paste0(pre, substr(x, 1L, room))
    return(truncated)
  }
  e1 <- trunc_names(a, nm1, lim1)
  e2 <- trunc_names(b, nm2, lim2)
  # element of the first factor varies fastest
  out <- as.vector(outer(e1, e2, \(x, y) paste0(x, "_", y)))
  if (anyDuplicated(out)) {
    bad_set <- owner
    dup_ele <- out[duplicated(out)][1]
    .cli_action(model_err$set_product_dup,
      action = "abort",
      call = call
    )
  }
  return(out)
}
