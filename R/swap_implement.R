#' @keywords internal
#' @noRd
.implement_swap <- function(args_list,
                            call) {
  v <- .validate_swap_args(
    a = args_list,
    call = call
  )
  
  if (!is.null(v$subset)) {
    breadth <- "partial"
    if (any(lengths(v$subset) > 1)) {
      depth <- "multi"
    } else {
      depth <- "single"
    }
    swap <- list(
      var = v$var,
      subset = v$subset
    )
  } else {
    breadth <- "full"
    swap <- list(var = v$var)
    depth <- "single"
  }
  attr(swap, "call") <- call
  class(swap) <- c("ems", depth, breadth, class(swap))
  swap <- list(swap)
  return(swap)
}