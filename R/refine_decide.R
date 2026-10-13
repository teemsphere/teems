#' @keywords internal
#' @noRd
.refine_decide <- function(method,
                           n_tasks,
                           plain_size,
                           condensed = FALSE,
                           mode = .o_refine(),
                           th = .auto_thresholds()) {
  if (!method %=% "DBBD") {
    return(NULL)
  }
  d <- list(
    on = isTRUE(mode),
    reason = if (isTRUE(mode)) "on" else "off",
    est_gb = .auto_memory_gb("DBBD", n_tasks, plain_size, condensed, th, refine = TRUE)
  )
  return(d)
}
