#' @keywords internal
#' @noRd
.solver_resources_record <- function(a,
                                     paths,
                                     call) {
  th <- .auto_thresholds()
  host <- .cntnr_resources(
    image = paste0("teems:", .resolve_docker_tag(quiet = TRUE))
  )
  metadata <- .deploy_metadata(cmf_path = paths$cmf)
  condensed <- isTRUE((metadata$condense$n_backsolve %|||% 0L) > 0L)
  plain_size <- if (is.null(metadata$system_size)) {
    NA_real_
  } else {
    metadata$system_size + (metadata$condense$n_backsolve_ele %|||% 0)
  }
  resources_record <- list(
    method = a$matrix_method,
    n_tasks = as.integer(a$n_tasks),
    n_threads = as.integer(a$n_threads),
    inmemory = a$inmemory,
    cores = host$cores,
    mem_gb = host$mem_gb
  )
  resources_record$fit <- .memory_fit_check(
    method = a$matrix_method,
    n_tasks = a$n_tasks,
    plain_size = plain_size,
    condensed = condensed,
    host = host,
    th = th,
    call = call
  )
  if (is.null(a$tempdir) && (a$matrix_method %=% "NDBBD" || isFALSE(a$inmemory))) {
    a$tempdir <- "/tmp"
  }
  resources_record$tempdir <- a$tempdir
  a$resources_record <- resources_record
  if (!is.null(metadata)) {
    .advise_cndns(
      metadata = metadata,
      matrix_method = a$matrix_method,
      enable_time = a$enable_time,
      call = call
    )
  }
  return(a)
}
