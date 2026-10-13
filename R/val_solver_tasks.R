#' @keywords internal
#' @noRd
.validate_solver_tasks <- function(a,
                                   paths,
                                   call) {
  n_tasks <- a$n_tasks
  if (a$matrix_method %=% "LU" && n_tasks > 1) {
    .cli_action(solve_err$lu_tasks,
      action = c("abort", "inform"),
      call = call
    )
  }
  metadata <- .deploy_metadata(cmf_path = paths$cmf)
  if (is.null(metadata)) {
    return(invisible(NULL))
  }
  matrix_method <- a$matrix_method
  if (a$matrix_method %=% "DBBD" && isTRUE(n_tasks > (metadata$n_ele_max %|||% n_tasks))) {
    n_blocks <- metadata$n_ele_max
    block_kind <- "element of the largest set"
    .cli_action(solve_err$tasks_blocks,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}
