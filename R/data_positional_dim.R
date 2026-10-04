#' @importFrom data.table setattr setDT
#' @keywords internal
#' @noRd
.dimension_positional <- function(dt,
                                  model,
                                  set_ele,
                                  call) {
  header <- class(dt)[1]
  positional_dim <- attr(dt, "positional_dim")
  n_values <- nrow(dt)
  id <- which(model$type %in% c("Coefficient", "Variable") &
    toupper(model$header) %in% toupper(header))[1]
  coeff <- model$name[id]
  decl_sets <- model$ls_upper_idx[[id]]
  if (length(decl_sets) == 1L && is.na(decl_sets)) {
    decl_sets <- character(0)
  }
  decl_sizes <- lengths(set_ele[decl_sets])
  decl_dims <- if (length(decl_sets) > 0L) {
    paste0(decl_sets, " (", decl_sizes, ")", collapse = " x ")
  } else {
    "a scalar"
  }
  file_dims <- positional_dim[positional_dim != 1L]
  file_shape <- paste(positional_dim, collapse = " x ")
  if (length(decl_sets) == 0L ||
    prod(decl_sizes) != n_values ||
    !identical(as.integer(file_dims), as.integer(decl_sizes[decl_sizes != 1L]))) {
    .cli_action(deploy_err$unlabelled_header,
      action = c("abort", "inform"),
      call = call
    )
  }
  cols <- expand.grid(set_ele[decl_sets], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  col_names <- decl_sets
  col_names[duplicated(col_names)] <- paste0(col_names[duplicated(col_names)], ".1")
  names(cols) <- col_names
  out <- data.table::setDT(c(as.list(cols), list(Value = dt$Value)))
  class(out) <- class(dt)
  data.table::setattr(out, "positional_dim", NULL)
  return(out)
}
