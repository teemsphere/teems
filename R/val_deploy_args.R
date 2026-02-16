#' @importFrom tools R_user_dir
#' 
#' @keywords internal
#' @noRd
.validate_deploy_args <- function(a,
                                  sets,
                                  model_headers,
                                  call,
                                  data_call) {

  checklist <- list(
    .data = "list",
    model = "data.frame",
    shock = c("NULL", "list"),
    swap_in = c("NULL", "character", "list"),
    swap_out = c("NULL", "character", "list"),
    write_dir = "character",
    shock_file = c("NULL", "character")
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  a$write_dir <- .check_write_dir(write_dir = a$write_dir,
                                  call = call)

  if (attr(sets, "intertemporal")) {
    int_sets <- a$model[which(a$model$qualifier_list == "(intertemporal)"), "name"][[1]]
  } else {
    int_sets <- NULL
  }

  if (!is.null(a$shock)) {
    a$shock <- .expand_ele(input = a$shock)
    a$shock <- lapply(
      a$shock,
      .check_shock,
      var_extract = a$model[a$model$type == "Variable",],
      int_sets = int_sets,
      call = call
    )
  }

  if (!is.null(a$swap_in)) {
    a$swap_in <- .expand_ele(input = a$swap_in, nested = TRUE)
    a$swap_in <- lapply(a$swap_in,
      .check_swap,
      var_extract = a$model[a$model$type == "Variable",],
      sets = sets,
      call = call
    )
  }

  if (!is.null(a$swap_out)) {
    a$swap_out <- .expand_ele(input = a$swap_out, nested = TRUE)
    a$swap_out <- lapply(a$swap_out,
      .check_swap,
      var_extract = a$model[a$model$type == "Variable",],
      sets = sets,
      call = call
    )
  }
  
  if (!is.null(a$shock_file)) {
    if (!is.null(a$shock)) {
      .cli_action(shk_err$shk_file_shocks,
        action = "abort",
        call = call
      )
    }

    a$shock_file <- .check_input(
      file = a$shock_file,
      valid_ext = "shf",
      call = call
    )
  }

  non_int_req <- setdiff(a$model[!is.na(a$model$header), ]$header,
                         c(.o_n_timestep_header(), .o_timestep_header()))
  
  if (!is.null(model_headers)) {
    non_int_req <- setdiff(non_int_req, model_headers)
  }
  
  if (any(!non_int_req %in% names(a$.data))) {
    missing_headers <- setdiff(non_int_req, names(a$.data))
    # add inform about how to load aux data
    .cli_action(deploy_err$missing_header,
      action = "abort",
      call = data_call
    )
  }
  
  return(a)
}