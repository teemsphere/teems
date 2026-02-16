#' @keywords internal
#' @noRd
.coeff_mod <- function(mod_coeff,
                       model,
                       call) {
  checks <- c("character", "numeric", "data.frame")
  for (coeff in seq_along(mod_coeff)) {
    nme <- names(mod_coeff[coeff])

    if (!nme %in% model[model$type == "Coefficient", ]$name) {
      .cli_action(
        model_err$invalid_coeff,
        action = "abort",
        call = call
      )
    }
    
    input <- mod_coeff[[coeff]]

    .check_class(
      arg = input,
      arg_name = nme,
      check = checks,
      call = call
    )

    if (inherits(input, "numeric") && length(input) %!=% 1L) {
      .cli_action(model_err$invalid_numeric,
        action = c("abort", "inform"),
        call = call
      )
    }

    if (nme %in% model[model$type == "Read", ]$name) {
      tab_entry <- "read"
    } else if (any(grepl(nme, model[model$type == "Formula", ]$comp1))) {
      tab_entry <- "formula"
    } else {
      .cli_action(
        model_err$invalid_mod,
        action = "abort",
        call = call
      )
    }

    concat_class <- class(input)[length(class(input))]
    class(input) <- c(paste0(concat_class, "_", tab_entry), class(input))
    attr(input, "name") <- nme
    model <- .inject_value(
      input = input,
      model = model,
      call = call
    )
  }
  return(model)
}