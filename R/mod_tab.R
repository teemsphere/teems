#' @keywords internal
#' @noRd
.tab_mod <- function(input,
                     model,
                     call) {
  UseMethod(".tab_mod")
}

#' @importFrom purrr pluck
#' @keywords internal
#' @noRd
#' @method .tab_mod data.frame
#' @export
.tab_mod.data.frame <- function(input,
                                model,
                                call) {

  nme <- attr(input, "name")
  r_idx <- grep(nme, model$comp1)
  purrr::pluck(model, "type", r_idx) <- "Read"
  purrr::pluck(model, "name", r_idx) <- nme
  model[which(model$name == nme), ]$header <- nme
  purrr::pluck(model, "ls_upper_idx", r_idx) <- NULL
  purrr::pluck(model, "ls_mixed_idx", r_idx) <- NULL
  purrr::pluck(model, "header", r_idx) <- nme

  param <- grepl("parameter", model[(model$name == nme) & (model$type == "Coefficient"), ]$qualifier_list)

  if (param) {
    # let's just take the most used file for parameters instead of complicating this
    files <- na.omit(model[grepl("parameter", model$qualifier_list), ]$file)
    file <- names(which.max(table(files)))
  } else {
    files <- na.omit(model[(!grepl("parameter", model$qualifier_list)) & (model$type == "Coefficient"), ]$file)
    file <- names(which.max(table(files)))
  }

  model[which(model$name == nme), ]$file <- file
  purrr::pluck(model, "definition", r_idx) <- NULL
  purrr::pluck(model, "subsets", r_idx) <- NULL
  purrr::pluck(model, "comp1", r_idx) <- NA
  purrr::pluck(model, "comp2", r_idx) <- NA
  purrr::pluck(model, "tab", r_idx) <- paste(
    "Read",
    nme,
    "from file",
    file, "header",
    paste0("\"", nme, "\";")
  )
  
  return(model)
}


#' @importFrom data.table setcolorder setkeyv
#' @importFrom purrr pluck map2_chr
#' @keywords internal
#' @noRd
#' @method .tab_mod numeric_read
#' @export
.tab_mod.numeric_read <- function(input,
                                  model,
                                  call) {
  nme <- attr(input, "name")
  r_idx <- which(model$name == nme & model$type == "Read")
  purrr::pluck(model, "type", r_idx) <- "Formula"
  purrr::pluck(model, "name", r_idx) <- NA
  purrr::pluck(model, "header", r_idx) <- NA
  purrr::pluck(model, "file", r_idx) <- NA

  hybrid_sets <- model[which(model$name == nme), ]$ls_mixed_idx[[1]]
  sub_idx <- purrr::map_chr(hybrid_sets, function(s) {
    substr(s, nchar(s), nchar(s))
  })
  comp1 <- paste0(nme, "(", paste0(sub_idx, collapse = ","), ")")
  purrr::pluck(model, "comp1", r_idx) <- comp1
  purrr::pluck(model, "comp2", r_idx) <- as.character(input)
  stnd_sets <- model[which(model$name == nme), ]$ls_upper_idx[[1]]
  sets <- paste0(purrr::map2_chr(stnd_sets, sub_idx, function(upper, lower) {
    paste0(c("(all", lower, paste0(upper, ")")), collapse = ",")
  }), collapse = "")
  purrr::pluck(model, "tab", r_idx) <- paste("Formula", sets, comp1, "=", paste0(input, ";"))
  return(model)
}

#' @keywords internal
#' @noRd
#' @method .tab_mod numeric_formula
#' @export
.tab_mod.numeric_formula <- function(input,
                                     model,
                                     call) {
  nme <- attr(input, "name")
  r_idx <- grep(nme, model$comp1)
  model$tab[r_idx] <- gsub(model[r_idx, ]$comp2, input, model$tab[r_idx])
  model[r_idx, ]$comp2 <- as.character(input)
  return(model)
}
