#' @keywords internal
#' @noRd
.finalize_tab <- function(model) {

  set_extract <- model[model$type == "Set",]
  coeff_extract <- model[model$type == "Coefficient",]

  set_extract$name <- toupper(set_extract$name)
  set_writeout <- paste(
    "File",
    "(new)",
    set_extract$name,
    "#",
    set_extract$name,
    "output file #;\nWrite",
    "(set)",
    set_extract$name,
    "to file",
    set_extract$name,
    "header",
    paste0('"', set_extract$name, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", set_extract$label)), '"', ";")
  )

  coeff_writeout <- paste(
    "File",
    "(new)",
    coeff_extract$name,
    "#",
    coeff_extract$name,
    "output file #;\nWrite",
    coeff_extract$name,
    "to file",
    coeff_extract$name,
    "header",
    paste0('"', coeff_extract$name, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", coeff_extract$label)), '"', ";")
  )

  tab <- paste(
    c(
      model$tab,
      set_writeout,
      coeff_writeout
    ),
    collapse = "\n"
  )
  
  attr(tab, "file") <- attr(model, "tab_file")
  class(tab) <- c("tab", class(tab))
  return(tab)
}