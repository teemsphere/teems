#' @keywords internal
#' @noRd
.finalize_tab <- function(model,
                          write_coefficients = FALSE) {

  if (is.null(model$postsim)) {
    model$postsim <- FALSE
  }

  backsolved <- model$type == "Variable" & model$condense %in% "backsolve"
  backsolve_writeout <- paste(
    "Backsolve",
    model$name[backsolved],
    "using",
    model$condense_eq[backsolved],
    ";"
  )
  if (!any(backsolved)) {
    backsolve_writeout <- NULL
  }

  is_ps <- !is.na(model$postsim) & model$postsim
  set_extract <- model[model$type == "Set",]
  coeff_extract <- model[model$type == "Coefficient" & !is_ps,]

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
    paste0('"', .longname(set_extract$label, set_extract$name), '"', ";")
  )

  coeff_write <- \(extract) {
    if (!write_coefficients || nrow(extract) == 0) {
      return(NULL)
    }
    writeout <- paste(
      "File",
      "(new)",
      extract$name,
      "#",
      extract$name,
      "output file #;\nWrite",
      extract$name,
      "to file",
      extract$name,
      "header",
      paste0('"', extract$name, '"'),
      "longname",
      paste0('"', .longname(extract$label, extract$name), '"', ";")
    )
    return(writeout)
  }
  coeff_writeout <- coeff_write(coeff_extract)

  ps_exec <- is_ps &
    tolower(model$type) %in% c("coefficient", "formula", "assertion", "zerodivide")
  postsim_block <- NULL
  if (any(ps_exec)) {
    postsim_block <- c(
      "PostSim (Begin);",
      model$tab[ps_exec],
      coeff_write(model[model$type == "Coefficient" & is_ps, ]),
      "PostSim (End);"
    )
  }

  tab <- paste(
    c(
      model$tab[!ps_exec],
      backsolve_writeout,
      set_writeout,
      coeff_writeout,
      postsim_block
    ),
    collapse = "\n"
  )
  
  attr(tab, "file") <- attr(model, "tab_file")
  class(tab) <- c("tab", class(tab))
  return(tab)
}

#' @keywords internal
#' @noRd
.longname <- function(label,
                      name) {
  longname <- trimws(gsub("#", "", label))
  missing <- is.na(label) | !nzchar(longname)
  longname[missing] <- name[missing]
  return(longname)
}
