#' @importFrom purrr map_chr
#' @keywords internal
#' @noRd
.writeout <- function(model,
                      write_dir,
                      sets = TRUE,
                      coeff = TRUE) {
  if (sets) {
    set_names <- model[model$type == "Set", "name"][[1]]
    set_writeout <- paste(
      "outdata",
      paste0('"', set_names, '"'),
      paste0(
        '"',
        file.path(
          write_dir,
          "out",
          "sets",
          paste0(set_names, ".csv")
        ),
        '"',
        ";"
      )
    )
    
    .out_mkdir(write_dir = write_dir,
               coeff = FALSE,
               var = FALSE)
  }

  if (coeff) {
    is_ps <- if (is.null(model$postsim)) {
      rep(FALSE, nrow(model))
    } else {
      !is.na(model$postsim) & model$postsim
    }
    coeff_names <- model[model$type == "Coefficient" & !is_ps, "name"][[1]]
    coeff_writeout <- paste(
      "outdata",
      paste0('"', coeff_names, '"'),
      paste0(
        '"',
        file.path(
          write_dir,
          "out",
          "coefficients",
          paste0(coeff_names, ".csv")
        ),
        '"',
        ";"
      )
    )

    .out_mkdir(write_dir = write_dir,
               sets = FALSE,
               var = FALSE)
  }

  if (sets && coeff) {
    writeout <- c(
      set_writeout,
      coeff_writeout
    )
  } else if (sets && !coeff) {
    writeout <- set_writeout
  } else if (!sets && coeff) {
    writeout <- coeff_writeout
  }
  
  writeout <- purrr::map_chr(writeout, \(c) {
    gsub(write_dir, "/opt/teems", c, fixed = TRUE)
  })

  return(writeout)
}
