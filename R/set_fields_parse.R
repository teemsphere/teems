#' @importFrom purrr map map_chr
#' @keywords internal
#' @noRd
.parse_set_fields <- function(sets,
                              call) {
  sets$qualifier_list <- ifelse(
    substr(sets$remainder, 1, 1) == "(",
    .get_element(
      input = sets$remainder,
      split = " ",
      index = 1
    ),
    NA
  )

  sets$qualifier_list <- ifelse(is.na(sets$qualifier_list),
    "(non_intertemporal)",
    sets$qualifier_list
  )

  valid_qual <- c("(intertemporal)", "(non_intertemporal)")
  if (any(!tolower(sets$qualifier_list) %in% valid_qual)) {
    invalid_qual <- setdiff(tolower(sets$qualifier_list), valid_qual)
    .cli_action(model_err$invalid_set_qual,
      action = "abort",
      call = call
    )
  }

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$qualifier_list
  )

  sets$name <- trimws(ifelse(grepl("#", sets$remainder),
    .get_element(
      input = sets$remainder,
      split = "#",
      index = 1
    ),
    ifelse(grepl("\\(", sets$remainder),
      .get_element(
        input = sets$remainder,
        split = "\\(",
        index = 1
      ),
      .get_element(
        input = sets$remainder,
        split = "=",
        index = 1
      )
    )
  ))
  
  sets$name <- ifelse(grepl("\\s", sets$name),
                      purrr::map_chr(strsplit(sets$name, "\\s"), 1),
                      sets$name)
  sets$name <- trimws(sub("=.*$", "", sets$name))

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$name
  )

  sets$label <- ifelse(grepl("#", sets$remainder),
    trimws(purrr::map(strsplit(sets$remainder, "#"), 2)),
    NA
  )

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$label
  )

  sets$remainder <- trimws(x = gsub(
    pattern = "#",
    replacement = "",
    x = sets$remainder
  ))

  sets$max_size <- unlist(x = ifelse(
    test = grepl(
      pattern = "maximum size",
      x = sets$remainder,
      ignore.case = TRUE
    ),
    yes = paste("maximum size", purrr::map(.x = strsplit(
      x = sets$remainder, split = " "
    ), 3)),
    no = NA
  ))

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$max_size
  )

  sets$full_read <- ifelse(
    test = grepl(pattern = "read elements from file", x = tolower(x = sets$remainder)),
    yes = sets$remainder,
    no = NA
  )

  sets$file <- ifelse(
    test = !is.na(x = sets$full_read),
    yes = unlist(x = purrr::map(
      .x = strsplit(x = toupper(x = sets$full_read), split = toupper(x = "from file")),
      .f = \(s) {
        trimws(x = purrr::map(strsplit(
          x = s[2], split = toupper("header")
        ), 1))
      }
    )),
    no = NA
  )

  sets$header <- ifelse(test = !is.na(x = sets$full_read),
    yes = unlist(x = purrr::map(
      .x = strsplit(x = toupper(x = sets$full_read), split = toupper(x = "from file")),
      .f = \(s) {
        trimws(x = purrr::map(strsplit(x = s[2], split = toupper("header")), 2))
      }
    )),
    no = NA
  )

  sets$header <- gsub(pattern = "\"", replacement = "", x = sets$header)

  hdr_long <- !is.na(sets$header) & nchar(trimws(sets$header)) > 4L
  if (any(hdr_long)) {
    bad_set <- sets$name[hdr_long][1]
    bad_header <- trimws(sets$header[hdr_long][1])
    .cli_action(model_err$set_header_len,
      action = "abort",
      call = call
    )
  }

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$full_read
  )

  sets$size <- ifelse(test = grepl(pattern = "size", x = tolower(x = sets$remainder)),
    yes = trimws(x = purrr::map(.x = strsplit(x = sets$remainder, split = "\\("), 1)),
    no = NA
  )

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$size
  )

  sets$definition <- ifelse(test = sets$remainder != "",
    yes = sets$remainder,
    no = NA
  )
  return(sets)
}
