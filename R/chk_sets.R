#' @importFrom purrr map2
#' @importFrom utils tail head
#' 
#' @keywords internal
#' @noRd
.check_sets <- function(bin_csv_paths,
                        model_dir,
                        set_path,
                        minimal,
                        call) {
  sets <- .unite_csvs(
    target = "set_csvs",
    paths = bin_csv_paths
  )
  sets <- .match_set_ele(
    sets_out = sets,
    paths = bin_csv_paths
  )

  if (!minimal) {
    tab_sets <- lapply(set_path, function(s) {
      utils::tail(utils::head(readLines(s), -1), -1)
    })
    names(tab_sets) <- gsub("\\.csv", "", basename(set_path))

    .check_set_consistency(
      bin_sets = sets,
      tab_sets = tab_sets,
      call = call
    )

    r_idx <- match(sets$setname, tolower(names(tab_sets)))
    sets$setname <- ifelse(!is.na(r_idx),
      names(tab_sets)[r_idx],
      sets$setname
    )

    names(sets$ele) <- sets$setname
  }

  if (any(sets$intertemp == 1L)) {
    nms <- names(sets$ele)
    sets$ele <- purrr::map2(
      sets$intertemp,
      sets$ele,
      function(int, e) {
        if (int %=% 1L) {
          e <- as.numeric(e)
          attr(e, "intertemporal") <- TRUE
        }
        return(e)
      }
    )

    names(sets$ele) <- nms
  }

  return(sets$ele)
}