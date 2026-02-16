#' @importFrom purrr pluck
#' @importFrom data.table setcolorder as.data.table setkeyv
#'
#' @keywords internal
#' @noRd
.inject_v_df <- function(input,
                         model,
                         call) {

  req_col <- c(purrr::pluck(model, "ls_mixed_idx", attr(input, "name")), "Value")
  input_col <- colnames(input)

  if (!all(req_col %in% input_col)) {
    .cli_action(model_err$injection_missing_col,
      action = c("abort", "inform"),
      call = call
    )
  }

  input[] <- lapply(input, function(c) {
    if (is.factor(c)) {
      as.character(c)
    } else {
      c
    }
  })

  if (!inherits(input, "data.table")) {
    input <- data.table::as.data.table(input)
  }

  if (req_col %!=% input_col) {
    data.table::setcolorder(input, req_col)
  }

  data.table::setkeyv(input, setdiff(req_col, "Value"))
  return(input)
}