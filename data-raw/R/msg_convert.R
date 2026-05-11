build_convert_err <- function() {
  list(
    format = "{.arg target} set to {.field {target}} but data appears to already be this format.",
    same_format = "{.arg target} must differ from {.arg origin}." 
  )
}