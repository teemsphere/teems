#' @importFrom tibble as_tibble
#' @importFrom data.table data.table
#' @useDynLib teems, .registration = TRUE
#'
#' @keywords internal
#' @noRd
.parse_solution <- function(sol_prefix) {
  raw <- parse_solution_bins(sol_prefix)

  set_union <- tibble::as_tibble(data.frame(
    r_idx     = seq_along(raw$set$header) - 1L,
    header    = raw$set$header,
    fileid    = raw$set$fileid,
    setname   = raw$set$setname,
    readele   = raw$set$readele,
    begadd    = raw$set$begadd,
    size      = raw$set$size,
    subsetid  = raw$set$subsetid,
    intertemp = raw$set$intertemp,
    intsup    = raw$set$intsup,
    regional  = raw$set$regional,
    regsup    = raw$set$regsup,
    stringsAsFactors = FALSE
  ))

  var_union <- tibble::as_tibble(data.frame(
    r_idx       = seq_along(raw$var$cofname) - 1L,
    cofname     = raw$var$cofname,
    begadd      = raw$var$begadd,
    size        = raw$var$size,
    setid       = raw$var$setid,
    antidims    = raw$var$antidims,
    matsize     = raw$var$matsize,
    level_par   = raw$var$level_par,
    change_real = raw$var$change_real,
    suplval     = raw$var$suplval,
    gltype      = raw$var$gltype,
    glval       = raw$var$glval,
    stringsAsFactors = FALSE
  ))

  setele <- data.table::data.table(
    r_idx      = seq_along(raw$sel$setele) - 1L,
    mapped_ele = raw$sel$setele
  )

  xc <- data.table::data.table(
    r_idx = seq_along(raw$bin) - 1L,
    Value = raw$bin
  )

  parsed <- list(
    set_union = set_union,
    var_union = var_union,
    setele    = setele,
    xc        = xc
  )
  
  return(parsed)
}
