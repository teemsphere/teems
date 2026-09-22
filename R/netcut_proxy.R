#' @keywords internal
#' @noRd
.netcut_proxy <- function(ref,
                          vars,
                          synth) {
  v <- match(ref$name, vars$name)
  args <- ref$args[[1]]
  fixed <- grepl("^\"", args)
  key <- paste0(
    "NC|", toupper(ref$name), "|",
    paste0(which(fixed), "=", tolower(args[fixed]), collapse = "|")
  )
  cached <- synth[[key]]
  keep_args <- args[!fixed]
  if (!is.null(cached)) {
    proxy <- list(
      pre = character(0),
      after = vars$stmt[v],
      ref = paste0(cached, "(", paste(keep_args, collapse = ","), ")")
    )
    return(proxy)
  }
  nm <- .synth_proxy_name(synth)
  synth[[key]] <- nm
  keep_idx <- vars$idx[[v]][!fixed]
  keep_sets <- vars$sets[[v]][!fixed]
  quants <- paste0("(all,", keep_idx, ",", keep_sets, ")", collapse = "")
  src_args <- args
  src_args[!fixed] <- keep_idx
  src_args <- sub("\\s*[+-]\\s*[0-9]+$", "", src_args)
  src <- paste0(ref$name, "(", paste(src_args, collapse = ","), ")")
  synth$rewrites <- c(synth$rewrites, paste0(nm, " = ", src))
  pre <- c(
    sprintf(
      "Variable %s%s %s(%s) # netcut proxy for %s #",
      vars$quals[v], quants, nm, paste(keep_idx, collapse = ","), src
    ),
    sprintf(
      "Equation E_%s # netcut proxy link # %s %s(%s) = %s",
      nm, quants, nm, paste(keep_idx, collapse = ","), src
    )
  )
  proxy <- list(
    pre = pre,
    after = vars$stmt[v],
    ref = paste0(nm, "(", paste(keep_args, collapse = ","), ")")
  )
  return(proxy)
}
