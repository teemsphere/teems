#' @keywords internal
#' @noRd
.rewrite_equation_if <- function(stmt,
                                 synth,
                                 call) {
  header <- .parse_eq_header(stmt)
  qual <- header$qual
  name <- header$name
  label <- header$label

  context <- .eq_quant_sides(groups = header$groups, rest = header$rest)
  quant <- context$quant
  q_idx <- context$q_idx

  if_pattern <- "(^|[^A-Za-z0-9_@])[Ii][Ff]\\s*[][({]"

  passed <- .rewrite_if_terms(
    sides = context$sides,
    quant = quant,
    q_idx = q_idx,
    synth = synth,
    stmt = stmt,
    if_pattern = if_pattern,
    call = call
  )
  side_terms <- passed$side_terms
  pre <- passed$pre
  membership <- passed$membership

  if (length(membership) %=% 0L) {
    rewritten <- c(pre, .assemble_eq(side_terms, quant, name, qual, label))
    return(rewritten)
  }

  domains <- .if_membership_domains(
    membership = membership,
    quant = quant,
    q_idx = q_idx,
    synth = synth,
    stmt = stmt,
    pre = pre,
    call = call
  )
  pre <- domains$pre
  at <- domains$at
  inter_names <- domains$inter_names
  idx_name <- domains$idx_name

  n_m <- length(membership)
  eq_names <- .synth_eq_names(name, synth, n_m + 1L)

  out <- character(0)
  for (k in seq_len(n_m)) {
    q_k <- quant
    q_k[[at]]$text <- sprintf("(all,%s,%s)", idx_name, inter_names[k])
    chunks_k <- .split_eq_chunks(
      keep = k,
      side_terms = side_terms,
      membership = membership,
      if_pattern = if_pattern
    )
    eq_k <- .assemble_eq(chunks_k, q_k, eq_names[k], qual, label)
    if (grepl(if_pattern, membership[[k]]$value)) {
      eq_k <- .rewrite_equation_if(eq_k, synth, call)
    }
    out <- c(out, eq_k)
  }
  q_out <- quant
  q_out[[at]]$text <- sprintf("(all,%s,%s)", idx_name, domains$comp)
  chunks_rest <- .split_eq_chunks(
    keep = 0L,
    side_terms = side_terms,
    membership = membership,
    if_pattern = if_pattern
  )
  out <- c(out, .assemble_eq(chunks_rest, q_out, eq_names[n_m + 1L], qual, label))

  rewritten <- c(pre, out)
  return(rewritten)
}
