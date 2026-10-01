# A data program: a TAB without equations (GEMPACK manual 5.1.2). Reads
# domestic purchases, totals them by region and asserts the totals.
write_data_program <- function(dir, extra = character(0)) {
  path <- file.path(dir, "dataonly.tab")
  writeLines(c(
    "File GTAPSETS # sets #;",
    "File GTAPDATA # data #;",
    "Set REG # regions # read elements from file GTAPSETS header \"REG\";",
    "Set COMM # commodities # read elements from file GTAPSETS header \"COMM\";",
    "Set ACTS # activities # read elements from file GTAPSETS header \"ACTS\";",
    "Coefficient (all,c,COMM)(all,a,ACTS)(all,r,REG) VDFB(c,a,r) # domestic purchases #;",
    "Read VDFB from file GTAPDATA header \"VDFB\";",
    "Coefficient (all,r,REG) TOT(r) # total domestic purchases #;",
    "Formula (all,r,REG) TOT(r) = sum{c,COMM, sum{a,ACTS, VDFB(c,a,r)}};",
    "Assertion # TOT nonnegative # (all,r,REG) TOT(r) >= 0;",
    extra
  ), path)
  path
}
