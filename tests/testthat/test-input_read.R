skip_on_cran()

test_that("HAR character fields are tagged from their bytes", {
  width <- 8L
  pad <- function(bytes) c(bytes, rep(as.raw(0x20), width - length(bytes)))
  fields <- c(
    pad(charToRaw("usa")),
    # "Côte d'I" in Latin-1: the byte 0xf4 is not valid UTF-8
    pad(as.raw(c(0x43, 0xf4, 0x74, 0x65, 0x20, 0x64, 0x27, 0x49))),
    # "Türkiye" in UTF-8
    pad(as.raw(c(0x54, 0xc3, 0xbc, 0x72, 0x6b, 0x69, 0x79, 0x65)))
  )
  out <- har_fixed_width_strings(fields, width, TRUE)
  expect_length(out, 3L)
  # ASCII carries no mark, a non-UTF-8 field is Latin-1, valid UTF-8 is UTF-8
  expect_equal(Encoding(out), c("unknown", "latin1", "UTF-8"))
  expect_equal(enc2utf8(out[[2]]), "Côte d'I")
  expect_equal(out[[3]], "Türkiye")
  # the case conversion that aborted on a mis-tagged field runs
  expect_equal(tolower(out[[2]]), "côte d'i")
  # untrimmed fields keep their padding and their tag
  raw_out <- har_fixed_width_strings(fields, width, FALSE)
  expect_equal(nchar(raw_out[[1]]), width)
  expect_equal(Encoding(raw_out[[2]]), "latin1")
})
