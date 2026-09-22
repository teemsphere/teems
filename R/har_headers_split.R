#' @keywords internal
#' @noRd
.split_har_headers <- function(cf) {
  if (cf[1] == 0xfd) {
    currentHeader <- ""
    headers <- list()
    i <- 2
    while (i < length(cf)) {
      fb <- cf[i]
      i <- i + 1
      bitsLength <- as.integer(rawToBits(fb))[3:8]
      toRead <- as.integer(rawToBits(fb))[1:2]
      toReadBytes <- Reduce(\(a, f) {
        a <- a + 2^(f - 1) * toRead[f]
      }, 1:length(toRead), 0)

      if (toReadBytes > 0) {
        for (i in (i):(i + toReadBytes - 1)) {
          bitsLength <- c(bitsLength, rawToBits(cf[i]))
        }
        i <- i + 1
      }

      recordLength <- Reduce(
        \(a, f) {
          a <- a + 2^(f - 1) * bitsLength[f]
        },
        1:length(bitsLength),
        0
      )

      if (recordLength == 4) {
        currentHeader <- trimws(rawToChar(cf[(i):(i + recordLength - 1)]))
      }
      if (is.null(headers[[currentHeader]])) {
        headers[[currentHeader]] <- list()
      }

      if (is.null(headers[[currentHeader]]$records)) {
        headers[[currentHeader]]$records <- list()
      }

      headers[[currentHeader]]$records[[length(headers[[currentHeader]]$records) +
        1]] <- cf[(i):(i + recordLength - 1)]
      i <- i + recordLength
      totalLength <- recordLength + 1 + toReadBytes
      endingBits <- intToBits(totalLength)
      maxPosition <- max(which(endingBits == 1))

      if (maxPosition <= 6) {
        needEnd <- 0
      } else {
        needEnd <- 0 + ceiling((maxPosition - 6) / 8)
      }

      expectedEnd <- packBits(c(intToBits(needEnd)[1:2], intToBits(totalLength))[1:(8 *
        (needEnd + 1))], "raw")
      expectedEnd <- expectedEnd[length(expectedEnd):1]

      if (any(cf[i:(i + length(expectedEnd) - 1)] != expectedEnd)) {
        stop("Surprising end of record")
      }

      i <- i + length(expectedEnd)
    }
  } else {
    headers <- lapply(
      har_split_records(cf),
      \(r) list(records = r)
    )
  }
  return(headers)
}
