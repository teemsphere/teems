#' @keywords internal
#' @noRd
.read_har_sparse <- function(headers) {
  for (h in names(headers)) {
    if (headers[[h]]$type %in% c("REFULL", "RESPSE")) {
      headers[[h]]$definedDimensions <- readBin(headers[[h]]$records[[3]][5:8], "integer",
        size =
          4
      )
      headers[[h]]$usedDimensions <- readBin(headers[[h]]$records[[3]][13:16], "integer",
        size =
          4
      )

      if (headers[[h]]$usedDimensions > 0) {
        dnames <- har_fixed_width_strings(
          headers[[h]]$records[[3]][33:(33 + headers[[h]]$usedDimensions * 12 - 1)],
          12L,
          FALSE
        )
        dimNames <- Map(\(f) {
          NULL
        }, 1:headers[[h]]$usedDimensions)
        actualDimsNamesFlags <- headers[[h]]$records[[3]][(33 + headers[[h]]$usedDimensions *
          12) + 0:6]
        actualDimsNames <- ifelse(actualDimsNamesFlags == 0x6b, TRUE, FALSE)
        uniqueDimNames <- unique(dnames[actualDimsNames])

        if (length(uniqueDimNames) > 0) {
          for (d in 1:length(uniqueDimNames)) {
            nele <- readBin(headers[[h]]$records[[3 + d]][13:16], "integer", size = 4)

            ele_names <- har_fixed_width_strings(
              headers[[h]]$records[[3 + d]][17:(17 + nele * 12 - 1)],
              12L,
              TRUE
            )

            for (dd in which(dnames == uniqueDimNames[d])) {
              dimNames[[dd]] <- ele_names
              names(dimNames)[dd] <- trimws(uniqueDimNames[d])
            }
          }
        }

        dataStart <- 3 + length(uniqueDimNames) + 1

        if (headers[[h]]$type == "REFULL") {
          numberOfFrames <- readBin(headers[[h]]$records[[dataStart]][5:8], "integer")
          numberOfDataFrames <- (numberOfFrames - 1) / 2
          dataFrames <- (dataStart) + 1:numberOfDataFrames * 2

          m <- array(
            har_payload_f32(
              headers[[h]]$records[dataFrames],
              8L,
              prod(headers[[h]]$dimensions)
            ),
            dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
            dimnames = dimNames
          )
        } else {
          dataVector <- har_spse_fill(
            headers[[h]]$records[(dataStart + 1):length(headers[[h]]$records)],
            16L,
            prod(headers[[h]]$dimensions)
          )

          m <- array(dataVector,
            dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
            dimnames = dimNames
          )
        }
      } else {
        m <- array(
          readBin(
            headers[[h]]$records[[length(headers[[h]]$records)]][9:length(headers[[h]]$records[[3]])],
            "double",
            size = 4,
            n = prod(headers[[h]]$dimensions)
          ),
          dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions]
        )
      }

      headers[[h]]$data <- m
    }
  }
  return(headers)
}
