# ems_aux errors when input is missing

    x argument `input` is missing, with no default

# ems_aux errors when type is missing

    x argument `type` is missing, with no default

# ems_aux errors when header is missing for data frame input

    x `header` must be provided if `input` is a data frame or path to a CSV file.

# ems_aux errors when header is missing for CSV input

    x `header` must be provided if `input` is a data frame or path to a CSV file.

# ems_aux errors when input is numeric

    x `input` must be a character or data.frame, not a number.

# ems_aux errors when input is logical

    x `input` must be a character or data.frame, not `TRUE`.

# ems_aux errors when data frame lacks Value column

    x The data provided to `POP` does not contain a "Value" column.

# ems_data errors when aux data has wrong dimensions

    x The dimensions of replacement data for POP, 2 and 2, are not identical to the data being replaced: 160.
    i For loading aggregated data, use the `...` argument of `teems::ems_model()`.

# ems_data errors when aux data is missing elements for replacement header

    x POP element names provided for REG do not contain all elements within the data being replaced.
    i Missing elements include "afg".

# ems_data errors when aux data is missing elements for new header

    x Element names provided for new_header do not contain all elements associated with the set REG.
    i Missing elements include "afg".

# ems_data issues warning when type is different for replacement header

    ! The `type` provided to POP differs from that of the data it replaces: "dat".

# ems_data issues warning when renaming aux data set with recognized ele

    ! The set population within new_header has been renamed to REG due to identical elements.

# ems_data issues warning when changing name on new header with recognized ele

    ! The set RREG within new_header has been renamed to REG due to identical elements.

# ems_data issues warning when changing name on replacement header with recognized ele

    ! An inconsistent set name (RREG) was identified when compared to the header to be replaced.
    i All associated set elements are present so this is likely due to a set used in multiple positions and will be presumed to be equivalent to REG.

# ems_data errors when unrecognized set and incomplete ele

    x The new_header set population is not found among loaded data, neither in terms of its name or constituitive elements.

# ems_data errors when aux data has unrecognizable set

    x The rndm set FAKE_SET is not found among loaded data, neither in terms of its name or constituitive elements.

