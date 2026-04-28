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

    x `POP` input has no "Value" column.

# ems_data errors when aux data has wrong dimensions

    x Replacement data for POP has dimensions 2 and 2; expected 160.
    i For loading aggregated data, use the `...` argument of `teems::ems_model()`.

# ems_data errors when aux data is missing elements for replacement header

    x Element names in POP for REG do not cover all elements of the data being replaced.
    i Missing elements include "afg".

# ems_data errors when aux data is missing elements for new header

    x Element names in new_header do not cover all elements of set REG.
    i Missing elements include "afg".

# ems_data issues warning when type is different for replacement header

    ! The `type` provided to POP differs from that of the data it replaces: "dat".

# ems_data issues warning when renaming aux data set with recognized ele

    ! Set population in new_header renamed to REG (identical elements).

# ems_data issues warning when changing name on new header with recognized ele

    ! Set RREG in new_header renamed to REG (identical elements).

# ems_data issues warning when changing name on replacement header with recognized ele

    ! Inconsistent set name RREG found in the header to be replaced.
    i All elements are present; likely a set used in multiple positions. Treating as equivalent to REG.

# ems_data errors when unrecognized set and incomplete ele

    x Set population in new_header is not found in loaded data by name or elements.

# ems_data errors when aux data has unrecognizable set

    x Set FAKE_SET in rndm is not found in loaded data by name or elements.

