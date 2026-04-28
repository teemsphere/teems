# ems_data requires dat_input argument

    x argument `dat_input` is missing, with no default

# ems_data requires par_input argument

    x argument `par_input` is missing, with no default

# ems_data requires set_input argument

    x argument `set_input` is missing, with no default

# ems_data requires REG argument

    x Set mappings are required as named arguments in `...`.

# ems_data rejects non-character dat_input

    x `dat_input` must be a character, not a number.

# ems_data rejects non-character par_input

    x `par_input` must be a character, not a number.

# ems_data rejects non-character set_input

    x `set_input` must be a character, not a number.

# ems_data rejects non-character aux_input

    x `aux_input` must be a NULL, list, array, or character, not a number.

# ems_data rejects non-character REG

    x `REG` must be a character or data.frame, not a number.

# ems_data rejects invalid internal mapping name

    x Internal mapping "not_an_internal_mapping" does not exist for set "REG".
    i Available internal mappings for "REG" include "AR5", "big3", "full", "huge", "large", "medium", "R32", "WB23", and "WB7"

# ems_data rejects non-existent CSV file

    x Cannot open file 'not_a_file.csv': No such file.

# ems_data rejects wrong file extension for mapping

    x must be a "csv" file, not a "txt" file.

# ems_data rejects invalid mapping values in CSV

    x The REG mapping has no entries for "afg".

# ems_data rejects CSV with extra columns

    ! The REG mapping has more than 2 columns; only columns 1 (origin) and 2 (destination) will be used.

# ems_data rejects CSV with insufficient columns

    x The REG mapping requires both an origin and destination column.

# ems_data rejects invalid target_format

    x Invalid `target_format`.
    i Valid target formats: "GTAPv6" and "GTAPv7".

# ems_data unnecessary convert

    ! Data format already matches `target_format`; no conversion applied.

# ems_data rejects unrecognized set arguments

    x No internal mappings exist for set not_a_set.

# ems_data rejects unrecognized set arguments with CSV mapping

    x No loaded set data corresponds to the not_a_set mapping.

# ems_data rejects duplicate time_steps

    x One or more `time_steps` does not progress into the future.

# ems_data warns wrong initial year

    ! Initial timestep is neither "0" nor the dat reference year (2017).

