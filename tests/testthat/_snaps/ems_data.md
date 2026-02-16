# ems_data requires dat_input argument

    argument "dat_input" is missing, with no default

# ems_data requires par_input argument

    argument "par_input" is missing, with no default

# ems_data requires set_input argument

    argument "set_input" is missing, with no default

# ems_data requires REG argument

    x Set mappings passed to `...` as a pairwise list are required.

# ems_data rejects non-character dat_input

    x `dat_input` must be a character, not a number.

# ems_data rejects non-character par_input

    x `par_input` must be a character, not a number.

# ems_data rejects non-character set_input

    x `set_input` must be a character, not a number.

# ems_data rejects non-character aux_input

    x `aux_input` must be a NULL or list, not a number.

# ems_data rejects non-character REG

    x `REG` must be a character, not a number.

# ems_data rejects invalid internal mapping name

    x The internal mapping selected: "not_an_internal_mapping", for set "REG" does not exist.
    i Available internal mappings for "REG" include "AR5", "big3", "full", "huge", "large", "R32", "WB23", and "WB7"

# ems_data rejects non-existent CSV file

    x Cannot open file 'not_a_file.csv': No such file.

# ems_data rejects wrong file extension for mapping

    x must be a "csv" file, not a "txt" file.

# ems_data rejects invalid mapping values in CSV

    x The set mapping loaded for REG is missing mappings for "afg".

# ems_data rejects CSV with extra columns

    ! The set mapping loaded for REG contains more than 2 columns. Only the first (origin element) and second (mapped element) columns will be utilized.

# ems_data rejects CSV with insufficient columns

    x The set mapping loaded for REG does not contain both an origin element column and a mapping column.

# ems_data rejects invalid target_format

    x Invalid `target_format`.
    i Valid target formats currently include: "GTAPv6" and "GTAPv7".

# ems_data unnecessary convert

    ! The retrieved data format is identical to the `target_format` specified. No conversion has taken place.

# ems_data rejects unrecognized set arguments

    x No internal mappings were found for the set not_a_set.

# ems_data rejects unrecognized set arguments with CSV mapping

    x There is no loaded set data which corresponds to the loaded set map: not_a_set.

# ems_data rejects duplicate time_steps

    x One or more `time_steps` does not progress into the future.

# ems_data warns wrong initial year

    ! The initial timestep provided is neither 0 nor the reference year corresponding to the dat file loaded: 2017.

