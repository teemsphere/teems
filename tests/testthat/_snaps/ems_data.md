# ems_data requires dat_input argument

    x argument `dat_input` is missing, with no default

# single-file route needs both or neither of par_input and set_input

    x `par_input` and `set_input` must be given together or both left `NULL` (single-file route).

# single-file route rejects set mappings

    x Set mappings cannot be applied on the single-file route (`par_input` and `set_input` absent); the file is loaded at full resolution.

# ems_data requires REG argument

    x Set mappings are required as named arguments in `...`.

# ems_data rejects non-character dat_input

    x `dat_input` must be a character or list, not a number.

# ems_data rejects non-character par_input

    x `par_input` must be a NULL, character, or list, not a number.

# ems_data rejects non-character set_input

    x `set_input` must be a NULL, character, or list, not a number.

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

# ems_data rejects a data element its mapping does not cover

    x The ENDW mapping has no entries for "natres".

# ems_data warns CSV with extra columns

    ! The REG mapping has more than 2 columns; only columns 1 (origin) and 2 (destination) will be used.

# ems_data rejects CSV with insufficient columns

    x The REG mapping requires both an origin and destination column.

# ems_data rejects unrecognized set arguments

    x No internal mappings exist for set not_a_set.

# ems_data rejects unrecognized set arguments with CSV mapping

    x No loaded set data corresponds to the not_a_set mapping.

# ems_data rejects duplicate time_steps

    x One or more `time_steps` does not progress into the future.

# ems_data warns wrong initial year

    ! Initial timestep is neither "0" nor the dat reference year (2023).

# ems_data errors when dots passed without names

    x Set mappings must be passed as named pairs: `REG = "mapping"`

# ems_data rejects a par_weights method it does not know

    x `par_weights` methods are "share" and "value", not "mean".

# ems_data rejects more than one default par_weights method

    x `par_weights` takes at most one unnamed method, the default for every weighted parameter; "share" and "value" were given.
    i Name the others after the parameters they apply to, e.g. `c("share", ESBM = "value")`.

# ems_data rejects a par_weights parameter the format does not weight

    x `par_weights` names "ESBX", which "GTAPv7" data does not weight.
    i Weighted parameters: "ESBD", "ESBM", "ESBT", "ESBV", "INCP", "SUBP", "ESBC", "ESBG", "ESBI", "ETRQ", "ESBQ", "EFVE", "EAEZ", and "ETRE".

