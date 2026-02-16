# ems_model requires both model_input and closure_file

    argument "model_input" is missing, with no default

# ems_model requires closure_file when only model_input provided

    argument "closure_file" is missing, with no default

# ems_model requires model_input when only closure_file provided

    argument "model_input" is missing, with no default

# ems_model rejects non-character model_input

    x `model_input` must be a character, not a number.

# ems_model rejects non-character closure_file

    x `closure_file` must be a character, not `TRUE`.

---

    x `closure_file` must be a character, not a number.

# ems_model rejects non-existent model_input file

    x Cannot open file 'not_a_file': No such file.

# ems_model rejects non-existent closure_file

    x Cannot open file 'not_a_file': No such file.

# ems_model rejects non-character var_omit

    x `var_omit` must be a NULL or character, not a number.

# ems_model rejects invalid variable names in var_omit

    x "not_a_var" designated for variable omission not found within the provided model input.

# ems_model rejects invalid coefficient arguments

    x The aggregated data coefficient `NOT_A_COEFF` is not declared within the provided model input.

# invalid numeric to a formula

    x Numeric value directly assigned must be length one.
    i Use a `data.frame` with the appropriate sets to assign multiple heterogeneous numeric values to a coefficient.

# ignored tab statement

    ! The following model statements Postsim are unsupported and will be ignored.

# invalid tab statement

    x The current version of teems does not support Omit.
    i Supported statements include: File, Coefficient, Read, Update, Set, Subset, Formula, Assertion, Variable, Equation, Write, and Zerodivide.

# invalid intertemporal header

    x The intertemporal timestep header required is currently "YEAR" but this header is not loaded.
    i See `teems::ems_option_set()` `timestep_header` for setting a custom timestep header.

# invalid read statement

    x Read statements missing "from file" detected.

# invalid binary set switch statement

    x A colon was detected within a Set definition indicating the use of an unsupported binary switch.
    i Declare sets explicitly within the Tablo file or using `...` within `teems::ems_model()`.
    i For example, Set ENDWM # mobile endowment # (capital,unsklab,sklab); not Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,"mobile") ne 0);.

# identical set assignment

    x It appears that one set has been defined as identical to a second set: Set SET_B # example Set B # = SET_A;.
    i If duplicate sets are desired, multiple Read statements should be implemented.
    i For example Set SET_A # example set A # maximum size 5 read elements from file GTAPSETS header "H2"; and Set SET_B # example set B # maximum size 5 read elements from file GTAPSETS header "H2";)

# invalid set qualifier

    x Invalid set qualifier detected: "(static)".

# multiple set operators

    x Multiple "+" and/or "-" were detected within a single Tablo Set statement.
    i For compatibility, split into multiple statements.
    i Instead of Set ENDWCFS # multiple op # = ENDWC + ENDWF + ENDWS;, Set ENDWCF # one op # = ENDWC + ENDWF; and Set ENDWCFS # second op # = ENDWCF + ENDWS;.

