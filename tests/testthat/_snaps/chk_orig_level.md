# an ORIG_LEVEL naming an undeclared coefficient aborts

    x `ORIG_LEVEL=nosuch` on variable qfd names no declared coefficient (GEMPACK manual 11.6.5).

# an ORIG_LEVEL coefficient over other sets aborts

    x `ORIG_LEVEL=vdgb` on variable qfd: the coefficient ranges over "COMM" and "REG", the variable over "COMM", "ACTS", and "REG".
    x The coefficient must range over exactly the variable's sets, in the same order (GEMPACK manual 11.6.5).

# an integer ORIG_LEVEL coefficient aborts

    x `ORIG_LEVEL=nint` on variable qfd names an integer coefficient; it must be real (GEMPACK manual 11.6.5).

