# a builder whose coefficient has no loaded data aborts

    x Set builder "NEWSET": no loaded data for its condition coefficient "NODT".

# a builder with the wrong number of arguments aborts

    x Set builder "NEWSET": condition coefficient "VDFB" has 2 dimensions but 1 argument was given.

# a builder naming an element outside the aggregation aborts

    x Set builder "NEWSET": element "fra" is not in the REG dimension of "VDFB" under the current aggregation.

# a builder looping over the wrong dimension aborts

    x Set builder "NEWSET": the loop index "c" must range over "VDFB"'s dimension set REG exactly (source set COMM differs).

# a builder that selects nothing aborts

    x Set builder "NEWSET" selected no elements of COMM with `VDFB(c,"chn") > 5`.
    i An empty set cannot enter the model (GEMPACK manual 10.1.2); check the condition against the aggregated data.

# a mapping-sum builder without its mapping aborts

    x Set builder "NEWSET": the mapping-conditional sum over "MAPRC" cannot be evaluated at deploy.
    i The sum must range over the mapping's domain set, the builder over its codomain set, and the mapping needs a `(by_elements)` Read whose header is in the set data (GEMPACK manual 10.1.2).

