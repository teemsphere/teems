# malformed mapping declarations abort

    x Malformed Mapping statement: "Mapping REGTOBLOC of REG onto BLOC"
    i Expected `Mapping [(onto)] <name> from <set> to <set>;` (GEMPACK manual 11.9.1).

# mapping with undeclared sets aborts

    x Set "BLOC" in the Mapping declaration of "REGTOBLOC" is not declared in the model.

# mapping name clashes abort

    x Name declared as both a mapping and a variable: "pop".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

---

    x Name declared as both a mapping and a set: "bloc".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

# duplicate mapping declarations abort

    x Duplicate mapping declaration: "rb" (GEMPACK manual 11.2.1).

# by_elements read of a non-mapping aborts

    x `Read (by_elements)` target "pop" is not a declared mapping (GEMPACK manual 11.9.3).

# partial reads of a coefficient abort

    x Partial Read statements are not supported.

# mapping without a read aborts

    x Mapping "regtobloc" has no `Read` or `Formula` assigning its values (GEMPACK manual 11.9.1).

# partial reads still abort with by_elements allowed

    x Partial Read statements are not supported.

# a mapping over a conflicted intersection set aborts

    x The domain set ANX of mapping "RB" is built by an INTERSECT whose operands disagree about the source composition of element "x".
    i The by_elements composition depends on which source elements aggregate into this element; align the aggregation mappings (or the set definitions) so both operands agree.

# mapping header missing from the data aborts

    x No header "MBLC" found in the input data for mapping "REGTOBLOC".
    i `Read (by_elements)` data must be supplied as a character header in the `teems::ems_data()` inputs.

# integer mapping values outside the codomain abort

    x Mapping "REGTOBLOC" header "MBLI" holds a value that is not an element number of the codomain set BLOC (1 to 2; GEMPACK manual 11.9.1): 3.

# integer mapping header missing from the data aborts

    x No integer header "MBLI" found in the input data for mapping "REGTOBLOC".
    i A `Read` of a mapping without `(by_elements)` takes element numbers of the codomain, supplied as an integer header in the `teems::ems_data()` inputs (GEMPACK manual 11.9.1).

# mapping header count mismatch aborts

    x Mapping "REGTOBLOC" header "MBLC" holds 2 values; the domain set REG has 163 elements in the input data.

# mapping values outside the codomain abort

    x Mapping "REGTOBLOC" value "blk9" is not an element of the codomain set BLOC.

# a split mapping under aggregation aborts

    x Aggregated REG element "row" merges source elements with different BLOC values: blk2 from aus; blk1 from nzl, xoc, hkg and 157 more.
    i Mapping "REGTOBLOC" cannot be composed under this aggregation; revise the REG aggregation or the "MBLC" data.

# onto coverage is re-checked on the aggregated sets

    x Codomain element "blk2" of the `(onto)` mapping "REGTOBLOC" is not covered after aggregation.
    i Every BLOC element must be the value of at least one REG element (GEMPACK manual 11.9.1).

