# malformed complementarity aborts

    x Malformed Complementarity statement: "Complementarity (variable = CX, wrong_key = 0) CMPA CX - 3;"
    i Expected `Complementarity (variable = <levels var>, lower_bound/upper_bound = <levels var | parameter | constant>) <name> [quantifiers] <expression>;` (GEMPACK manual 10.17).

# missing variable qualifier aborts

    x Complementarity "Complementarity (lower_bound = 0) CMPA CY - 3;" needs a `variable =` qualifier (GEMPACK manual 11.14).

# non-levels complementarity variable aborts

    x The Complementarity variable "qgdp" must be a declared levels variable (GEMPACK manual 11.14).

# missing bound aborts

    x Complementarity "CMPA" needs at least one of `lower_bound`/`upper_bound` (GEMPACK manual 10.17).

# invalid bound aborts

    x Invalid bound "NPB" in Complementarity "CMPA".
    i A bound must be a levels variable, a `Coefficient (parameter)` or a real constant (GEMPACK manual 10.17).

# long complementarity name aborts

    x Complementarity name "CMPTOOLONGX" exceeds the 10-character limit (GEMPACK manual 11.14/11.2.1).

# quantifier count mismatch aborts

    x Complementarity "CMPA" has 0 quantifiers but "QX" has 1 argument (GEMPACK manual 11.14).

# condensed complementarity variable aborts

    x "CX" cannot be backsolved: it is the variable of Complementarity "CMPA".
    i The complementarity variable must not be substituted out or backsolved (GEMPACK manual 11.14.1).

# active complementarity components join the squaring count

    x The closure does not square the system: 1000 endogenous variable elements against 999 equation elements.
    i Arithmetic: 1984 variable elements - 984 exogenous elements (closure after swaps) = 1000 endogenous; the equation system determines exactly 999, so 1 element must still be exogenized.
    i Candidates: exogenizing 1 element of one of globalcgds, pcgdswld, pt, qtm, rorg closes the gap exactly.
    i If the counts look right but the partition is structurally deficient, run `teems::ems_probe()` on the deployed model for a named diagnosis.

