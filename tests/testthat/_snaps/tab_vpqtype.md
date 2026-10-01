# an unknown VPQ type aborts

    x Unknown VPQ type "cost".
    x A VPQ type is one of "Value", "Price", "Quantity", "None" or "Unspecified" (GEMPACK manual 57.2).

---

    x Unknown VPQ type "cost".
    x A VPQ type is one of "Value", "Price", "Quantity", "None" or "Unspecified" (GEMPACK manual 57.2).

# conflicting VPQ types for one variable abort

    x Variable p is given more than one VPQ type by its `VPQType=` qualifier and `(Name ... VPQType ...)` statements (GEMPACK manual 57.2).

# a malformed VPQ type statement aborts

    x Malformed VPQ type statement: `Variable (begins p VPQType Price)`.
    x The forms are `Variable (begins <prefix> default VPQType <type>);`, `Variable (begins <prefix> VPQType default OFF);` and `Variable (Name <variable> VPQType <type>);` (GEMPACK manual 57.2).

# a Name statement for an undeclared variable warns

    ! `(Name ... VPQType ...)` for undeclared variable ghost ignored (GEMPACK manual 57.2).

