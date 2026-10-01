# a deployment without VPQ types cannot be checked

    x No variable in the model file has a VPQ type, so there is nothing to check.
    x Declare types with `VPQType=` qualifiers, `(begins <prefix> default VPQType <type>)` rules or `(Name <variable> VPQType <type>)` statements (GEMPACK manual 57.2).

---

    x This deployment carries no VPQ types.
    x Deploy the model with this version of teems; `teems::ems_homogeneity()` reads the types `teems::ems_model()` found in the model file.

---

    `type` must be one of "nominal" or "real", not "both".

---

    x `simulate` must be "TRUE" or "FALSE".

