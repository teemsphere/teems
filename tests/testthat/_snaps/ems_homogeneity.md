# unknown or unused solver arguments abort (R6-B5)

    x Unknown argument "n_task" in `...`.
    i The dots take the `teems::ems_solve()` arguments of the simulation run (for example `matrix_method`, `n_tasks`, `solution_method`, `steps`).

---

    x Unknown argument "matrix_methd" in `...`.
    i The dots take the `teems::ems_solve()` arguments of the simulation run (for example `matrix_method`, `n_tasks`, `solution_method`, `steps`).

---

    x Argument "n_tasks" in `...` applies to the simulation run only; set `simulate = TRUE` or drop it.

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

