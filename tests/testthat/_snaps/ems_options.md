# ems_option_get errors on invalid name

    `name` must be one of "tempdir", "verbose", "ndigits", "zero_below", "accuracy_threshold", "accuracy_figures", "accuracy_criterion", "required_figures", "full_exclude", "check_shock_status", "timestep_header", "n_timestep_header", "docker_tag", "assertions", "range_test_initial", "range_test_updated", "random_seed", "refine", and "converge", not "not_an_option".

# ems_option errors when write_dir does not exist

    Code
      ems_option_set(tempdir = file.path(basename(getwd()), "does_not_exist"))
    Condition
      Error in `ems_option_set()`:
      ! 'testthat/does_not_exist' does not exist.

# zero_below is validated, stored, applied to written values and reset

    `zero_below` must be a single non-negative finite number.

---

    `zero_below` must be a single non-negative finite number.

# ems_option_set rejects invalid values

    `verbose` must be TRUE or FALSE.

---

    `ndigits` must be an integer or coercible to one.

---

    `accuracy_threshold` must be a numeric between 0 and 1.

---

    `check_shock_status` must be TRUE or FALSE.

---

    `timestep_header` must be an upper case character vector.

---

    `n_timestep_header` must be an upper case character vector.

---

    `full_exclude` must be a character vector.

---

    `docker_tag` must be a character vector.

# ems_option_set sets the solver run modes and rejects other values

    `assertions` must be one of "fatal", "warn" or "off".

---

    `range_test_initial` must be one of "fatal", "warn" or "off".

---

    `range_test_updated` must be one of "fatal", "warn" or "off".

# random_seed is validated, stored as an integer and reset

    `random_seed` must be a single whole number from 0 to 2147483647.

# the refine option is validated, stored and reset

    `refine` must be TRUE or FALSE.

# the converge option is validated, stored and reset

    `converge` must be TRUE or FALSE.

# the accuracy figures and criterion options are validated, stored and reset

    `accuracy_figures` must be a single whole number from 1 to 6.

---

    `accuracy_criterion` must be one of "solution", "data" or "both".

# the required_figures option is validated, stored, reset and sent as -rqf

    `required_figures` must be a single whole number from 1 to 6.

