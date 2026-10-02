# ems_option_get errors on invalid name

    `name` must be one of "verbose", "tempdir", "ndigits", "accuracy_threshold", "check_shock_status", "timestep_header", "n_timestep_header", "full_exclude", "docker_tag", "version_check", "assertions", "range_test_initial", "range_test_updated", "random_seed", "refine", and "convergence_rule", not "not_an_option".

# ems_option errors when write_dir does not exist

    Code
      ems_option_set(tempdir = file.path(basename(getwd()), "does_not_exist"))
    Condition
      Error in `ems_option_set()`:
      ! 'testthat/does_not_exist' does not exist.

# ems_option_set sets version_check and rejects other values

    `version_check` must be one of "abort", "warn" or "off".

---

    `version_check` must be one of "abort", "warn" or "off".

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

    `refine` must be "on" or "off".

# the convergence_rule option is validated, stored and reset

    `convergence_rule` must be "on" or "off".

# docker tag auto-selection

    Code
      tag <- .resolve_docker_tag()
    Message
      i Using image teems:v3 (matches host CPU capability "x86-64-v3"). Set `docker_tag` via `ems_option_set()` to override.

