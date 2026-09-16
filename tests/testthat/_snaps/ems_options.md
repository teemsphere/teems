# ems_option_get errors on invalid name

    `name` must be one of "verbose", "tempdir", "ndigits", "accuracy_threshold", "check_shock_status", "timestep_header", "n_timestep_header", "full_exclude", "docker_tag", and "version_check", not "not_an_option".

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

# docker tag auto-selection

    Code
      tag <- .resolve_docker_tag()
    Message
      i Using image teems:v3 (matches host CPU capability "x86-64-v3"). Set `docker_tag` via `ems_option_set()` to override.

