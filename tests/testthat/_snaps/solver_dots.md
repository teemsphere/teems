# solve_in_situ solver_args must be a fully named allowlisted list

    x `solver_args` must be a fully named list.
    i It carries the named solver arguments `ems_solve()` accepts through `...`; the in-situ `...` is reserved for the input files.

---

    x Unknown argument `bogus` in `solver_args`.
    i Accepted: the MA48 workspace initial guesses (`laA`, `laD`, `laDi`), the expert solver flags (`postsim`, `inmemory`, `fastrefac`, `cntl_3`, `cntl_6`, `nsbbdblocks`, `withmc66`, `smllthreads`, `tempdir`, `nowrites`, `condest`, `jacdump`, `ma48u`) and the Runge-Kutta run controls (`rk_chart`, `rk_norm`, `rk_controller`, `rk_scope`, `rk_h0`, `rk_guard`). The Runge-Kutta step controls are formal arguments of `solve_in_situ()`.

# ems_probe rejects unknown dot arguments

    x Unknown argument `bogus_argument` passed to `...`.
    i `...` accepts the MA48 workspace initial guesses (`laA`, `laD`, `laDi`) and the expert solver flags (`postsim`, `inmemory`, `fastrefac`, `cntl_3`, `cntl_6`, `nsbbdblocks`, `withmc66`, `smllthreads`, `tempdir`, `nowrites`, `condest`, `jacdump`, `ma48u`, and the Runge-Kutta run controls, which a probe ignores).

