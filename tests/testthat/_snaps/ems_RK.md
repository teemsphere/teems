# an h_init above the uniform step is refused by name (R5-B3)

    x `h_init` (0.5) exceeds the uniform first step `1/steps` = 0.25.
    i The solver starts from the smaller of the two; raise `steps` or lower `h_init`.

# the dot form of a Runge-Kutta control is refused in ems_RK

    x `rk_chart` is the `ems_solve()` dot form of `chart`; `ems_RK()` takes the formal argument.

---

    x `rk_h0` and `rk_guard` are the `ems_solve()` dot form of `h_init` and `guard`; `ems_RK()` takes the formal arguments.

# unknown dot arguments abort

    x Unknown argument `bogus_argument` passed to `...`.
    i `...` accepts the Runge-Kutta step controls (`adaptive`, `eps_tolerance`, `max_retries`, `retry_adjust`, `rk_chart`, `rk_norm`, `rk_controller`, `rk_scope`, `rk_h0`, `rk_guard`; see `ems_RK()`), the MA48 workspace initial guesses (`laA`, `laD`, `laDi`) and the expert solver flags (`postsim`, `inmemory`, `fastrefac`, `nsbbdblocks`, `withmc66`, `smllthreads`, `nowrites`, `condest`, `jacdump`, `ma48_cntl2`, `ma48_cntl4`).

