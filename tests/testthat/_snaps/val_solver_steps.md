# Runge-Kutta subintervals pass only in a complementarity run with an accurate run

    x `n_subintervals` must be 1 when `solution_method` is "DoPri54", except in a complementarity run with an accurate run.
    i Elsewhere subintervals restart the integrator and only benefit the extrapolating methods; increase `steps` (or use `adaptive`) instead. In a complementarity run each subinterval re-predicts the states (an approximate and an accurate run), with `steps` steps in each.

# Johansen takes one subinterval

    x `n_subintervals` must be 1 when `solution_method` is "Johansen".
    i Johansen is a single linear solve of the whole shock. For several updated steps use `solution_method = "Euler"` with one `steps` count.

# a complementarity run without the accurate run takes one subinterval

    x `n_subintervals` must be 1 when the complementarity accurate run is switched off (`do_acc_run = FALSE`).
    i Each subinterval pairs an approximate run with an accurate run; the approximate run on its own covers the whole shock in one pass. Raise `steps_approx_run` in `ems_complementarity()` or keep the accurate run.

