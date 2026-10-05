# Runge-Kutta subintervals pass only in a complementarity run with an accurate run

    x `n_subintervals` must be 1 when `solution_method` is "DoPri54", except in a complementarity run with an accurate run.
    i Elsewhere subintervals restart the integrator and only benefit the extrapolating methods; increase `steps` (or use `adaptive`) instead. In a complementarity run each subinterval re-predicts the states (an approximate and an accurate run), with `steps` steps in each.

