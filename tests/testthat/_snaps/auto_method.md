# the memory fit check refuses a run past the error band and warns inside it

    x Estimated peak memory 14.8 GB for "DBBD" at 4 tasks exceeds the container's 12.0 GB.
    i The estimate is 1.93 kB per equation over 7,690,000 plain-equivalent equations (measured 2026-09 on whole containers; conservative by up to a quarter), so the run would be killed by the memory limit, not fail cleanly.
    i Remedies: a coarser aggregation, condensation (`backsolve` in `teems::ems_model()`), a higher Docker Desktop memory limit, fewer tasks under "DBBD", "NDBBD" at one task for an intertemporal model, or a larger host.

