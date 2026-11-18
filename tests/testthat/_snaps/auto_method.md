# the memory fit check refuses a run past the error band and warns inside it

    x Estimated peak memory 14.8 GB for "DBBD" at 4 tasks exceeds the container's 12.0 GB.
    i The estimate is 1.93 kB per equation over 7,690,000 plain-equivalent equations (measured 2026-09 on whole containers; conservative by up to a quarter), so the run would be killed by the memory limit, not fail cleanly.
    i Remedies for a static model: a coarser aggregation, fewer tasks under "DBBD", a higher Docker Desktop memory limit, or a larger host.

# the memory fit check targets its remedies by model type

    x Estimated peak memory 57.8 GB for "SBBD" at 8 tasks exceeds the container's 33.6 GB.
    i The estimate is 0.52 kB per equation over 111,100,277 plain-equivalent equations (measured 2026-09 on whole containers; conservative by up to a quarter), so the run would be killed by the memory limit, not fail cleanly.
    i Remedies for an intertemporal model: fewer time steps, a coarser aggregation, "NDBBD" at one task, a higher Docker Desktop memory limit, or a larger host.

