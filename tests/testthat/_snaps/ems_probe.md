# probe print methods run

    Code
      print(healthy)
    Message
      -- Structural probe ---------------------------------------- 10,524 equations --
      v Closure valid: every variable is determined and no equation is redundant, at
        base data too
      i `summary(x)` for the full report

---

    Code
      print(broken)
    Message
      -- Structural probe ---------------------------------------- 10,530 equations --
      x Structurally singular: rank 10,527 of 10,530 (3 unmatched equations, 3
        unmatched variables)
        Undetermined: dprobeb ×3
        Over-constrained: e_dprobe2 ×3
      i Locate it with `plot(x, type = "dm")`
      i `summary(x)` for the full report

---

    Code
      summary(healthy)
    Message
      -- Structural probe: full report -----------------------------------------------
      * System: 10,524 equations
      v Structural rank: 10,524 of 10,524
      v Rank at base data: 10,524 of 10,524
      * Structure: no time chain; no block partition
      * Simultaneous cores: 28 of 6,049 strongly connected components have more than
        one element; the largest has 4,341 rows
      * Largest core by equation: e_qfa ×225, e_qfd ×225, e_qfm ×225, e_pfa ×225,
        e_pfd ×225, e_pfm ×225, and 44 more
      * Statements: 247 equation statements, 856 statement-variable incidences
      -- Heaviest statement-variable incidences --------------------------------------
    Output
      # A tibble: 10 x 4
         eq        var   weight  rows
         <chr>     <chr>  <int> <int>
       1 e_qfa     afa      450   225
       2 e_qfe     afe      360   180
       3 e_ev_alt  qes      360     9
       4 e_qxs     ams      270   135
       5 e_vgdp    qxs      270     9
       6 e_qgdp    qxs      270     9
       7 e_ev_alt  qxs      270     9
       8 e_ev_alt  pfob     270     9
       9 e_cnttotr pfob     270     9
      10 e_qo      pfa      225    45

---

    Code
      summary(broken)
    Message
      -- Structural probe: full report -----------------------------------------------
      * System: 10,530 equations
      x Structural rank: 10,527 of 10,530, structurally singular (3 unmatched
        equations, 3 unmatched variables)
        Under-determined by variable: dprobeb ×3
        Over-constrained by equation: e_dprobe2 ×3
        Dulmage-Mendelsohn blocks: under 0 × 3, well 10,524 × 10,524, over 6 × 3
      x Rank at base data: 10,527 of 10,530, structurally singular (3 unmatched
        equations, 3 unmatched variables)
        Under-determined by variable: dprobeb ×3
        Over-constrained by equation: e_dprobe2 ×3
        Dulmage-Mendelsohn blocks: under 0 × 3, well 10,524 × 10,524, over 6 × 3
      * Simultaneous cores: 28 of 6,049 strongly connected components have more than
        one element; the largest has 4,341 rows
      * Largest core by equation: e_qfa ×225, e_qfd ×225, e_qfm ×225, e_pfa ×225,
        e_pfd ×225, e_pfm ×225, and 44 more
      * Statements: 249 equation statements, 858 statement-variable incidences
      -- Defect elements (capped upstream at 200 per list) ---------------------------
    Output
      # A tibble: 12 x 5
         pattern    side                      element      name      elements 
         <chr>      <chr>                     <chr>        <chr>     <list>   
       1 structural under_determined_variable dprobeb(0)   dprobeb   <chr [1]>
       2 structural under_determined_variable dprobeb(1)   dprobeb   <chr [1]>
       3 structural under_determined_variable dprobeb(2)   dprobeb   <chr [1]>
       4 structural over_constrained_equation e_dprobe2(0) e_dprobe2 <chr [1]>
       5 structural over_constrained_equation e_dprobe2(1) e_dprobe2 <chr [1]>
       6 structural over_constrained_equation e_dprobe2(2) e_dprobe2 <chr [1]>
       7 realized   under_determined_variable dprobeb(0)   dprobeb   <chr [1]>
       8 realized   under_determined_variable dprobeb(1)   dprobeb   <chr [1]>
       9 realized   under_determined_variable dprobeb(2)   dprobeb   <chr [1]>
      10 realized   over_constrained_equation e_dprobe2(0) e_dprobe2 <chr [1]>
      11 realized   over_constrained_equation e_dprobe2(1) e_dprobe2 <chr [1]>
      12 realized   over_constrained_equation e_dprobe2(2) e_dprobe2 <chr [1]>
    Message
      -- Heaviest statement-variable incidences --------------------------------------
    Output
      # A tibble: 10 x 4
         eq        var   weight  rows
         <chr>     <chr>  <int> <int>
       1 e_qfa     afa      450   225
       2 e_qfe     afe      360   180
       3 e_ev_alt  qes      360     9
       4 e_qxs     ams      270   135
       5 e_vgdp    qxs      270     9
       6 e_qgdp    qxs      270     9
       7 e_ev_alt  qxs      270     9
       8 e_ev_alt  pfob     270     9
       9 e_cnttotr pfob     270     9
      10 e_qo      pfa      225    45

# incidence plot errors on a probe without incidence data

    x This probe carries no statement incidence data (report version 2); rerun against a solver image with probe report version 2.

# dm plot errors on a structurally valid probe

    x No Dulmage-Mendelsohn localization to plot: the system has full structural rank on both patterns.
    x The "dm" plot only applies to structurally singular systems; see `plot(x, type = "incidence")` for the system structure.

# cores plot errors without fine data

    x This probe carries no fine-decomposition (core) data.
    x Rerun with `ems_probe(cmf_path, fine = TRUE)`.

# ems_probe errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# ems_probe errors when fine is not a logical scalar

    x `fine` must be a logical of length 1.

# probe prints each condensation verdict

    Code
      for (v in list(list(vecsize = 2e+05, nbacksolve = 68, nbselems = 2000,
        bordered = TRUE, ndblock = 35, netcut = 400, partition_set = "REG"), list(
        nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 35, netcut = 400,
        partition_set = "REG"), list(nbacksolve = 68, nbselems = 2000), list(
        nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 12, chain_set = "alltime",
        chain_source = "structural"), list(vecsize = 1350000))) {
        .probe_print_cndns(do.call(probe_stats_variant, v))
      }
    Message
      -- Condensation ----------------------------------------------------------------
      * 68 backsolved variables remove 1% of the uncondensed system (202,000 to
        200,000 equations)
      ! "REG" partitions the system into 35 blocks (400 border variables), which the
        bordered methods exploit and substitution densifies
      i Redeploy without `backsolve` and solve with a bordered method: measured on
        GTAP-RE, condensation made "SBBD" runs 69% to 393% slower, and condensed
        "DBBD" stops gaining from extra tasks
      -- Condensation ----------------------------------------------------------------
      * 68 backsolved variables remove 16% of the uncondensed system (12,524 to
        10,524 equations)
      v "REG" partitions the condensed system into 35 blocks, but below 120,000
        condensed equations threaded "LU" beats "DBBD"
      -- Condensation ----------------------------------------------------------------
      * 68 backsolved variables remove 16% of the uncondensed system (12,524 to
        10,524 equations)
      v No block partition, so the system is "LU"-bound: the case condensation pays
        for
      -- Condensation ----------------------------------------------------------------
      * 68 backsolved variables remove 16% of the uncondensed system (12,524 to
        10,524 equations)
      ! "alltime" partitions the system into 12 blocks (0 border variables), which
        the bordered methods exploit and substitution densifies
      i Redeploy without `backsolve` and solve with a bordered method: measured on
        GTAP-RE, condensation made "SBBD" runs 69% to 393% slower, and condensed
        "DBBD" stops gaining from extra tasks
      -- Condensation ----------------------------------------------------------------
      i No condensation and no block partition: a "LU"-bound system this large is a
        candidate for `backsolve` in `ems_model()`

# the probe prints its recommendation

    Code
      print(probe)
    Message
      -- Structural probe ------------------------------------- 1,500,000 equations --
      v Closure valid: every variable is determined and no equation is redundant, at
        base data too
      > Recommended for the machine given (8 cores, 12.0 GB): "DBBD", 2 tasks × 4
        threads
        `ems_solve(cmf_path, matrix_method = "DBBD", n_tasks = 2, n_threads = 4)`
      i `summary(x)` for the full report

---

    Code
      summary(probe)
    Message
      -- Structural probe: full report -----------------------------------------------
      * System: 1,500,000 equations
      v Structural rank: 1,500,000 of 1,500,000
      v Rank at base data: 1,500,000 of 1,500,000
      * Structure: no time chain; block partition by "reg" (3 blocks, 74 border
        variables)
      * Simultaneous cores: 28 of 6,049 strongly connected components have more than
        one element; the largest has 4,341 rows
      * Largest core by equation: e_qfa ×225, e_qfd ×225, e_qfm ×225, e_pfa ×225,
        e_pfd ×225, e_pfm ×225, and 44 more
      * Statements: 247 equation statements, 856 statement-variable incidences
      -- Heaviest statement-variable incidences --------------------------------------
    Output
      # A tibble: 10 x 4
         eq        var   weight  rows
         <chr>     <chr>  <int> <int>
       1 e_qfa     afa      450   225
       2 e_qfe     afe      360   180
       3 e_ev_alt  qes      360     9
       4 e_qxs     ams      270   135
       5 e_vgdp    qxs      270     9
       6 e_qgdp    qxs      270     9
       7 e_ev_alt  qxs      270     9
       8 e_ev_alt  pfob     270     9
       9 e_cnttotr pfob     270     9
      10 e_qo      pfa      225    45
    Message
      -- Recommendation --------------------------------------------------------------
      v "DBBD", 2 tasks × 4 threads for the machine given (8 cores, 12.0 GB)
      * Evidence: 1,500,000 equations, no time chain, partition reg (3 blocks, border
        6.4% of the system), assessed at 2 tasks
      * Rationale: two ranks on a laptop, more only where cores and memory allow;
        threads take the rest
      * Memory: about 2.4 GB at 2 tasks, 20% of 12.0 GB (fits)
      * Refinement: on, one step per "DBBD" solve; the estimate includes it
      > `ems_solve(cmf_path, matrix_method = "DBBD", n_tasks = 2, n_threads = 4)`

---

    Code
      print(probe)
    Message
      -- Structural probe ------------------------------------- 1,500,000 equations --
      v Closure valid: every variable is determined and no equation is redundant, at
        base data too
      x No solve recommendation: the container's cores could not be read; pass
        `cores` and `memory` to `ems_probe()`
      i `summary(x)` for the full report

---

    Code
      print(probe)
    Message
      -- Structural probe ------------------------------------- 1,500,000 equations --
      v Closure valid: every variable is determined and no equation is redundant, at
        base data too
      > Recommended for 8 cores as given, memory unknown: "DBBD", 2 tasks × 4 threads
        `ems_solve(cmf_path, matrix_method = "DBBD", n_tasks = 2, n_threads = 4)`
      i `summary(x)` for the full report

---

    Code
      print(probe)
    Message
      -- Structural probe ------------------------------------ 12,500,000 equations --
      v Closure valid: every variable is determined and no equation is redundant, at
        base data too
      ! Memory is tight: about 11.2 GB estimated of 12.0 GB available
      > Recommended for the machine given (8 cores, 12.0 GB): "LU", 1 task × 1 thread
        `ems_solve(cmf_path, matrix_method = "LU")`
      i `summary(x)` for the full report

---

    Code
      summary(probe)
    Message
      -- Structural probe: full report -----------------------------------------------
      * System: 12,500,000 equations
      v Structural rank: 12,500,000 of 12,500,000
      v Rank at base data: 12,500,000 of 12,500,000
      * Structure: no time chain; block partition by "reg" (3 blocks, 74 border
        variables)
      * Simultaneous cores: 28 of 6,049 strongly connected components have more than
        one element; the largest has 4,341 rows
      * Largest core by equation: e_qfa ×225, e_qfd ×225, e_qfm ×225, e_pfa ×225,
        e_pfd ×225, e_pfm ×225, and 44 more
      * Statements: 247 equation statements, 856 statement-variable incidences
      -- Heaviest statement-variable incidences --------------------------------------
    Output
      # A tibble: 10 x 4
         eq        var   weight  rows
         <chr>     <chr>  <int> <int>
       1 e_qfa     afa      450   225
       2 e_qfe     afe      360   180
       3 e_ev_alt  qes      360     9
       4 e_qxs     ams      270   135
       5 e_vgdp    qxs      270     9
       6 e_qgdp    qxs      270     9
       7 e_ev_alt  qxs      270     9
       8 e_ev_alt  pfob     270     9
       9 e_cnttotr pfob     270     9
      10 e_qo      pfa      225    45
    Message
      -- Recommendation --------------------------------------------------------------
      v "LU", 1 task × 1 thread for the machine given (8 cores, 12.0 GB)
      * Evidence: 12,500,000 equations, no time chain, partition reg (3 blocks,
        border 6.4% of the system), assessed at 2 tasks
      * Rationale: one task and one thread: threads do not help an uncondensed "LU"
        factorization
      ! "DBBD" would be faster but its 19.9 GB estimate does not fit; "LU" stays
      ! Memory: about 11.2 GB at 1 task, 94% of 12.0 GB (tight)
      > `ems_solve(cmf_path, matrix_method = "LU")`

---

    Code
      print(probe)
    Message
      -- Structural probe ------------------------------------ 40,000,000 equations --
      v Closure valid: every variable is determined and no equation is redundant, at
        base data too
      x No solve recommendation: nothing fits in 12.0 GB; "LU" needs about 36.0 GB.
        More memory for the container, a coarser aggregation or condensation
      i `summary(x)` for the full report

---

    Code
      summary(probe)
    Message
      -- Structural probe: full report -----------------------------------------------
      * System: 40,000,000 equations
      v Structural rank: 40,000,000 of 40,000,000
      v Rank at base data: 40,000,000 of 40,000,000
      * Structure: no time chain; block partition by "reg" (3 blocks, 74 border
        variables)
      * Simultaneous cores: 28 of 6,049 strongly connected components have more than
        one element; the largest has 4,341 rows
      * Largest core by equation: e_qfa ×225, e_qfd ×225, e_qfm ×225, e_pfa ×225,
        e_pfd ×225, e_pfm ×225, and 44 more
      * Statements: 247 equation statements, 856 statement-variable incidences
      -- Heaviest statement-variable incidences --------------------------------------
    Output
      # A tibble: 10 x 4
         eq        var   weight  rows
         <chr>     <chr>  <int> <int>
       1 e_qfa     afa      450   225
       2 e_qfe     afe      360   180
       3 e_ev_alt  qes      360     9
       4 e_qxs     ams      270   135
       5 e_vgdp    qxs      270     9
       6 e_qgdp    qxs      270     9
       7 e_ev_alt  qxs      270     9
       8 e_ev_alt  pfob     270     9
       9 e_cnttotr pfob     270     9
      10 e_qo      pfa      225    45
    Message
      -- Recommendation --------------------------------------------------------------
      x No solve recommendation: nothing fits in 12.0 GB; "LU" needs about 36.0 GB.
        More memory for the container, a coarser aggregation or condensation

---

    Code
      print(singular)
    Message
      -- Structural probe ---------------------------------------- 10,530 equations --
      x Structurally singular: rank 10,527 of 10,530 (3 unmatched equations, 3
        unmatched variables)
        Undetermined: dprobeb ×3
        Over-constrained: e_dprobe2 ×3
      i Locate it with `plot(x, type = "dm")`
      x No solve recommendation: the closure is structurally singular; fix it first
      i `summary(x)` for the full report

# ems_probe validates the cores and memory overrides

    x `cores` must be a positive number of length 1 or "NULL".

---

    x `memory` must be a positive number of length 1 or "NULL".

# ems_probe announces a structurally singular system

    Code
      res <- ems_probe(cmf_path, cores = 4, memory = 8)
    Message
      i Probing 'model.cmf'; solver log at 'out/probe/solver_out_probe.txt'
      i Probe finished in 0.0 s
      i The system is structurally singular; see `print()` and `plot(x, type = "dm")` for the diagnosis.

