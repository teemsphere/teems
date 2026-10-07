build_probe_err <- function() {
  list(
    # test-ems_probe.R: "ems_probe errors when fine is not a logical scalar"
    x_logical = "{.arg {arg}} must be a logical of length 1.",
    # test-ems_probe.R: "ems_probe validates the cores and memory overrides"
    x_positive = "{.arg {arg}} must be a positive number of length 1 or {.val NULL}.",
    # test-solver_dots.R: "ems_probe rejects unknown dot arguments"
    probe_dots = c(
      "Unknown argument{?s} {.arg {unknown_args}} passed to {.arg ...}.",
      "{.arg ...} accepts the MA48 workspace initial guesses
      ({.arg laA}, {.arg laD}, {.arg laDi}) and the expert solver
      flags ({.arg postsim}, {.arg inmemory}, {.arg fastrefac},
      {.arg nsbbdblocks}, {.arg withmc66}, {.arg smllthreads},
      {.arg tempdir}, {.arg nowrites}, {.arg condest}, {.arg jacdump},
      {.arg ma48_cntl2}, {.arg ma48_cntl4}, and the Runge-Kutta run controls, which a probe
      ignores)."
    ),
    # test-ems_probe.R: "probe report errors when the report is absent"
    no_report = c(
      "No probe report was produced at {.path {probe_path}}.",
      "The structural probe requires a solver image with MC79 support.
      Rebuild the teems image (see the teems-solver README) or inspect
      the solver log at {.path {diag_out}}."
    ),
    # test-ems_probe.R: "cores plot errors without fine data"
    no_fine = c(
      "This probe carries no fine-decomposition (core) data.",
      "Rerun with {.code ems_probe(cmf_path, fine = TRUE)}."
    ),
    # test-ems_probe.R: "dm plot errors on a structurally valid probe"
    no_defects_dm = c(
      "No Dulmage-Mendelsohn localization to plot: the system has full
      structural rank on both patterns.",
      "The {.val dm} plot only applies to structurally singular systems;
      see {.code plot(x, type = \"incidence\")} for the system structure."
    ),
    # test-ems_probe.R: "incidence plot errors on a probe without incidence data"
    no_incidence = "This probe carries no statement incidence data (report
    version {x$version %|||% 1}); rerun against a solver image with
    probe report version 2."
  )
}

build_probe_info <- function() {
  list(
    # test-ems_probe.R: "ems_probe announces a structurally singular system"
    probe_defective = "The system is structurally singular; see
    {.code print()} and {.code plot(x, type = \"dm\")} for the diagnosis.",
    # console lines around the solver run, under the verbose option
    # test-ems_probe.R: "ems_probe announces a structurally singular system"
    run = list(
      start = "Probing {.file {cmf_file}}; solver log at {.path {log_rel}}",
      done = "Probe finished in {elapsed_txt}",
      warnings = "The solver logged {n_warn} warning{?s} (see the log):",
      warn_more = "... and %d more"
    ),
    # the brief print() of a teems_probe object
    # test-ems_probe.R: "probe print methods run"
    brief = list(
      rule = "Structural probe",
      size = "{n_fmt} equations",
      valid = "Closure valid: every variable is determined and no equation
      is redundant, at base data too",
      singular = "Structurally singular: rank {rank_txt} of {n_txt}
      ({p$unmatched_rows} unmatched equation{?s}, {p$unmatched_cols}
      unmatched variable{?s})",
      singular_base = "Singular at base data: rank {rank_txt} of {n_txt};
      zero flows leave {p$unmatched_cols} element{?s} undetermined",
      under = "Undetermined: {agg}",
      over = "Over-constrained: {agg}",
      dm_hint = "Locate it with {.code plot(x, type = \"dm\")}",
      candidate = "Large {.val LU}-bound system: {.arg backsolve} in
      {.fn ems_model} would shrink it",
      hurts = "Backsolving hurts here: {.val {set}} splits the system into
      {blocks} blocks, which the bordered methods exploit and substitution
      densifies; redeploy without {.arg backsolve}",
      more = "{.code summary(x)} for the full report"
    ),
    # the full report's structure section
    # test-ems_probe.R: "probe print methods run"
    print = list(
      rule = "Structural probe: full report",
      system = "System: {n_fmt} equations",
      system_condensed = "System: {n_fmt} equations after condensation
      ({n_backsolve} backsolved variable{?s}; {uncondensed_fmt} uncondensed)",
      pattern_structural = "Structural rank",
      pattern_realized = "Rank at base data",
      rank_full = "{lbl}: {rank_txt} of {n_txt}",
      rank_singular = "{lbl}: {rank_txt} of {n_txt}, structurally singular
      ({p$unmatched_rows} unmatched equation{?s},
      {p$unmatched_cols} unmatched variable{?s})",
      under_by_var = "Under-determined by variable: {agg}",
      over_by_eq = "Over-constrained by equation: {agg}",
      dm_blocks = "Dulmage-Mendelsohn blocks: under {dm$m1} × {dm$n1},
      well {dm$m2} × {dm$n2}, over {dm$m3} × {dm$n3}",
      structure = "Structure: {chain_txt}; {partition_txt}",
      chain = "time chain over {.val {chain_set}} ({n_time} period{?s},
      {border} border {cli::qty(border_n)}variable{?s})",
      no_chain = "no time chain",
      partition = "block partition by {.val {partition_set}} ({n_blocks}
      block{?s}{border_txt})",
      partition_border = ", {border} border {cli::qty(border_n)}variable{?s}",
      no_partition = "no block partition",
      fine_dm = "Simultaneous cores: {gt1_txt} of {sq_txt} strongly
      connected {cli::qty(x$cores$sq_comps)}component{?s} {?has/have} more
      than one element; the largest has {largest_txt} rows",
      largest_core = "Largest core by equation: {preview}",
      core_more = "%d more",
      statements = "Statements: {n_stmt} equation statement{?s},
      {n_inc} statement-variable {cli::qty(NROW(x$incidence))}incidence{?s}",
      files = "Files: report {.path {x$paths$report}}, solver log
      {.path {x$paths$log}}",
      defects = "Defect elements (capped upstream at 200 per list)",
      incidences = "Heaviest statement-variable incidences"
    ),
    # the condensation section of the full report
    # test-ems_probe.R: "probe prints each condensation verdict"
    cndns = list(
      rule = "Condensation",
      status = "{n_backsolve} backsolved variable{?s} remove {share} of the
      uncondensed system ({uncondensed_fmt} to {n_fmt} equations)",
      helps = "No block partition, so the system is {.val LU}-bound: the
      case condensation pays for",
      lu_fine = "{.val {set}} partitions the condensed system into {blocks}
      blocks, but below {lu_size} condensed equations threaded {.val LU}
      beats {.val DBBD}",
      hurts = "{.val {set}} partitions the system into {blocks} blocks
      ({border} border variable{?s}), which the bordered methods exploit
      and substitution densifies",
      hurts_advice = "Redeploy without {.arg backsolve} and solve with a
      bordered method: measured on GTAP-RE, condensation made {.val SBBD}
      runs 69% to 393% slower, and condensed {.val DBBD} stops gaining
      from extra tasks",
      candidate = "No condensation and no block partition: a {.val LU}-bound
      system this large is a candidate for {.arg backsolve} in
      {.fn ems_model}"
    ),
    # the recommendation, brief (print) and full (summary)
    # test-ems_probe.R: "the probe prints its recommendation"
    recommend = list(
      rule = "Recommendation",
      brief = "Recommended for {where_txt}: {.val {r$matrix_method}},
      {n_tasks} task{?s} × {n_threads} thread{?s}",
      host = "{.val {r$matrix_method}}, {n_tasks} task{?s} ×
      {n_threads} thread{?s} for {where_txt}",
      none = "No solve recommendation: {why}",
      why_singular = "the closure is structurally singular; fix it first",
      why_no_host = "the container's cores could not be read; pass
      {.arg cores} and {.arg memory} to {.fn ems_probe}",
      why_wont_fit = "nothing fits in {limit_gb}; {.val {r$matrix_method}}
      needs about {est_gb}. More memory for the container, a coarser
      aggregation or condensation",
      where_container = "this container ({cores} core{?s}, {mem})",
      where_given = "the machine given ({cores} core{?s}, {mem})",
      where_cores_given = "{cores} core{?s} as given, {mem} from this
      container",
      where_cores_given_nomem = "{cores} core{?s} as given, memory unknown",
      where_memory_given = "{cores} core{?s} from this container, {mem} as
      given",
      unknown_mem = "memory unknown",
      memory_tight = "Memory is tight: about {est_gb} estimated of
      {limit_gb} available",
      evidence = "Evidence: {r$evidence}",
      rationale = "Rationale: {r$rationale}",
      small = "below the smallest measured size (%s equations) every method
      solves quickly, so the simplest is recommended",
      lu_plain = "one task and one thread: threads do not help an
      uncondensed {.val LU} factorization",
      johansen = "With {.code solution_method = \"Johansen\"} the method
      would be {.val {r$method_johansen}} instead",
      memory_arm = "{.val SBBD} does not fit the memory ({sbbd_gb}
      estimated); {.val NDBBD} holds the tables once",
      dbbd_blocked = "{.val DBBD} would be faster but its {dbbd_gb} estimate
      does not fit; {.val LU} stays",
      dbbd_blocked_plain = "Without condensation {.val DBBD} would need
      about {plain_gb} and fit; on the one rig measured at this size the
      uncondensed deployment solved faster",
      lu_excluded = "{.val LU} excluded: the projected MA48 workspace passes
      the 32-bit ceiling",
      memory = "Memory: {est_gb} at {fit$n_tasks} task{?s}{share_txt}
      ({fit$verdict})",
      memory_unknown = "Memory: {est_gb} estimated at {fit$n_tasks}
      task{?s}; not checked, the memory is unknown",
      refine_on = "Refinement: on, one step per {.val DBBD} solve; the
      estimate includes it",
      refine_off = "Refinement: off by the {.field refine} option; on, the
      estimate would be {refine_gb}",
      scratch = "Scratch: {.val NDBBD} writes its block factors to
      {.path {r$tempdir}} inside the container ({.fn ems_solve} sets this)",
      call = "{.code {r$call}}",
      unknown_gb = "unknown",
      small_gb = "under 0.01 GB",
      about = "about %s",
      mem_share = ", %d%% of %s",
      mem_share_small = ", under 1%% of %s"
    ),
    # the evidence line of the recommendation; sprintf templates
    # test-auto_method.R: "evidence and record lines render every input"
    evidence = list(
      size = "%s equations",
      size_unknown = "unknown size",
      condensed = "%s (condensed)",
      skipped = "%s, assessed at %s; structural probe skipped (%s)",
      skip_default = "not a candidate",
      chain = "time chain %s (%d periods)",
      no_chain = "no time chain",
      no_partition = "no partition viable for %s",
      partition = "partition %s (%d blocks, border %s of the system)",
      border_na = "n/a",
      lu_excluded = ", LU excluded (projected MA48 workspace %s > 32-bit ceiling %s)",
      lu_share = ", LU workspace %s of the 32-bit ceiling",
      task = "%d task",
      tasks = "%d tasks",
      full = "%s, %s, %s, assessed at %s%s"
    ),
    # base-graphics labels; sprintf templates, not cli ones
    # test-ems_probe.R: "probe plots render on a null device"
    plot = list(
      cores_xlab = "simultaneous cores",
      cores_ylab = "core size (rows)",
      cores_main = "%d cores > 1 element; %d recursive rows",
      cores_none = "no simultaneous cores: fully recursive system",
      core_xlab = "rows in the largest core",
      core_main = "largest core: %d rows",
      core_other = "(+%d eqs)",
      core_none = "no core composition recorded",
      dm_main = "Dulmage-Mendelsohn localization (%s pattern): rank %d of %d",
      dm_structural = "structural",
      dm_realized = "realized",
      incidence_main = "equation-system structure: %d statements x %d variables (%d incidences)"
    )
  )
}
