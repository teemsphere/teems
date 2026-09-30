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
      flags ({.arg fastrefac}, {.arg gpzerodivide}, {.arg cntl_3},
      {.arg cntl_6}, {.arg nsbbdblocks}, {.arg withmc66},
      {.arg smllthreads}, {.arg tempdir}, {.arg nowrites})."
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
    # print()/summary() output for a teems_probe object. cli reflows
    # these, so the line breaks here are layout only
    # test-ems_probe.R: "probe print methods run"
    print = list(
      rule = "teems structural probe",
      system = "condensed system: {x$vecsize} x {x$vecsize}",
      pattern_structural = "structural pattern",
      pattern_realized = "realized pattern (nonzero at base data)",
      rank_full = "{lbl}: full structural rank {p$rank} of {p$n}",
      rank_singular = "{lbl}: structurally singular — rank {p$rank} of {p$n}
      ({p$unmatched_rows} unmatched equation{?s},
      {p$unmatched_cols} unmatched variable{?s})",
      under_by_var = "  under-determined by variable: {.val {agg}}",
      over_by_eq = "  over-constrained by equation: {.val {agg}}",
      dm_blocks = "  DM blocks: under {p$dm$m1} x {p$dm$n1},
      well {p$dm$m2} x {p$dm$n2}, over {p$dm$m3} x {p$dm$n3}",
      fine_dm = "fine DM: {x$cores$sq_comps} strongly connected component{?s} —
      {x$cores$cores_gt1} simultaneous core{?s} (>1 element),
      largest {x$cores$largest}",
      largest_core = "  largest core by equation: {.val {preview}}",
      statements = "{NROW(x$statements)} equation statement{?s},
      {NROW(x$incidence)} statement-variable incidence{?s}",
      ordering = "ordering evidence: chain {x$structure$chain_source %|||% 'none'},
      partition {x$structure$partition_source %|||% 'none'}",
      defects = "defect elements (capped upstream at 200 per list):",
      incidences = "heaviest statement-variable incidences:"
    ),
    # the condensation verdict lines
    # test-ems_probe.R: "probe prints each condensation verdict"
    cndns = list(
      hurts = "condensation: {n_backsolve} backsolved variable{?s} ({share} of
      the uncondensed system), but the probe finds a {blocks}-block
      partition on {.val {set}} (border {border})",
      hurts_advice = "  substitution densifies those blocks -- redeploy without
      {.arg backsolve} and solve with a bordered method",
      helps = "condensation: {n_backsolve} backsolved variable{?s} ({share} of
      the uncondensed system); no usable block partition, so this
      system is {.val LU}-bound -- the case condensation pays for",
      candidate = "condensation: none, and no usable block partition -- this
      {.val LU}-bound system is a candidate for
      {.fn ems_model} {.arg backsolve}"
    ),
    # the recommendation block
    # test-ems_probe.R: "the probe prints its recommendation"
    recommend = list(
      no_host = "recommended (container not inspected, so one task and one thread): {.val {r$matrix_method}}",
      host = "recommended for {cores} core{?s}, {mem} ({where}): {.val {r$matrix_method}}, {n_tasks} task{?s} x {n_threads} thread{?s}",
      where_given = "as given",
      where_container = "this container",
      evidence = "  evidence: {r$evidence}",
      rationale = "  {r$rationale}",
      johansen = "  under Johansen the crossover differs: {.val {r$method_johansen}}",
      memory_arm = "  {.val SBBD} does not fit the memory ({sbbd_gb} estimated); {.val NDBBD} holds the tables once",
      dbbd_blocked = "  {.val DBBD} would be faster but its {dbbd_gb} estimate does not fit; {.val LU} stays",
      lu_excluded = "  {.val LU} excluded: the projected MA48 workspace passes the 32-bit ceiling",
      memory = "  memory: about {est_gb} at {fit$n_tasks} task{?s}{share_txt} -- {fit$verdict}",
      refine_on = "  one refinement step per {.val DBBD} solve: on (about {refine_gb} with it)",
      refine_off = "  one refinement step per {.val DBBD} solve: off, set by the refine option",
      scratch = "  scratch inside the container ({.path {r$tempdir}}); ems_solve() sets it for {.val NDBBD}",
      call = "  {.code {r$call}}",
      unknown_gb = "unknown"
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
