build_solve_err <- function() {
  list(
    # test-ems_homogeneity.R: "a deployment without VPQ types cannot be checked"
    homog_no_types = c(
      "This deployment carries no VPQ types.",
      "Deploy the model with this version of teems; {.fun teems::ems_homogeneity}
      reads the types {.fun teems::ems_model} found in the model file."
    ),
    # test-ems_homogeneity.R: "a deployment without VPQ types cannot be checked"
    homog_untyped = c(
      "No variable in the model file has a VPQ type, so there is nothing to check.",
      "Declare types with {.code VPQType=} qualifiers, {.code (begins <prefix>
      default VPQType <type>)} rules or {.code (Name <variable> VPQType <type>)}
      statements (GEMPACK manual 57.2)."
    ),
    # not in tests: the solver run succeeded but listed no Jacobian export
    homog_no_jacobian = "The solver run for the homogeneity check left no
    Jacobian export ({.file sol.jac}); see its log in the {.file _homogeneity}
    copy of the deployment.",
    # .solve_lock_acquire()
    # test-ems_solve.R: "a second solve in one deploy directory is refused by name"
    lock_held = c(
      "A solve is already running in this deploy directory: {lock_owner}.",
      "Concurrent runs need a deploy directory each: call
       {.code ems_option_set(tempdir = ...)} before {.fn ems_deploy} for
       every run. Two solves in one directory share the solver's scratch
       files and the outputs the CMF names, and would corrupt each other.
       If a previous run was interrupted, remove {.path {lock_path}}."
    ),
    # see solve_err$lock_held
    # test-solve_lock_acquire.R: "a lock without an owner record is attributed to an earlier run"
    lock_earlier_run = "an earlier run",
    # .get_solver_paths()
    # test-solver_switches.R: "a cmf_path that does not exist aborts"
    no_cmf = "The {.arg cmf_path} provided {.path {cmf_path}} does not
    exist.",
    # {requirement} for comp_arg_type / the solver scalar validators
    # see solve_err$comp_arg_type
    # test-ems_complementarity.R: "constructor validation aborts"
    requirement = list(
      positive_int = "a positive integer-like numeric of length 1",
      level_ratio = "a numeric of length 1 greater than 1 (a level ratio)",
      logical_flag = "a non-missing logical of length 1",
      numeric_scalar = "a numeric of length 1",
      character_scalar = "a character of length 1",
      open_unit = "a numeric of length 1 in (0, 1)",
      half_open_unit = "a numeric of length 1 in (0, 1]",
      fatal_or_warn = "either \"fatal\" or \"warn\"",
      one_of = "one of %s"
    ),
    # test-auto_method.R: "the memory fit check refuses a run past the error band"
    wont_fit = c(
      "Estimated peak memory {est_gb} GB for {.val {method}} at {n_tasks} task{?s} exceeds the container's {mem_gb} GB.",
      "The estimate is {kb_per_eq} kB per equation over {plain_size} plain-equivalent equations (measured 2026-09 on whole containers; conservative by up to a quarter), so the run would be killed by the memory limit, not fail cleanly.",
      "Remedies: a coarser aggregation, condensation ({.arg backsolve} in {.fun teems::ems_model}), a higher Docker Desktop memory limit, fewer tasks under {.val DBBD}, {.val NDBBD} at one task for an intertemporal model, or a larger host."
    ),
    # test-solve_in_situ.R: "solve_in_situ errors when no input files are given"
    # test-ems_calculate.R: "ems_calculate requires its input files"
    no_insitu_inputs = "No input files loaded; all files must be passed as named arguments via {.arg ...}.",
    # test-solve_in_situ.R: "solve_in_situ errors when missing input file"
    # test-ems_calculate.R: "ems_calculate requires its input files"
    missing_insitu_inputs = "Required files {.val {req_inputs}} not all provided; missing: {.val {missing_files}}.",
    # test-solve_in_situ.R: "solve_in_situ errors when an input file does not exist"
    # test-ems_calculate.R: "ems_calculate requires its input files"
    insitu_no_file = "Input file{?s} not found: {.val {nonexist_files}}.",
    # test-ems_solve.R: "ems_solve errors when n_tasks is not integerish"
    x_integerish = "{.arg {arg}} must be integer-like.",
    # test-ems_complementarity.R: "constructor validation aborts"
    # test-solver_switches.R: "numeric-knob validation aborts", "mode-switch validation aborts"
    comp_arg_type = "{.arg {bad_arg}} must be {requirement}.",
    # test-ems_complementarity.R: "both runs disabled aborts"
    comp_runs_off = c(
      "{.arg do_approx_run} and {.arg do_acc_run} cannot both be {.val FALSE}.",
      "Skipping the approximate run takes the pre-simulation states as
      the accurate run's targets; skipping the accurate run keeps the
      approximate solution as the result (GEMPACK manual 51.6).
      Skipping both leaves nothing to solve."
    ),
    # test-solver_switches.R: "mode-switch validation aborts"
    # test-ems_complementarity.R: "ems_solve rejects a non-spec complementarity"
    comp_spec_class = c(
      "{.arg complementarity} must be built by {.fun ems_complementarity}.",
      "Example: {.code complementarity = ems_complementarity(steps_approx_run = 20L)}."
    ),
    # test-solver_switches.R: "numeric-knob validation aborts"
    invalid_length = "{.arg {arg}} must be an integer-like numeric of length 1.",
    # test-ems_solve.R: "ems_solve errors when Gragg steps mix parity", "the midpoint method solves and extrapolates"
    step_parity = c(
      "{.arg steps} must be all even or all odd when {.arg solution_method} is {.val {solution_method}}.",
      "Richardson extrapolation of Gragg and midpoint solutions needs step counts of one parity (e.g. 2, 4, 6 or 3, 5, 7); even counts are recommended."
    ),
    # test-ems_solve.R: "ems_solve errors when steps is not one to three whole numbers"
    step_length = c(
      "{.arg steps} must be one, two or three positive whole numbers.",
      "One count (e.g. {.code steps = 8L}) is a single multi-step run without extrapolation; two or three increasing counts (e.g. {.code c(2L, 4L, 8L)}) are extrapolated, and three also give an accuracy estimate."
    ),
    # test-ems_solve.R: "ems_solve errors on invalid Runge-Kutta arguments"
    step_single_rk = c(
      "{.arg steps} must be a single positive integer when {.arg solution_method} is {.val {solution_method}}.",
      "Runge-Kutta methods take one step count (e.g. {.code steps = 8L}); they use no Richardson extrapolation, so no step-count triple is involved."
    ),
    # test-ems_solve.R: "ems_solve errors on invalid Runge-Kutta arguments"
    adaptive_method = c(
      "{.arg adaptive} {.val {adaptive}} requires an embedded Runge-Kutta {.arg solution_method} ({.val BoSha32} or {.val DoPri54}).",
      "Only the embedded pairs provide the per-step error estimate the adaptive controller acts on."
    ),
    # test-ems_solve.R: "ems_solve errors on invalid Runge-Kutta arguments"
    rk_subintervals = c(
      "{.arg n_subintervals} must be 1 when {.arg solution_method} is {.val {solution_method}}.",
      "Subintervals restart the integrator and only benefit the extrapolating methods; increase {.arg steps} (or use {.arg adaptive}) instead."
    ),
    # test-ems_solve.R: "ems_solve errors on invalid Runge-Kutta arguments"
    epstol_range = "{.arg eps_tolerance} must be a positive numeric of length 1.",
    # test-ems_RK.R: "unknown dot arguments abort"
    solver_dots = c(
      "Unknown argument{?s} {.arg {unknown_args}} passed to {.arg ...}.",
      "{.arg ...} accepts the Runge-Kutta step controls
      ({.arg adaptive}, {.arg eps_tolerance}, {.arg max_retries},
      {.arg retry_adjust}, {.arg rk_chart}, {.arg rk_norm},
      {.arg rk_controller}, {.arg rk_scope}, {.arg rk_h0},
      {.arg rk_guard}; see
      {.fun ems_RK}), the MA48 workspace initial guesses
      ({.arg laA}, {.arg laD}, {.arg laDi}) and the expert solver
      flags ({.arg postsim}, {.arg inmemory}, {.arg fastrefac},
      {.arg gpzerodivide}, {.arg cntl_3}, {.arg cntl_6},
      {.arg nsbbdblocks}, {.arg withmc66}, {.arg smllthreads},
      {.arg tempdir}, {.arg nowrites}, {.arg condest}, {.arg jacdump},
      {.arg ma48u})."
    ),
    # test-solver_dots.R: "solve_in_situ solver_args must be a fully named allowlisted list"
    solver_args_list = c(
      "{.arg solver_args} must be a fully named list.",
      "It carries the named solver arguments {.fun ems_solve} accepts
      through {.arg ...}; the in-situ {.arg ...} is reserved for the
      input files."
    ),
    # test-solver_dots.R: "solve_in_situ solver_args must be a fully named allowlisted list"
    solver_args_unknown = c(
      "Unknown argument{?s} {.arg {unknown_args}} in {.arg solver_args}.",
      "Accepted: the MA48 workspace initial guesses ({.arg laA},
      {.arg laD}, {.arg laDi}), the expert solver flags
      ({.arg postsim}, {.arg inmemory}, {.arg fastrefac},
      {.arg gpzerodivide}, {.arg cntl_3}, {.arg cntl_6},
      {.arg nsbbdblocks}, {.arg withmc66}, {.arg smllthreads},
      {.arg tempdir}, {.arg nowrites}, {.arg condest}, {.arg jacdump},
      {.arg ma48u}) and the Runge-Kutta run controls
      ({.arg rk_chart}, {.arg rk_norm}, {.arg rk_controller},
      {.arg rk_scope}, {.arg rk_h0}, {.arg rk_guard}). The
      Runge-Kutta step controls are formal arguments of
      {.fun solve_in_situ}."
    ),
    # test-ems_solve.R: "ems_solve errors when steps are not increasing"
    step_increasing = c(
      "{.arg steps} must be strictly increasing for {.arg solution_method} {.val {solution_method}}.",
      "Richardson extrapolation combines solutions computed with distinct, increasing step counts."
    ),
    # test-ems_solve.R: "ems_solve errors when SBBD used with static model"
    invalid_method = "{.arg matrix_method} {.val {matrix_method}} only applicable to intertemporal model runs.",
    # test-chk_solver_log.R: "unmapped Error lines fall back to the generic abort"
    solution_err = "Errors detected during solution. See {.path {paths$diag_out}}.",
    # test-chk_solver_log.R: "singularity without Error lines routes to the probe hint"
    solution_sing = c(
      "Singularity detected during solution. See {.path {paths$diag_out}}.",
      "A square-but-singular system usually indicates a structurally deficient closure partition.",
      "Run {.fun teems::ems_probe} on the deployed model for a named structural diagnosis."
    ),
    # test-chk_solver_log.R: "singularity after updated range violations routes to the shock-size hint"
    solution_sing_range = c(
      "Singularity detected during solution. See {.path {paths$diag_out}}.",
      "{n_viol} updated-value range warning{?s} preceded it (first: coefficient {first_viol}): the shock is too large for the step schedule, so data flows crossed their declared bounds and the system degenerated.",
      "Raise {.arg n_subintervals}, switch to {.code solution_method = \"DoPri54\"} (adaptive, keeps levels positive), or use {.code solution_method = \"Euler\"} for severe shocks.",
      "If the closure is in doubt, run {.fun teems::ems_probe} on the deployed model for a named structural diagnosis."
    ),
    # test-chk_solver_log.R: "condest near-singularity warns without aborting the run"
    # (the run completed; the verdict is the modeller's to act on)
    condest_nearsing = c(
      "The {.code condest} diagnostic reports the linear system as numerically near-singular (kappa_w2 {kappa_w2}): solutions are unreliable.",
      "The structural probe may pass -- look for near-zero data flows carried by the closure, or re-solve with {.code precision = \"double\"}. See {.path {paths$diag_out}}."
    ),
    # test-chk_solver_log.R: "TAB errors map to the model-specification abort"
    # lines 2-4 filled by .check_solver_log; line 3 dropped when no
    # manual section applies
    solver_tab = c(
      "The solver rejected the model specification with {n_err} error{?s}:",
      "{err_preview}",
      "See GEMPACK manual section{?s} {.val {manual_secs}}.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "closure errors map to the closure abort"
    solver_closure = c(
      "The solver rejected the closure or shock inputs with {n_err} error{?s}:",
      "{err_preview}",
      "Check the closure, swap and shock arguments supplied to {.fun teems::ems_model} and {.fun teems::ems_deploy}.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "data errors map to the data abort"
    solver_data = c(
      "The solver could not read the model data with {n_err} error{?s}:",
      "{err_preview}",
      "Check the data inputs supplied to {.fun teems::ems_data} and the headers named in the TAB file.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "runtime errors map to the numeric abort"
    solver_numeric = c(
      "The solver stopped on {n_err} runtime error{?s} while evaluating model values:",
      "{err_preview}",
      "See GEMPACK manual section{?s} {.val {manual_secs}}.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "workspace exhaustion maps to the resource abort"
    # a solver-side resource limit, not a fault in the model, closure or
    # data: the remedies are all about how the system is factorized
    solver_resource = c(
      "The solver ran out of factorization workspace with {n_err} error{?s}:",
      "{err_preview}",
      "Raise {.arg laA}, {.arg laD} or {.arg laDi} in {.fun teems::ems_solve}, or split the factorization with {.code matrix_method = \"SBBD\"} or {.code \"DBBD\"}.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "index ceiling maps to the size abort"
    # the system passed the solver's integer index width for one rank's
    # copy of the Jacobian: no workspace argument helps, only the layout
    # or the system size
    solver_size = c(
      "The system is too large for the solver's integer index width, with {n_err} error{?s}:",
      "{err_preview}",
      "Use the distributed {.code matrix_method = \"DBBD\"}, condense the model, or reduce its dimensions.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "an option or manifest statement the image does not know maps to the interface abort"
    # teems and the solver image are released together: an unknown
    # command-line option or manifest statement means the image is older
    # (or newer) than the package driving it
    solver_interface = c(
      "The solver image does not accept {n_err} input{?s} that teems sent:",
      "{err_preview}",
      "teems and its solver image are released together: use the image that matches this version of teems (the {.arg docker_tag} option of {.fun teems::ems_option_set} names the image in use).",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "an unwritable output or scratch file maps to the system abort"
    # the run environment, not the model: a directory the solver cannot
    # write, a full scratch filesystem, memory it could not get
    solver_system = c(
      "The solver could not get the disk or memory it needs, with {n_err} error{?s}:",
      "{err_preview}",
      "Check that the run directory and the scratch directory ({.arg tempdir}) are writable and have free space, and that Docker has the memory {.fun teems::ems_probe} estimates for this model.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "shock-group subtotal errors map to the subtotal abort"
    # the run asked for subtotals (or for more solves with each step's
    # factorization) that the groups or the chosen method cannot give
    solver_subtotal = c(
      "The solver rejected the subtotal (shock-group) request with {n_err} error{?s}:",
      "{err_preview}",
      "A subtotal names shocked exogenous components under a unique label, and needs {.code matrix_method} LU, SBBD or DBBD with the Johansen, Euler, midpoint or Gragg method (GEMPACK manual section 29).",
      "Full log: {.path {diag_out}}."
    ),
    # not in tests: not simulated (Docker absent or misconfigured)
    docker_installed = "Docker is required but not installed.",
    # not in tests: not simulated (Docker absent or misconfigured)
    docker_sudo = "Docker is installed but cannot be called without sudo.",
    # not in tests: not simulated (Docker absent or misconfigured)
    docker_not_running = "Docker is installed but the daemon is not running. Start Docker Desktop and try again.",
    # not in tests: not simulated (Docker absent or misconfigured)
    docker_x_image = "The {.val {image_name}} Docker image is not present.",
    # test-solve_in_situ.R: "solve_in_situ errors when model directory doesn't exist"
    # test-ems_calculate.R: "ems_calculate requires its input files"
    no_model_dir = "The {.arg model_dir} provided {.path {model_dir}} does not exist.",
    # test-solve_in_situ.R: "solve_in_situ errors when input file is without name"
    # test-ems_calculate.R: "ems_calculate requires its input files"
    no_input_names = "Input files provided to {.arg ...} must be named as the appear within the {.arg model_file}.",
    # test-ems_solve.R: "ems_solve errors when verbosity is invalid"
    verbosity_range = "{.arg verbosity} must be 0, 1, or 2.",
    # test-chk_solver_log.R: "a non-zero exit status aborts even with a clean log"
    solver_exit = "The solver exited with status {status} without a recognised error in its log. See {.path {paths$diag_out}}.",
    # test-chk_solver_version.R: the version handshake (release plan
    # 1.1.0): abort by default, downgraded to a warning under
    # ems_option_set(version_check = "warn")
    # test-chk_solver_version.R: "a silent image (pre-1.1) aborts by name"
    solver_version_none = c(
      "The {.val {image}} image gives no answer to {.code teems-solver -version}: its solver predates the versioned interface, and teems {pkg_version} requires teems-solver {min_version} or later.",
      "Pull or rebuild a current image (see the teems-solver repository), or set {.code ems_option_set(version_check = \"warn\")} to run against it anyway."
    ),
    # test-chk_solver_version.R: "a major-version mismatch aborts by name"
    solver_version_major = c(
      "The {.val {image}} image carries teems-solver {solver_version}, whose major version is not the {pkg_major}.x interface teems {pkg_version} drives.",
      "Use a teems-solver {pkg_major}.x image, or set {.code ems_option_set(version_check = \"warn\")} to run against it anyway."
    ),
    # test-chk_solver_version.R: "a solver below the package floor aborts by name"
    solver_version_floor = c(
      "The {.val {image}} image carries teems-solver {solver_version}; teems {pkg_version} requires {min_version} or later.",
      "Pull or rebuild a current image, or set {.code ems_option_set(version_check = \"warn\")} to run against it anyway."
    )
  )
}

build_solve_wrn <- function() {
  list(
    # test-auto_method.R: "the memory fit check refuses a run past the error band and warns inside it"
    memory_tight = c(
      "Estimated peak memory {est_gb} GB for {.val {method}} at {n_tasks} task{?s} is {share} of the container's {mem_gb} GB; the run may not fit.",
      "Remedies: a coarser aggregation, condensation ({.arg backsolve} in {.fun teems::ems_model}), a higher Docker Desktop memory limit, fewer tasks under {.val DBBD}, {.val NDBBD} at one task for an intertemporal model, or a larger host."
    ),
    # test-ems_solve.R: "ems_solve warns when poor accuracy"
    accuracy = c(
      "Only {.emph {accuracy}} of variables accurate to at least 4 digits, below the {a_threshold} threshold.",
      "Adjust with {.arg accuracy_threshold} in {.fun teems::ems_option_set}."
    )
  )
}

build_solve_info <- function() {
  list(
    # the model_diagnostics.txt record lines and the resource
    # rationales. sprintf templates: these are written to a report
    # file, not rendered by cli
    # test-ems_deploy.R: "ems_deploy without a shock announces the null shock"
    null_shock = "No shock has been provided so a
    {.val NULL} shock will be used. A null shock will return all model
    coefficients as they are read and/or calculated in the model file.
    Any significant deviation under these conditions would indicate an error
    in the loading of input files or parsing of model outputs.",
    # test-ems_solve.R: "matrix_method has no auto: the run is what is given, and the record says what ran"
    # test-solver_switches.R: "switches reach the solver and the run records them (e2e)"
    # test-chk_solver_log.R: "the solve record names the BLAS kernel family the run dispatched"
    # test-ems_solve.R: "RANDOM draws are reproducible, seeded by the random_seed option and recorded"
    record = list(
      solver_version = "Solver version: %s",
      blas = "BLAS kernels: %s",
      method = "Solution method: %s%s (subintervals %s)",
      method_steps = " (steps %s)",
      adaptive = "Adaptive stepping: %s (eps tolerance %s)",
      rk_chart = "Runge-Kutta chart: %s (error norm %s over %s, controller %s%s)",
      rk_scope_all = "all elements",
      rk_scope_pc = "percent-change elements",
      rk_h0 = ", initial step %s",
      rk_run = paste0(
        "Runge-Kutta run: %s step(s), %s stage solve(s) (%s reused); ",
        "rejected %s accuracy, %s crossing, %s range test, %s assertion, ",
        "%s guard, %s singular; step length %s to %s"
      ),
      rk_error = paste0(
        "Runge-Kutta estimated error: worst element metric %s (the ",
        "embedded pair's accumulated estimate: an indicator of the ",
        "least-settled elements, not a bound)"
      ),
      matrix_method = "Matrix method: %s (laA %s, laDi %s, laD %s; fastrefac %s; ma48u %s)",
      ma48u_default = "default",
      la_used = "Workspace used (equivalent percents): laA %s, laDi %s, laD %s",
      condest = paste0(
        "Solve quality (condest): kappa_w1 max %s, kappa_w2 max %s, ",
        "omega max %s (%s solve(s) measured, %s zero-rhs skip(s))"
      ),
      parallelism = "Parallelism: %s MPI task(s), %s OpenMP thread(s)",
      storage = "Coefficient storage: %s precision",
      system = "System: %s equations, %s exogenous elements",
      modes = "Modes: assertions %s; range test initial %s, updated %s; postsim %s; gpzerodivide %s",
      random_seed = "Random seed: %s (RANDOM draws in the model)",
      complementarity = paste0(
        "Complementarity: %s active component(s); approximate run %s ",
        "Euler steps (%s; redo %s, min fraction %s); accurate run %s; ",
        "state/bound errors %s"
      ),
      memory_phases = "Memory (resident GB, max per rank / sum over ranks): %s",
      memory_phase = "%s %s/%s",
      memory_peak = "Memory high-water mark: %s GB max per rank, %s GB sum over ranks",
      resources = "Resources: n_tasks %s, n_threads %s, inmemory %s, tempdir %s (%s)",
      host_unknown = "container not inspected",
      host = "container %s core(s), %s",
      solver_default = "solver default",
      unknown_gb = "unknown",
      fit_na = "  memory check: not applied (system size unknown)",
      fit_unknown = "  memory check: estimate %s, container limit unknown",
      fit = paste0(
        "  memory check: %s at %s task(s) estimated %s = %s kB/eq x %s ",
        "plain-equivalent equations%s -> %s of %s (%s)"
      ),
      fit_condensed = " (condensed)",
      refine = "  refinement (DBBD, one step per solve): %s (%s)",
      refine_reason = list(
        on = "the default",
        off = "set by the refine option"
      ),
      refine_run = "Refinement (DBBD): %s solve(s) refined; worst residual ratio %s before the step, %s after",
      refine_skipped = "Refinement (DBBD): off"
    ),
    # .resolve_resources() rationales, shown with the recommendation
    # test-ems_probe.R: "the probe prints its recommendation"
    rationale = list(
      SBBD = "ranks to the knee, threads take the remaining cores",
      DBBD = "two ranks on a laptop, more only where cores and memory allow; threads take the rest",
      NDBBD = "one rank holds the tables once; the solver budgets its own threads",
      LU = "one rank; threads for the condensed factorization"
    ),
    # test-ems_solve.R: "condensed deployments are advised against bordered methods (roadmap 6.2)"
    condense_bordered = c(
      "This deployment is condensed ({n_backsolve} backsolved variable{?s}, {share} of the uncondensed system) and {.val {matrix_method}} is a bordered method.",
      "Substitution densifies the diagonal blocks the bordered methods exploit: condensed deployments solve slower at every elimination share.",
      "Condensation pays under {.val LU}; deploy without {.arg backsolve} for bordered runs."
    ),
    # test-ems_solve.R: "condensed intertemporal deployments are advised against"
    condense_intertemporal = c(
      "This intertemporal deployment is condensed ({n_backsolve} backsolved variable{?s}, {share} of the uncondensed system).",
      "Condensation is counterproductive on intertemporal models: bordered runs solve slower condensed, and a fully condensed {.val LU} run is slower still than plain {.val SBBD}.",
      "Deploy without {.arg backsolve} and solve with {.val SBBD}."
    ),
    # test-ems_options.R: "docker tag auto-selection"
    docker_tag_auto = "Using image {.field teems:{tag}} (matches host CPU capability {.val {level}}). Set {.arg docker_tag} via {.fn ems_option_set} to override.",
    # test-ems_solve.R: "ems_solve informs terminal run"
    terminal_run = "{.arg terminal_run} activated. To solve and compose outputs:",
    # test-ems_solve.R: "ems_solve informs terminal run"
    terminal_run_steps = c("Run the above command in your OS terminal.",
                           "If errors are present in the terminal output during an ongoing run, it is possible to stop the relevant {.field {hsl}} process early according to your OS-specific system activity monitor.",
                           "Any error and/or singularity indicators will be present in the model diagnostic output: {.path {diag_out}}.",
                           "If no errors or singularities are detected, use the following expression to structure solver binary outputs: {.run ems_compose({cmf_path})}"),
    # test-inform_messages.R: "a finished run reports its elapsed time and accuracy"
    accuracy = "{.emph {accuracy}} of variables accurate to at least 4 digits.",
    # test-inform_messages.R: "a finished run reports its elapsed time and accuracy"
    elapsed_time = "Elapsed time: {elapsed_time}"
  )
}
