#' @title Solve a model with a Runge-Kutta method
#' @export
#' @description A Runge-Kutta front end for [`ems_solve()`]: the same
#'   solver run restricted to the Runge-Kutta `solution_method`
#'   flavors, with the step-control arguments that only exist for
#'   them (`adaptive`, `eps_tolerance`, `max_retries`,
#'   `retry_adjust`) and RK-tuned defaults — `"DoPri54"` with a
#'   single step and adaptive control enabled. Everything else
#'   (`matrix_method`, `n_tasks`, `precision`, ...) is forwarded to
#'   [`ems_solve()`] unchanged, which performs all validation and
#'   remains fully capable of Runge-Kutta runs itself.
#' @param solution_method The Runge-Kutta flavor, one of
#'   `"DoPri54"` (default), `"BoSha32"`, `"RK4"`, `"Heun"` or
#'   `"RK2"`. The embedded pairs (`"DoPri54"`, `"BoSha32"`) carry
#'   the per-step error estimate that `adaptive` control acts on.
#'   `"Heun"` (the explicit trapezoid rule) and `"RK2"` (the midpoint
#'   rule) cost the same two solves per step; Heun is
#'   strong-stability-preserving, the midpoint rule is not.
#' @param steps Integer length 1 (default `4L`), the initial number
#'   of steps. Runge-Kutta methods use no Richardson extrapolation,
#'   so no step-count triple is involved; under `adaptive` control
#'   this is the initial step count only.
#' @param adaptive Character length 1, adaptive step-size control
#'   for the embedded Runge-Kutta methods (`"BoSha32"`,
#'   `"DoPri54"`). Default `NULL` resolves to `"yes"` for the
#'   embedded pairs and `"no"` for `"RK2"`/`"RK4"` (which provide no
#'   error estimate). Choices:
#'   * `"no"`: Fixed steps.
#'   * `"yes"`: After each step the worst per-component error
#'   metric is compared against `eps_tolerance`; failing steps are
#'   redone with a smaller step size and passing steps adjust the
#'   next step size (at most halving or doubling it). A step on
#'   which a percentage-change variable crosses `-100%` is also
#'   retried at a reduced step size.
#'   * `"accuracy-only"`: As `"yes"`, but only the error metric is
#'   acted on; check failures are ignored.
#' @param eps_tolerance Numeric length 1 (default `0.01`), the
#'   per-step error-metric bound targeted by `adaptive` control.
#'   Measured on large shocks against 32-step references, `0.01`
#'   reaches four correct digits on about 92-96% of elements at a
#'   third of the factorizations the pre-2026-09 driver needed;
#'   `0.1` (GEMPACK's recommendation) is loose now that the accept
#'   test is steered by percentage-change variables only, and
#'   `0.001` buys the last digits at roughly 1.6x the cost. Ignored
#'   when `adaptive = "no"`.
#' @param max_retries Integer length 1 (default `3L`),
#'   `adaptive = "yes"` only: how many times a step
#'   failing the -100% crossing check is retried at reduced length
#'   before the run aborts.
#' @param retry_adjust Numeric length 1 in (0, 1) (default `0.5`),
#'   adaptive control only: the step-length
#'   multiplier applied on each retry.
#' @param chart Character length 1, the coordinate the stages are
#'   combined in. `"log"` (default) carries every percentage-change
#'   variable with a nonzero base as the logarithm of its level ratio:
#'   levels stay positive for any tableau and any step (a
#'   percentage-change variable cannot reach `-100%`), and every
#'   method keeps its order (Munthe-Kaas 1999 on the multiplicative
#'   group). `"percent"` is the GEMPACK-orientation arithmetic (stage
#'   combination in initial-based percentage changes).
#' @param error_norm Character length 1, the norm of the per-element
#'   error metric that the adaptive accept test compares against
#'   `eps_tolerance`: `"max"` (default; GEMPACK's rule, the worst
#'   element decides) or `"rms"` (root mean square over elements).
#' @param controller Character length 1, the step-size controller:
#'   `"std"` (default; the elementary rule with safety factor 0.85 and
#'   a 0.5-2 clamp) or `"pi"` (proportional-integral control, which
#'   damps step growth with the previous step's estimate and rejects
#'   less often on rough paths).
#' @param scope Character length 1, which elements steer the adaptive
#'   accept test: `"pct"` (default) the percentage-change variables,
#'   whose error metric is dimensionless; `"all"` adds the
#'   ordinary-change variables (GEMPACK's rule). Early in the path
#'   `max(1, |X|)` reads an ordinary-change variable's estimate in its
#'   own units, so welfare-decomposition accumulators in $ millions can
#'   reject every step; every element is still reported in
#'   `error_estimate`.
#' @param h_init Numeric length 1 in (0, 1] (default `NULL`): the
#'   first step length. Absent, the solver starts from `1/steps` and
#'   caps it from the initial gradient so that no element moves by
#'   more than about half its level in the first attempt.
#' @param guard Numeric length 1 greater than 1 (default `NULL`,
#'   solver default `1e30`), `chart = "log"` only: the level ratio
#'   beyond which a stage state is rejected and the step retried.
#' @param ... All other [`ems_solve()`] arguments
#'   (`matrix_method`, `n_tasks`, `precision`, `terminal_run`, ...),
#'   forwarded unchanged.
#' @inheritParams ems_solve
#' @return As [`ems_solve()`].
#' @seealso [`ems_solve()`] for the general interface and the full
#'   argument documentation.
#' @references Schiffmann, F. (2022), "Runge Kutta integrators for
#'   fast and accurate solutions in GEMPACK".
#' @examples
#' \dontrun{
#' # The following examples require the teems solver to be built.
#' # See https://teemsphere.github.io/ to get started.
#'
#' # Adaptive DoPri54 (the defaults):
#' ems_RK(cmf_path)
#'
#' # Fixed-step RK4:
#' ems_RK(cmf_path, solution_method = "RK4", steps = 8L)
#' }
ems_RK <- function(cmf_path,
                   solution_method = c("DoPri54", "BoSha32", "RK4", "Heun", "RK2"),
                   steps = 4L,
                   adaptive = NULL,
                   eps_tolerance = 0.01,
                   max_retries = 3L,
                   retry_adjust = 0.5,
                   chart = c("log", "percent"),
                   error_norm = c("max", "rms"),
                   controller = c("std", "pi"),
                   scope = c("pct", "all"),
                   h_init = NULL,
                   guard = NULL,
                   ...) {
  if (missing(cmf_path)) {
    .cli_missing(cmf_path)
  }
  solution_method <- rlang::arg_match(solution_method)
  chart <- rlang::arg_match(chart)
  error_norm <- rlang::arg_match(error_norm)
  controller <- rlang::arg_match(controller)
  scope <- rlang::arg_match(scope)
  if (is.null(adaptive)) {
    adaptive <- if (solution_method %in% c("BoSha32", "DoPri54")) {
      "yes"
    } else {
      "no"
    }
  }
  sol <- ems_solve(
    cmf_path = cmf_path,
    solution_method = solution_method,
    steps = steps,
    adaptive = adaptive,
    eps_tolerance = eps_tolerance,
    max_retries = max_retries,
    retry_adjust = retry_adjust,
    rk_chart = chart,
    rk_norm = error_norm,
    rk_controller = controller,
    rk_scope = scope,
    rk_h0 = h_init,
    rk_guard = guard,
    ...
  )
  return(sol)
}
