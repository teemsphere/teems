#' @title Solve a model with a Runge-Kutta method
#' @export
#' @description An [`ems_solve()`] wrapper restricted to the
#'   Runge-Kutta methods, with the step controls that exist only
#'   for them and defaults chosen for them: adaptive `"DoPri54"`
#'   from four initial steps.
#' @param solution_method The method: `"DoPri54"` (default),
#'   `"BoSha32"`, `"RK4"`, `"Heun"` or `"RK2"`. `"DoPri54"` and
#'   `"BoSha32"` are embedded pairs, so each step also yields an
#'   error estimate for adaptive control to act on; `"RK4"`,
#'   `"Heun"` and `"RK2"` take fixed steps. `"DoPri54"` measured
#'   the most accurate per step on large shocks and `"RK4"` with
#'   a few steps the best cheap fixed-step method. When most
#'   retries come from step checks rather than from
#'   `eps_tolerance`, the cheaper steps of `"BoSha32"` can make
#'   it faster (Schiffmann 2022).
#' @param steps Integer length 1 (default `4L`), the number of
#'   steps; under adaptive control, the number to start from.
#'   Runge-Kutta methods do not extrapolate, so there is no
#'   step-count triple.
#' @param eps_tolerance Numeric length 1 (default `0.01`), the
#'   error estimate a step may not exceed under adaptive control.
#'   On large shocks `0.01` gives four correct digits on most
#'   elements; `0.001` adds the last digits at about 1.6 times
#'   the cost. GEMPACK suggests `0.1`, which measured rougher on
#'   large shocks. Ignored when `adaptive = "no"`.
#' @param adaptive Character length 1. `NULL` (default) means
#'   `"yes"` for `"DoPri54"` and `"BoSha32"` and `"no"` for the
#'   fixed-step methods, which have no error estimate.
#'   * `"no"`: fixed steps.
#'   * `"yes"`: a step whose error estimate exceeds
#'   `eps_tolerance` is redone shorter, and a step that passes
#'   may lengthen the next one, by at most a factor of two either
#'   way. A step that fails a step check (a percentage-change
#'   variable crossing `-100%`, a coefficient range test, a TAB
#'   assertion or the `guard`) is also retried shorter.
#'   * `"accuracy-only"`: as `"yes"`, but step checks do not
#'   trigger a retry.
#' @param max_retries Integer length 1 (default `3L`). How many
#'   times a step that fails a step check is retried shorter
#'   before the run aborts.
#' @param retry_adjust Numeric length 1 in (0, 1) (default
#'   `0.5`). The factor the step is shortened by on each retry.
#' @param chart Character length 1, the coordinates the stages
#'   are combined in. `"log"` (default) carries each
#'   percentage-change variable as the logarithm of its level
#'   ratio, so levels stay positive whatever the step and no
#'   percentage-change variable can reach `-100%`; the methods
#'   keep their order (Munthe-Kaas 1999). `"percent"` combines
#'   the stages in percentage changes from the initial level.
#' @param error_norm Character length 1. Whether the worst
#'   element (`"max"`, default) or the root mean square over
#'   elements (`"rms"`) is compared with `eps_tolerance`.
#' @param controller Character length 1, the step-size rule:
#'   `"std"` (default), or `"pi"`, which also weighs the previous
#'   step's estimate and rejects fewer steps on rough solution
#'   paths.
#' @param scope Character length 1, which elements the accept
#'   test looks at: `"pct"` (default), the percentage-change
#'   variables, whose error estimate is dimensionless; or
#'   `"all"`, which adds the ordinary-change variables. Their
#'   estimate is in their own units, so an accumulator in dollars
#'   can reject every step early in the path. `error_estimate` is
#'   reported for every element either way.
#' @param h_init Numeric length 1 in (0, `1/steps`] (default
#'   `NULL`, which is `1/steps`), the first step length. Under
#'   adaptive control the first step is also capped from the
#'   initial gradient.
#' @param guard Numeric length 1 greater than 1 (default `NULL`,
#'   no guard), `chart = "log"` only: the level ratio beyond
#'   which a stage is rejected and the step retried.
#' @param ... Named extras, as `...` of [`ems_solve()`]: the MA48
#'   workspace guesses (`laA`, `laD`, `laDi`) and the expert
#'   solver flags. The Runge-Kutta controls are formal arguments
#'   here, so their `rk_*` names are refused.
#' @inheritParams ems_solve
#' @return As [`ems_solve()`].
#' @seealso [`ems_solve()`].
#' @references Schiffmann, F. (2022), "Runge Kutta integrators for
#'   fast and accurate solutions in GEMPACK", GEMPACK Software,
#'   Centre of Policy Studies, Victoria University, Melbourne
#'   (14 April).
#'
#'   Munthe-Kaas, H. (1999), "High order Runge-Kutta methods on
#'   manifolds", Applied Numerical Mathematics, vol. 29,
#'   pp. 115-127.
#'
#'   Dormand, J.R. and Prince, P.J. (1980), "A family of embedded
#'   Runge-Kutta formulae", Journal of Computational and Applied
#'   Mathematics, vol. 6, pp. 19-26.
#'
#'   Bogacki, P. and Shampine, L.F. (1989), "A 3(2) pair of
#'   Runge-Kutta formulas", Applied Mathematics Letters, vol. 2,
#'   pp. 321-325.
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
                   eps_tolerance = 0.01,
                   adaptive = NULL,
                   matrix_method = c("LU", "DBBD", "SBBD", "NDBBD"),
                   n_tasks = 1L,
                   n_threads = 1L,
                   precision = c("single", "double"),
                   n_subintervals = 1L,
                   verbosity = 1L,
                   suppress_outputs = FALSE,
                   terminal_run = FALSE,
                   accuracy_vars = NULL,
                   complementarity = NULL,
                   max_retries = 3L,
                   retry_adjust = 0.5,
                   chart = c("log", "percent"),
                   error_norm = c("max", "rms"),
                   controller = c("std", "pi"),
                   scope = c("pct", "all"),
                   h_init = NULL,
                   guard = NULL,
                   ...
) {
if (missing(cmf_path)) {
  .cli_missing(cmf_path)
}
args_list <- mget(setdiff(names(formals()), "..."))
args_list$solve_args <- list(...)
call <- match.call()
output <- .implement_RK(
  args_list = args_list,
  call = call
)
if (is.null(output) || is.character(output)) {
  invisible(output)
} else {
  output
}
}
