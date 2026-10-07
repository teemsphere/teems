#' @keywords internal
#' @noRd
.chk_flag <- function(x, nme, call) {
  if (!is.null(x) && (!is.logical(x) || length(x) != 1L || is.na(x))) {
    bad_arg <- nme
    requirement <- solve_err$requirement$logical_flag
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}

#' @title Prepare complementarity run controls
#' @export
#' @description Prepares the run controls for a simulation whose
#'   model declares one or more `Complementarity` statements with the
#'   complementarity variable (partly) endogenous. Such simulations
#'   solve in two automated passes (GEMPACK manual ch. 51): a
#'   many-step Euler *approximate run* that discovers the final state
#'   of each complementarity component, followed by an *accurate run*
#'   with the requested `solution_method` after the solver rewrites
#'   the closure and shocks to pin each component in its discovered
#'   state. Every control has a sensible solver default; construct
#'   this object only to override them. Arguments left `NULL` are not
#'   passed and the solver default applies.
#' @return A `teems_complementarity` list object to be passed to the
#'   `complementarity` argument of [`ems_solve()`] or
#'   [`solve_in_situ()`].
#' @param steps_approx_run Integer length 1 (default `NULL`), the
#'   number of Euler steps in the approximate run. The solver default
#'   is the accurate method's step sum (e.g. 14 for Gragg
#'   `steps = c(2L, 4L, 8L)`). More steps track state changes more
#'   precisely; steps may additionally be redone (shortened) around a
#'   state change, so the executed count can exceed the request.
#' @param redo_steps Logical length 1 (default `NULL`, solver default
#'   `TRUE`). When a component changes state during an Euler step,
#'   redo the step with a shorter length so the change lands just
#'   before the step's end, limiting bound overshoot.
#' @param redo_step_min_fraction Numeric length 1 in (0, 1] (default
#'   `NULL`, solver default `0.005`), the shortest redone step as a
#'   fraction of the original step length.
#' @param do_approx_run Logical length 1 (default `NULL`, solver
#'   default `TRUE`). `FALSE` skips the approximate run and takes the
#'   pre-simulation states as the accurate run's targets — a time
#'   saver only when the shocks change no states. A missed state
#'   change is caught by the post-run verification.
#' @param do_acc_run Logical length 1 (default `NULL`, solver default
#'   `TRUE`). `FALSE` stops after the approximate run and keeps its
#'   (Euler-accurate) solution as the simulation result.
#' @param state_bound_error Character length 1, `"fatal"` or `"warn"`
#'   (default `NULL`, solver default `"fatal"`). Severity of the
#'   post-accurate-run checks that every component sits in its
#'   approximate-run state with the complementarity variable inside
#'   its bounds. When downgraded to `"warn"`, inspect the solver log
#'   carefully.
#' @seealso [`ems_solve()`] and [`solve_in_situ()`] for loading the
#'   output of this function.
#' @references Harrison, W.J., Horridge, M., Pearson, K.R. and
#'   Wittwer, G. (2004), "A Practical Method for Explicitly Modeling
#'   Quotas and Other Complementarities", Computational Economics,
#'   vol. 23, pp. 325-341. A preliminary version was Centre of Policy
#'   Studies and the Impact Project Preliminary Working Paper No.
#'   IP-78, Melbourne (April 2002).
#'   \url{http://www.copsmodels.com/elecpapr/ip-78.htm}
#'
#'   Horridge, J.M., Jerie, M., Mustakinov, D. and Schiffmann, F.
#'   (2018), GEMPACK manual, GEMPACK Software, Centre of Policy
#'   Studies, Victoria University, Melbourne, ISBN 978-1-921654-34-3.
#'   \url{https://ideas.repec.org/p/cop/wpaper/gpman.html}
#' @examples
#' # More Euler steps for a large shock, keeping errors fatal
#' ems_complementarity(steps_approx_run = 40L)
#'
#' # Skip the approximate run when no state changes are expected
#' ems_complementarity(do_approx_run = FALSE)
#'
#' # Inspect the approximate run only
#' ems_complementarity(do_acc_run = FALSE)
ems_complementarity <- function(steps_approx_run = NULL,
                                redo_steps = NULL,
                                redo_step_min_fraction = NULL,
                                do_approx_run = NULL,
                                do_acc_run = NULL,
                                state_bound_error = NULL) {
  call <- match.call()
  if (!is.null(steps_approx_run) &&
    (!is.numeric(steps_approx_run) || length(steps_approx_run) != 1L ||
      is.na(steps_approx_run) ||
      steps_approx_run != as.integer(steps_approx_run) ||
      steps_approx_run < 1)) {
    bad_arg <- "steps_approx_run"
    requirement <- solve_err$requirement$positive_int
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  .chk_flag(redo_steps, "redo_steps", call)
  .chk_flag(do_approx_run, "do_approx_run", call)
  .chk_flag(do_acc_run, "do_acc_run", call)
  if (!is.null(redo_step_min_fraction) &&
    (!is.numeric(redo_step_min_fraction) ||
      length(redo_step_min_fraction) != 1L ||
      is.na(redo_step_min_fraction) ||
      redo_step_min_fraction <= 0 || redo_step_min_fraction > 1)) {
    bad_arg <- "redo_step_min_fraction"
    requirement <- solve_err$requirement$half_open_unit
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(state_bound_error) &&
    (!is.character(state_bound_error) || length(state_bound_error) != 1L ||
      !state_bound_error %in% c("fatal", "warn"))) {
    bad_arg <- "state_bound_error"
    requirement <- solve_err$requirement$fatal_or_warn
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (isFALSE(do_approx_run) && isFALSE(do_acc_run)) {
    .cli_action(solve_err$comp_runs_off,
      action = c("abort", "inform"),
      call = call
    )
  }
  structure(
    list(
      steps_approx_run = if (is.null(steps_approx_run)) {
        NULL
      } else {
        as.integer(steps_approx_run)
      },
      redo_steps = redo_steps,
      redo_step_min_fraction = redo_step_min_fraction,
      do_approx_run = do_approx_run,
      do_acc_run = do_acc_run,
      state_bound_error = state_bound_error
    ),
    class = "teems_complementarity"
  )
}
