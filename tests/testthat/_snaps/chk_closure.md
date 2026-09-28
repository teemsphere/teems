# unknown closure variables suggest candidates

    x Closure variable "qgdpp" not found among the model's variables.
    i Did you mean "qgdp"?

# unknown closure variables without a near match omit candidates

    x Closure variable "zzzzzzzz" not found among the model's variables.

# linear names of levels variables resolve in the closure (GEMPACK manual 9.2.2)

    x Closure variable "p_CTAXBAS" not found among the model's variables.
    i Did you mean "CTAXBAS"?

---

    x Closure variable "c_qgdp" not found among the model's variables.
    i Did you mean "qgdp"?

# closure names match case-insensitively and take the declared spelling

    x Closure variable "Nothere" not found among the model's variables.

# unsquared closures abort with arithmetic and candidates

    x The closure does not square the system: 4 endogenous variable elements against 3 equation elements.
    i Arithmetic: 6 variable elements - 2 exogenous elements (closure after swaps) = 4 endogenous; the equation system determines exactly 3, so 1 element must still be exogenized.
    i Candidates: exogenizing 1 element of one of y closes the gap exactly.
    i If the counts look right but the partition is structurally deficient, run `teems::ems_probe()` on the deployed model for a named diagnosis.

# over-exogenized closures name endogenizing candidates

    x The closure does not square the system: 2 endogenous variable elements against 3 equation elements.
    i Arithmetic: 6 variable elements - 4 exogenous elements (closure after swaps) = 2 endogenous; the equation system determines exactly 3, so 1 element too many are exogenous (endogenize via swaps).
    i No single variable matches the gap exactly; nearest by element count: y (3).
    i If the counts look right but the partition is structurally deficient, run `teems::ems_probe()` on the deployed model for a named diagnosis.

# an over-exogenized count with nothing exogenous to release names no candidate

    x The closure does not square the system: 2 endogenous variable elements against 3 equation elements.
    i Arithmetic: 6 variable elements - 4 exogenous elements (closure after swaps) = 2 endogenous; the equation system determines exactly 3, so 1 element too many are exogenous (endogenize via swaps).
    i No single-variable candidate closes the gap.
    i If the counts look right but the partition is structurally deficient, run `teems::ems_probe()` on the deployed model for a named diagnosis.

# unresolvable quantifier sets warn and skip the count check

    ! Equation quantifier set "MYSTERY" could not be sized before deployment, so the closure count check (endogenous elements = equation elements) was skipped.
    i A closure that does not square the system will be reported by the solver instead.

