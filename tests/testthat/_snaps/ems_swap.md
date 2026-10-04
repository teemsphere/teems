# ems_swap errors when var is missing

    x argument `var` is missing, with no default

# ems_swap errors when var is not character

    x `var` must be a character, not a number.

# ems_swap errors when invalid closure element entry swapped in

    x Elements or subsets designated for a swap are not part of the set REGr: not_an_ele.
    i Valid elements include "chn", "row", and "usa".
    i Valid subsets include *none*.
    i Call: `ems_swap(var = "qfd", REGr = "not_an_ele")`

# ems_swap errors when invalid closure element entry swapped out

    x Elements or subsets designated for a swap are not part of the set REGr: not_an_ele.
    i Valid elements include "chn", "row", and "usa".
    i Valid subsets include *none*.
    i Call: `ems_swap(var = "tfd", REGr = "not_an_ele")`

# ems_swap errors when invalid set provided to swap-in

    x Set "REGs" is not associated with "qfd".
    i Set designations in teems are the uppercase set name and lowercase variable-specific index.
    i For "qfd" these include: COMMc, ACTSa, REGr, and ALLTIMEt.
    i Call: `ems_swap(var = "qfd", REGs = "row")`

# ems_swap errors when dots passed without names

    x Subset arguments in `...` must be named pairs: `SETi = "component"`.
    i Note that set names consist of the concatenation of the set name and variable-specific lowercase index.
    i Call: `ems_swap(var = "tfd", "usa")`

# ems_swap errors when invalid set provided to swap-out

    x Set "REGs" is not associated with "tfd".
    i Set designations in teems are the uppercase set name and lowercase variable-specific index.
    i For "tfd" these include: COMMc, ACTSa, REGr, and ALLTIMEt.
    i Call: `ems_swap(var = "tfd", REGs = "row")`

# ems_swap errors when invalid element provided to swap-in

    x Elements or subsets designated for a swap are not part of the set REGr: not_an_ele.
    i Valid elements include "chn", "row", and "usa".
    i Valid subsets include *none*.
    i Call: `ems_swap(var = "qfd", REGr = "not_an_ele")`

# ems_swap errors when invalid element provided to swap-out

    x Elements or subsets designated for a swap are not part of the set REGr: not_an_ele.
    i Valid elements include "chn", "row", and "usa".
    i Valid subsets include *none*.
    i Call: `ems_swap(var = "tfd", REGr = "not_an_ele")`

# full ems_swap out var not fully exogenous

    x "qe" cannot be fully swapped out; it is not fully exogenous.
    i Call: `ems_swap(var = "qe")`

# ems_swap out some ele not exogenous

    x 2 tuples on "qe" designated for swap-out are not exogenous: 1: capital chn 1 and 2: capital row 1.
    i Call: `ems_swap(var = "qe", ENDWMSe = "ENDWC", ALLTIMEt = "FWDTIME")`

# ems_swap out no entry

    x No closure entry for swap-out variable "qfd".
    i Call: `ems_swap(var = "qfd", COMMc = "food")`

# ems_swap duplicate tup in closure

    x Swap on "qe" creates duplicate closure entries: 1: capital chn 0, 2: capital row 0, and 3: capital usa 0.
    i Call: `ems_swap(var = "qe", ENDWMSe = "ENDWC", ALLTIMEt = 0)`

# ems_swap in ele component not valid

    x Elements or subsets designated for a swap are not part of the set COMMc: zzz.
    i Valid elements include "crops", "food", "livestock", "mnfcs", and "svces".
    i Valid subsets include MARG and NMRG.
    i Call: `ems_swap(var = "qfd", COMMc = "zzz")`

# ems_swap out var component no entry

    x Elements or subsets designated for a swap are not part of the set COMMc: zzz.
    i Valid elements include "crops", "food", "livestock", "mnfcs", and "svces".
    i Valid subsets include MARG and NMRG.
    i Call: `ems_swap(var = "qfd", COMMc = "zzz")`

# ems_swap in variable not valid

    x Swap variable "not_a_var" not found in the model.
    i Call: `ems_swap(var = "not_a_var")`

# ems_swap out variable not valid

    x Swap variable "not_a_var" not found in the model.
    i Call: `ems_swap(var = "not_a_var")`

# ems_swap in subset not valid

    x Set "NOT_A_SET" is not associated with "pop".
    i Set designations in teems are the uppercase set name and lowercase variable-specific index.
    i For "pop" these include: REGr and ALLTIMEt.
    i Call: `ems_swap(var = "pop", NOT_A_SET = "usa")`

# ems_swap out subset not valid

    x Set "NOT_A_SET" is not associated with "pop".
    i Set designations in teems are the uppercase set name and lowercase variable-specific index.
    i For "pop" these include: REGr and ALLTIMEt.
    i Call: `ems_swap(var = "pop", NOT_A_SET = "usa")`

# two swaps in on one tuple abort with no valid order

    x No order of the swaps reconciles the closure; 1 swap cannot be applied.
    i qfd("food","crops","usa",0), qfd("food","crops","usa",1), qfd("food","crops","usa",2), qfd("food","food","usa",0), qfd("food","food","usa",1) and 10 more swapped in 2 and out 0 time(s) while initially endogenous: ems_swap(var = "qfd", COMMc = "food", REGr = "usa"); ems_swap(var = "qfd", COMMc = "food", REGr = "usa")
    i ems_swap(var = "qfd", COMMc = "food", REGr = "usa") cannot swap in, already exogenous: qfd("food","crops","usa",0), qfd("food","crops","usa",1), qfd("food","crops","usa",2), qfd("food","food","usa",0), qfd("food","food","usa",1) and 10 more

# a tuple swapped out twice aborts with no valid order

    x No order of the swaps reconciles the closure; 1 swap cannot be applied.
    i tfd("food","crops","usa",0), tfd("food","crops","usa",1), tfd("food","crops","usa",2), tfd("food","food","usa",0), tfd("food","food","usa",1) and 10 more swapped in 0 and out 2 time(s) while initially exogenous: ems_swap(var = "tfd", COMMc = "food", REGr = "usa"); ems_swap(var = "tfd", COMMc = "food", REGr = "usa")
    i ems_swap(var = "tfd", COMMc = "food", REGr = "usa") cannot swap out, not exogenous: tfd("food","crops","usa",0), tfd("food","crops","usa",1), tfd("food","crops","usa",2), tfd("food","food","usa",0), tfd("food","food","usa",1) and 10 more

# swaps blocking one another abort with no valid order

    x No order of the swaps reconciles the closure; 2 swaps cannot be applied.
    i ems_swap(var = "qe", ENDWMSe = "ENDWC") cannot swap in, already exogenous: qe("capital","chn",0), qe("capital","row",0), qe("capital","usa",0)
    i ems_swap(var = "qe", ENDWMSe = "ENDWC") cannot swap out, not exogenous: qe("capital","chn",1), qe("capital","chn",2), qe("capital","row",1), qe("capital","row",2), qe("capital","usa",1) and 1 more

