# ems_model requires both model_file and closure_file

    x argument `model_file` is missing, with no default

# ems_model requires closure_file when only model_file provided

    x argument `closure_file` is missing, with no default

# ems_model requires model_file when only closure_file provided

    x argument `model_file` is missing, with no default

# ems_model rejects non-character model_file

    x `model_file` must be a character, not a number.

# ems_model rejects non-character closure_file

    x `closure_file` must be a character, not `TRUE`.

---

    x `closure_file` must be a character, not a number.

# ems_model rejects non-existent model_file file

    x Cannot open file 'not_a_file': No such file.

# ems_model rejects non-existent closure_file

    x Cannot open file 'not_a_file': No such file.

# ems_model rejects invalid coefficient arguments

    x `NOT_A_COEFF` is not declared in the model.

# a value for a coefficient that is neither read nor assigned aborts

    x `TESTC` is neither read in nor assigned on the LHS of a formula in the model.

# invalid numeric to a formula

    x Directly assigned numeric values must be length 1.
    i To assign heterogeneous values, use a `data.frame` with the appropriate set columns.

# unbalanced PostSim markers

    x Unbalanced PostSim section markers: 3 `PostSim (Begin)` against 2 `PostSim (End)` (GEMPACK manual 12.2).

# invalid tab statement

    Code
      ems_model(err_model, closure_file)
    Condition
      Error in `ems_model()`:
      x teems version_number does not support Display statements.
      i Supported statements include: File, Coefficient, Read, Update, Set, Subset, Formula, Assertion, Variable, Equation, Write, Zerodivide, Omit, Substitute, Backsolve, Postsim, Mapping, Complementarity, ..., Break, and Cycle.

# invalid intertemporal header

    x Intertemporal timestep header "YEAR" not found in loaded data.
    i Use `teems::ems_option_set()` `timestep_header` to set a custom timestep header.

# invalid read statement

    x Read statements missing "from file" detected.

# a set builder on an undeclared/unread coefficient aborts

    x Set builder "ENDWM" conditions on "ENDOWFLAG", which is neither Read from an input file nor assigned by a Formula.
    i Declare the coefficient and Read or compute it before the Set statement.

# intertemporal set equality

    x Set equality involving an intertemporal set detected: Set ALLTIME2 = ALLTIME.
    i Converting between intertemporal and non-intertemporal sets via set equality is not supported.

# unparseable set definition

    x Unparseable Set definition detected: = ENDWM ENDWS.

# invalid set qualifier

    x Invalid set qualifier detected: (static).

# conditional set builders (GEMPACK manual 10.1.2)

    x Unsupported condition in the Set builder "BADX": "= (all,c,COMM: VDFB(c,\"crops\",\"chn\",\"t0\") > 0 and)".
    i A condition compares expressions over coefficients (Read, or assigned by Formulas), mappings, `$POS` and sums, joined by `and`, `or` and `not` (GEMPACK manual 10.1.2); every name it uses must be declared.

---

    x Unsupported condition in the Set builder "BADX": "= (all,c,COMM: VDFB(c,\"crops\",\"chn\",\"t0\") > NOPE(c))".
    i A condition compares expressions over coefficients (Read, or assigned by Formulas), mappings, `$POS` and sums, joined by `and`, `or` and `not` (GEMPACK manual 10.1.2); every name it uses must be declared.

# set builders on indicator-formula operands (GTAP-AEZ UNITD* shape)

    x Set referenced before declaration in "Set BADX = (all,c,NOSET: VDFB(c,\"crops\",\"chn\",\"t0\") > 0)": "NOSET".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

# a Set built from an excluded coefficient aborts

    x Set "ENDWMX" depends on coefficient "ENDOWFLAG" (header "EFLG"), which is excluded from the data by `full_exclude`.
    i The flag-header approach (GTAP `ENDOWFLAG`/`SLUG`) is not supported through the R package (the solver alone accepts it): declare the set explicitly, e.g. `Set ENDWM (capital, labor);`.

# expression IF conditions (LULC shape, manual 11.4.5/11.4.6)

    x IF condition references variable "PDS": Equation E_ifbad (all,c,COMM)(all,r,REG)(all,t,ALLTIME) ifbad(c,r,t) = IF[VDB(c,r,t)*pds(c,r,t) > 0, pds(c,r,t)].
    i Conditions are evaluated from coefficient values only (GEMPACK manual 11.4.6/11.4.8).

# index, element and mapping IF comparisons take the $POS route (manual 11.4.11)

    x Unsupported IF condition detected: r > 3.
    i An index, quoted element or mapping expression can only be compared with another index, mapping expression or quoted element (GEMPACK manual 11.4.11); a data comparison takes a coefficient reference on both sides.

---

    x Unsupported IF condition detected: r EQ c.
    i The compared sets REG and COMM are neither equal nor is one a declared subset of the other (GEMPACK manual 11.4.11.2).

---

    x Unsupported IF condition detected: r < s.
    i Ordered comparisons (< <= > >=) of indices need an intertemporal set; REG is not one, so only EQ/NE apply (GEMPACK manual 11.4.11.2).

# IF takes a condition and one value

    x IF term IF[VTRPROV(r,t) > 0, 1, 2] has more than a condition and one value.
    i An IF takes two arguments, `IF[condition, value]` (GEMPACK manual 11.4.6).

# IN conditions stay single (manual 11.4.7 rule 5)

    x IF condition c in MARG and VDB(c,r,t) > 0 combines an `index IN set` test with AND, OR or NOT.
    i An `IN` condition cannot be combined with AND, OR or NOT (GEMPACK manual 11.4.7 rule 5); nest the IF instead.

# IF in a levels equation and a variable in an equation condition

    x IF condition references variable "PGDP": Equation E_ifvc (all,r,REG)(all,t,ALLTIME) ifvc(r,t) = IF[pgdp(r,t) > 0, pgdp(r,t)].
    i Conditions are evaluated from coefficient values only (GEMPACK manual 11.4.6/11.4.8).

# netcut inflation warning (roadmap 6.5 E1)

    ! Multidimensional qfe(ENDW,ACTS,REG,ALLTIME) referenced with a lead or lag in E_nctest.
    i Every element of a lead/lagged variable joins the dense border (netcut) of the bordered matrix methods (SBBD/DBBD/NDBBD); each non-time dimension multiplies the border size.
    i Link periods through a minimal intertemporal proxy instead, e.g. `capital(REG,TIME) = qo("capital",REG,TIME)`, and place the lead/lag on the proxy.

# netcut proxy rewrite (roadmap 6.5 E2)

    Code
      model <- ems_model(fix_model, closure_file)
    Message
      i Inter-period links on element slices rewritten onto minimal intertemporal proxies: NCV1 = qfe("capital","crops",r,t) and NCV2 = qfe("capital","svces",r,t).
      i Proxy variables (NCV*) and their linking equations (E_NCV*) appear in solve outputs.

# partial read statement

    x Partial Read statements are not supported.

# data frame input missing a set

    x Input for SUBPAR is missing required columns.
    i Required: COMMc, REGr, ALLTIMEt, and Value.

# invalid var in closure

    x Closure variable "not_a_var" not found among the model's variables.

# closure missing exo/endo spec

    x The closure must contain both "Exogenous" and "Rest Endogenous" entries. The inverse approach is not supported.

# ems_model errors when invalid closure mixed entry present preswap

    x 3 closure entry elements in pop("zzz",ALLTIME) do not belong to the respective variable sets: 1: zzz 0, 2: zzz 1, and 3: zzz 2.

# ems_model errors when invalid closure subset entry present preswap

    x Some subsets in qe(COMM,REG,INITIME) do not belong to qe.
    x Parent sets include: ENDWMS, REG, and ALLTIME.

# ems_model errors when invalid closure pure element entry present preswap

    x The closure entry tuple pop("zzz","2") is invalid under the current set mapping.

# ems_model errors when duplicate closure entry present preswap

    x 9 tuples for "pop" in the pre-swap closure with multiple entries: 1: chn 0, 2: chn 1, 3: chn 2, 4: row 0, 5: row 1, 6: row 2, 7: usa 0, 8: usa 1, and 9: usa 2.

# ems_model errors dots passed without names

    x Coefficients to modify must be passed as named pairs: `RDLT = 1`.

# in-TAB Omit statements exogenize the omitted variables

    Code
      model <- ems_model(omit_model, closure_file)
    Message
      i In-TAB Omit statements for "atall", "avaall", and "qgdp" applied as exogenous, unshocked closure entries.
      i Omission removes a variable from the solved system in GEMPACK; TEEMS keeps it in the model and holds it fixed, so its values remain available in solve outputs.

# in-TAB Substitute executes as backsolve with a message

    Code
      model <- ems_model(sub_model, closure_file)
    Message
      i In-TAB Substitute statement for "tva" executed as backsolve.
      i Backsolved values remain available in solve outputs; plain substitution is not implemented.

# ems_model rejects a non-logical or non-scalar ignore_condense

    x `ignore_condense` must be "TRUE" or "FALSE".

---

    x `ignore_condense` must be "TRUE" or "FALSE".

# ems_model rejects invalid variable names in backsolve

    x "not_a_var" designated for backsolving not found in the model.

# ems_model rejects invalid equation names in backsolve

    x Equation "E_not_real" nominated for backsolving "qgdp" not found in the model.

# ems_model rejects unresolvable backsolve entries

    x No equation "E_pop" found to backsolve "pop".
    i Unnamed `backsolve` entries resolve their defining equation by the E_<variable> convention.
    i Name the defining equation explicitly: `backsolve = c(pop = "<equation>")`.

# ems_model rejects conflicting condensation actions

    x Variable "tva" is nominated for more than one backsolve.

# ems_model rejects a reused backsolve equation

    x Equation "E_tva" nominated for more than one backsolve.

# a backsolve equation the condensation parser cannot read aborts with the reason

    x Failed to parse E_tn into linear terms while condensing: product of two variable-bearing expressions (nonlinear).
    i Statement: Equation E_tn (all,r,REG)(all,t,ALLTIME) tvn(r,t) = pop(r,t) * pop(r,t).

---

    x Failed to parse E_tq into linear terms while condensing: unsupported equation quantifier form.
    i Statement: Equation E_tq (all,r,REG: pop(r) > 0)(all,t,ALLTIME) tvq(r,t) = pop(r,t).

# backsolve rule violations abort (GEMPACK 14.1.10)

    x Equation E_tr1 cannot be used to backsolve tvr.
    i Occurrence tvr("usa",t): an element occurs as an argument; every argument must be an index (requirement 1).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr2 cannot be used to backsolve tvr.
    i Occurrence sum{r,REG, tvr(r,t)}: a SUM index occurs as an argument; every index must be an equation ALL index (requirement 2).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr3 cannot be used to backsolve tvc3.
    i Equation ALL index (r) absent from occurrence tvc3(t); every equation ALL index must appear in each occurrence (requirement 3).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr4 cannot be used to backsolve tvm.
    i Occurrence tvm(m,t) ranges over {MARG,ALLTIME} but the variable is declared over {COMM,ALLTIME}; every index must range over the full declared set (requirement 4).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr5 cannot be used to backsolve tvrr.
    i Occurrence tvrr(r,r,t): a repeated index; all indices of one occurrence must be different (requirement 5).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr6 cannot be used to backsolve tvr.
    i Occurrence tvr(r,t+1): an argument carries a lead/lag offset; offsets block substitution in intertemporal models (requirement 6).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr7 cannot be used to backsolve tvrr.
    i Occurrences tvrr(r,s,t) and tvrr(s,r,t) have different index patterns; all occurrences must share one pattern (requirement 7).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_trc cannot be used to backsolve tvr.
    i The occurrences of the variable cancel; no expression for it can be obtained from this equation.
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

# backsolved variables must be endogenous in the closure

    x Backsolved "pop" is exogenous in the closure.
    i Substituted-out variables must be endogenous; swap out of the closure or drop the backsolve.

# swaps and shocks on condensed variables abort

    x Swap variable "tva" was condensed out of the model (backsolve).
    i Backsolved variables cannot enter the closure; drop the backsolve in `teems::ems_model()` (or load with `ignore_condense = TRUE`) to swap this variable.

---

    x Shock variable "tva" was condensed out of the model (backsolve).
    i Backsolved variables are endogenous; drop the backsolve in `teems::ems_model()` (or load with `ignore_condense = TRUE`) to shock this variable.

# ranked PostSim sets are flattened to their base set

    Code
      model <- ems_model(write_modified_model(model_file, graft), closure_file)
    Message
      i Ranked set definition(s) for "REGUP" read as the base set; the ranking by "qgdp" is not preserved.
      i Ordering of report rows is left to the composed outputs.

