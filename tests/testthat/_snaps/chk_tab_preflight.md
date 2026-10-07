# name collisions abort

    x Name declared as both a coefficient and a variable: "psave".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

---

    x Name declared as both a coefficient and a set: "reg".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

---

    x Name declared as both a variable and a set: "reg".
    i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).

# duplicate declarations abort

    x Duplicate coefficient declaration: "dupx" (GEMPACK manual 11.2.1).

# reserved words abort

    x Declaration name "max" is a reserved word (GEMPACK manual 11.2.1).

# coefficients named for a levels variable's linear variable abort

    x Coefficient "c_LCH" has the name of the linear variable of a levels variable (`c_X` for a change, `p_X` for a percentage-change levels variable `X`; GEMPACK manual 9.2.2); rename the coefficient.

---

    x Coefficient "p_LPC" has the name of the linear variable of a levels variable (`c_X` for a change, `p_X` for a percentage-change levels variable `X`; GEMPACK manual 9.2.2); rename the coefficient.

# variables named for a levels variable's linear variable abort

    x Variable "p_LPV" has the name of the linear variable of a levels variable (`c_X` for a change, `p_X` for a percentage-change levels variable `X`; GEMPACK manual 9.2.2); rename the variable.

# over-length names abort

    x Declaration name longer than 255 characters: "AAAAAAAAAAAAAAAAAAAA...".

# unknown qualifiers abort

    x Unknown declaration qualifier: "foo".
    i See GEMPACK manual 10.3/10.4 for the recognized variable and coefficient qualifiers.

# no_split qualifier aborts

    x The variable qualifier `no_split` (full shock at every step) is not supported: "Variable (no_split) dummyvar"

# empty qualifiers abort

    x Empty qualifier `()` in declaration: "Variable () dummyvar"

# duplicate bounds abort

    x Duplicate lower bound in declaration: "Coefficient (ge 0, ge 1) (all,r,REG) BNDP(r)"
    i One lower (`ge`/`gt`) and one upper (`le`/`lt`) bound are allowed per declaration (GEMPACK manual 10.19.1).

# invalid Default statements abort

    x Coefficient bound defaults are not supported (GEMPACK manual 10.19): "Coefficient (default=lower_bound ge 0)"

---

    x Unknown Variable default "foo" (GEMPACK manual 10.19): "Variable (default=foo)"

---

    x Default statements apply only to Coefficient, Variable, Formula, and Equation declarations (GEMPACK manual 10.19): "Update (default=always)"

---

    x Unknown Equation default "add_homotopy=" (GEMPACK manual 10.19): "Equation (default=add_homotopy=)"

# self-referential set expressions abort

    x Set SBAD references itself in its defining expression: "SBAD + COMM".
    i Define a set from other sets and quoted elements only (GEMPACK manual 10.1.1.1).

# undeclared set references abort

    x Set referenced before declaration in "Set SND = COMM - MRGX": "MRGX".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

---

    x Set referenced before declaration in "Set SEQ = NOPE": "NOPE".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

---

    x Set referenced before declaration in "Subset REG is subset of NOPE2": "NOPE2".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

# set self-equality aborts

    x Set SSE is defined as equal to itself (GEMPACK manual 10.1.2.1).

# malformed element ranges abort

    x Element range "s1 - t5" in set SRG cannot be expanded: the two ends do not share a stem followed by a number.
    i A range names two elements with the same stem and a number at the end, `grain1 - grain4` or `ind008 - ind112` (GEMPACK manual 11.2.2).

---

    x Element range "ind01 - ind123" in set SRG cannot be expanded: a zero-padded range needs the same number of digits at both ends.
    i A range names two elements with the same stem and a number at the end, `grain1 - grain4` or `ind008 - ind112` (GEMPACK manual 11.2.2).

---

    x Element range "s5 - s1" in set SRG cannot be expanded: it runs from a larger number to a smaller one.
    i A range names two elements with the same stem and a number at the end, `grain1 - grain4` or `ind008 - ind112` (GEMPACK manual 11.2.2).

---

    x Element range "s1 - s2 - s3" in set SRG cannot be expanded: it is not two element names joined by one dash.
    i A range names two elements with the same stem and a number at the end, `grain1 - grain4` or `ind008 - ind112` (GEMPACK manual 11.2.2).

# malformed element lists abort

    x Malformed element list for set SEL: "()" contains empty elements.

---

    x Malformed element list for set SEL2: "(x1,)" contains empty elements.

# over-length set headers abort

    x Header longer than 4 characters in the declaration of set SHD: "TOOBIG".

# math statements without = abort

    x Formula statement without `=`: "Formula NOEQ 1"
    i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Formula continuation.

---

    x Formula statement without `=`: "Formula Frobnicate all the things"
    i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Formula continuation.

# headerless reads abort

    x Read without a header is not supported (GEMPACK manual 11.11.8): "Read ELX from file GTAPDATA"

# reads into undeclared names abort

    x Read target "notdecl" not declared as a coefficient.

# read from terminal aborts

    x Read from terminal is not supported; read from a file instead: "Read ELX from terminal"

# PostSim scope violations abort

    x Ordinary statement references PostSim-declared name: "pscalc".
    i PostSim declarations are only visible inside PostSim sections (GEMPACK manual 12.2.1).

# PostSim reads from ordinary files abort

    x File "gtapdata" read in both the ordinary and PostSim parts.
    i Split the data across two files (GEMPACK manual 12.2.3).

# PostSim reads into ordinary coefficients abort

    x PostSim Read into ordinary coefficient "save"; targets must be PostSim coefficients (GEMPACK manual 12.2.3).

# PostSim reads into variables abort

    x PostSim Read into variable "psave"; simulation results cannot be changed (GEMPACK manual 12.2.3).

# PostSim reads into undeclared names abort

    x PostSim Read target "notdecl" not declared (GEMPACK manual 12.2.3).

# PostSim formulas assigning variables abort

    x PostSim Formula assigns variable "psave"; simulation results cannot be changed (GEMPACK manual 12.2.2).

# PostSim formulas assigning ordinary coefficients abort

    x PostSim Formula assigns ordinary coefficient "save"; the LHS must be a PostSim coefficient (GEMPACK manual 12.2.2).

# malformed quantifiers, sums and zerodivide defaults abort

    x Malformed quantifier "(all,r)" in: "Variable (all,r) qbad(r)"
    i A quantifier is `(all,<index>,<set>)`; the index and the set are both required (GEMPACK manual 11.3).

---

    x Malformed quantifier "(all,REG)" in: "Equation E_qbad2 (all,REG) qgdp(REG) = 0"
    i A quantifier is `(all,<index>,<set>)`; the index and the set are both required (GEMPACK manual 11.3).

---

    x Sum with an empty index in: "Formula (all,r,REG) CBAD(r) = sum(,REG, vgdp(r))"
    i Write `sum(<index>,<set>, <expression>)`.

---

    x 11 dimensions in declaration (the solver holds at most 10): "Variable (all,d1,REG)(all,d2,REG)(all,d3,REG)(all,d4,REG)(all,d5,REG)(all,d6,REG)(all,d7,REG)(all,d8,REG)(all,d9,REG)(all,d10,REG)(all,d11,REG) vbig(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11)"

---

    x Zerodivide default "NOSUCHCOEF" is neither a number nor a declared coefficient: "Zerodivide (nonzero_by_zero) default NOSUCHCOEF"

---

    x Reference "vgdp( )" has an empty index in: "Formula (all,r,REG) CBAD2(r) = vgdp( )"
    i A reference carries exactly the declared indices, `NAME(<index>, ...)` (GEMPACK manual 10.3, 11.4.10).

---

    x Reference "vgdp(r,,)" has an empty index in: "Formula (all,r,REG) CBAD3(r) = vgdp(r,,)"
    i A reference carries exactly the declared indices, `NAME(<index>, ...)` (GEMPACK manual 10.3, 11.4.10).

# statements over the solver statement buffer abort

    x Statement "Formula (all,r,REG) CLONG(r) = vgdp(r)+vgdp(r)+vgdp(r)+vgdp(..." needs about 20858 characters in the solver, over its 20000-character statement limit.
    i Split it into shorter statements, for example through intermediate coefficients or variables. Equation and Update statements count 2 extra characters per variable reference.

---

    x Statement "Equation E_long (all,r,REG) qgdp(r) = 0+qgdp(r)+qgdp(r)+qgdp..." needs about 21061 characters in the solver, over its 20000-character statement limit.
    i Split it into shorter statements, for example through intermediate coefficients or variables. Equation and Update statements count 2 extra characters per variable reference.

# unbalanced parentheses abort

    x Unbalanced parentheses in: "Formula (all,r,REG) CUNB(r) = sum(c,COMM, VDFP(c,\"food\",r)"
    i Every `(` needs a matching `)`; an unclosed `sum(` is the common case.

---

    x Unbalanced parentheses in: "Formula (all,r,REG) CUNB2(r) = (vgdp(r)+1))"
    i Every `(` needs a matching `)`; an unclosed `sum(` is the common case.

# subsets by numbers abort

    x `Subset (by_numbers)` is obsolete and not supported; list the subset's elements by name (GEMPACK manual 10.2).

# a qualifier list that never closes aborts

    x Unbalanced parentheses in the qualifier list of: "Coefficient (parameter QUNB"

# intertemporal set builders abort

    x Set builder "BIGR" is intertemporal; builders are supported for static sets only.

# a mapping-sum set builder over a non-mapping aborts

    x Set builder "COMMZ" sums over "NOMAP", which is not a Mapping with a `(by_elements)` Read.
    i The mapping-conditional sum form needs a file-Read mapping and a file-Read summed coefficient (GEMPACK manual 10.1.3).

# a binary switch in a set definition aborts

    x Unsupported binary switch detected in a Set definition.
    i A set selected by a condition takes the builder form (GEMPACK manual 10.1.3), e.g. Set ENDWM # mobile endowments # = (all,e,ENDW: ENDOWFLAG(e,"mobile") ne 0);.
    i Otherwise declare its elements explicitly, e.g. Set ENDWM # mobile endowments # (capital,unsklab,sklab);.

