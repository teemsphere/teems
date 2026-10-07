# corpus fixtures abort with their named messages

    Code
      writeLines(paste0(fixtures, ": ", msgs))
    Output
      bound_dup.tab: x Duplicate lower bound in declaration: "Coefficient (ge 0, ge 1) (all,r,REG) BNDP(r) # duplicate lower bound #" i One lower (`ge`/`gt`) and one upper (`le`/`lt`) bound are allowed per declaration (GEMPACK manual 10.19.1).
      default_coef_bound.tab: x Coefficient bound defaults are not supported (GEMPACK manual 10.19): "Coefficient (default=lower_bound ge 0)"
      default_eq_homotopy.tab: x Unknown Equation default "add_homotopy=" (GEMPACK manual 10.19): "Equation (default=add_homotopy=)"
      formula_and_equation.tab: x Malformed `Formula & Equation` statement: expected `Formula [(initial)] & Equation [(levels)] name [quantifiers] lhs = rhs` (GEMPACK manual 10.9.1): "Formula & Equation E_ppl # malformed: no equals # (all,r,REG) PPL(r)"
      formula_no_equals.tab: x Formula statement without `=`: "Formula NOEQ 1" i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Formula continuation.
      index_not_subset.tab: x Index "a" of `qo(a,r)` in Equation E_qlnd ranges over set "LANDACTS", which is not "ACTS" (the declared set at that argument position) or a declared subset of it. i GEMPACK requires the relation to be declared (manual 10.2): add `Subset LANDACTS is subset of ACTS;`. Without it the solver would address the wrong elements of "ACTS".
      mapping_malformed.tab: x Malformed Mapping statement: "Mapping REGTOBLOC of REG onto BLOC" i Expected `Mapping [(onto)] <name> from <set> to <set>;` (GEMPACK manual 10.13).
      mapping_no_read.tab: x Mapping "regtobloc" has no `Read` or `Formula` assigning its values (GEMPACK manual 11.9.1).
      mapping_undeclared_set.tab: x Set "BLOC" in the Mapping declaration of "REGTOBLOC" is not declared in the model.
      name_c_prefix.tab: x Coefficient "c_pop" has the name of the linear variable of a levels variable (`c_X` for a change, `p_X` for a percentage-change levels variable `X`; GEMPACK manual 9.2.2); rename the coefficient.
      name_coef_set_clash.tab: x Name declared as both a coefficient and a set: "reg". i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).
      name_coef_var_clash.tab: x Name declared as both a coefficient and a variable: "pop". i TABLO names are case-insensitive and must be unique (GEMPACK manual 11.2.1).
      name_dup.tab: x Duplicate coefficient declaration: "dupx" (GEMPACK manual 11.2.1).
      name_overlength.tab: x Declaration name longer than 255 characters: "QQQQQQQQQQQQQQQQQQQQ...".
      name_reserved.tab: x Declaration name "max" is a reserved word (GEMPACK manual 11.2.1).
      postsim_scope.tab: x Ordinary statement references PostSim-declared name: "psx". i PostSim declarations are only visible inside PostSim sections (GEMPACK manual 12.2.1).
      postsim_unbalanced.tab: x Unbalanced PostSim section markers: 1 `PostSim (Begin)` against 0 `PostSim (End)` (GEMPACK manual 12.2).
      qual_empty.tab: x Empty qualifier `()` in declaration: "Variable () dummyv # empty qualifier #"
      qual_unknown.tab: x Unknown declaration qualifier: "fob". i See GEMPACK manual 10.3/10.4 for the recognized variable and coefficient qualifiers.
      read_no_header.tab: x Read without a header is not supported (GEMPACK manual 11.11.8): "Read ELX from file GTAPDATA"
      read_terminal.tab: x Read from terminal is not supported; read from a file instead: "Read SCLR from terminal"
      read_undeclared.tab: x Read target "notdecl" not declared as a coefficient.
      set_ele_empty.tab: x Malformed element list for set SEL: "(x1,)" contains empty elements.
      set_ele_range.tab: x Element range "s5 - s1" in set SRG cannot be expanded: it runs from a larger number to a smaller one. i A range names two elements with the same stem and a number at the end, `grain1 - grain4` or `ind008 - ind112` (GEMPACK manual 11.2.2).
      set_header_len.tab: x Header longer than 4 characters in the declaration of set REG: "TOOLONG".
      set_self_eq.tab: x Set SSE is defined as equal to itself (GEMPACK manual 10.1.2.1).
      set_self_ref.tab: x Set BADS references itself in its defining expression: "BADS + COMM". i Define a set from other sets and quoted elements only (GEMPACK manual 10.1.1.1).
      set_undeclared.tab: x Set referenced before declaration in "Set NMRG = COMM - MRG": "MRG". i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).
      stmt_unknown_keyword.tab: x Equation statement without `=`: "Equation Frobnicate all the things" i Either the statement is malformed or its leading token is an unrecognized keyword that was read as an implicit Equation continuation.
      subset_undeclared.tab: x Set referenced before declaration in "Subset COMM is subset of KOMM": "KOMM". i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

