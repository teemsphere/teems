# unclosed strong comment aborts (A5)

    x Strong comment `![[!` opened on line 5541 is never closed.
    i Strong comments nest: every `![[!` needs its own `!]]!`; everything between the outermost pair is ignored.

# stray label text outside a statement aborts

    x Statement starting with label text: "# stray label # Coefficient (all,r,REG) SAVE(r) # NET saving in region r valued "
    i A `# label #` outside any statement (usually a label placed after the terminating `;`) is not valid TABLO.

# a misspelt keyword after a declaration aborts; a first statement without a keyword aborts by name (R1-B6)

    x Statement "Coeficient B" has no TABLO keyword and is not a continuation of the preceding Coefficient statement.
    i A statement without a keyword continues the previous statement type (GEMPACK manual 4.2); a misspelt keyword reads as two names.

---

    x Unsupported Tablo declarations detected: Foo.

# a partial Read of a non-mapping aborts and a quantified full Read passes (R1-B7)

    x Partial Read statement is not supported: "Read (all,r,REG) V(r,\"usa\") from file F header \"VV\"".

---

    x Partial Read statement is not supported: "Read (all,r,SUBREG) V(r) from file F header \"VV\"".

# a Read whose file name contains header or from file parses (R1b-B7)

    x Read statement not of the form `Read <coefficient> from file <file> header "<header>"`: "Read AH from file MYHEADERS".

