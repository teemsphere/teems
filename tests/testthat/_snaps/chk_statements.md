# unclosed strong comment aborts (A5)

    x Strong comment `![[!` opened on line 5545 is never closed.
    i Strong comments nest: every `![[!` needs its own `!]]!`; everything between the outermost pair is ignored.

# stray label text outside a statement aborts

    x Statement starting with label text: "# stray label # Coefficient (all,r,REG) SAVE(r) # NET saving in region r valued "
    i A `# label #` outside any statement (usually a label placed after the terminating `;`) is not valid TABLO.

