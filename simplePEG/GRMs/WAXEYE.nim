
leftDefinitionPEGForward(WAXEYE)
leftDefinitionPEGForward(Definition)
leftDefinitionPEGForward(Alternation)
leftDefinitionPEGForward(Sequence)
leftDefinitionPEGForward(Unit)
leftDefinitionPEGForward(Prefix)
leftDefinitionPEGForward(Identifier)
leftDefinitionPEGForward(Literal)
leftDefinitionPEGForward(CaseLiteral)
leftDefinitionPEGForward(LChar)
leftDefinitionPEGForward(CharClass)
leftDefinitionPEGForward(Range)
leftDefinitionPEGForward(Char)
leftDefinitionPEGForward(Hex)
leftDefinitionPEGForward(WildCard)
pruneDefinitionPEGForward(Arrow)
leftDefinitionPEGForward(LeftArrow)
leftDefinitionPEGForward(PruneArrow)
leftDefinitionPEGForward(VoidArrow)
voidDefinitionPEGForward(Alt)
voidDefinitionPEGForward(Open)
voidDefinitionPEGForward(Close)
voidDefinitionPEGForward(SComment)
voidDefinitionPEGForward(MComment)
voidDefinitionPEGForward(EndOfLine)
voidDefinitionPEGForward(Ws)



WAXEYE.leftDefinitionPEG:
  sequencePEG:
    Ws.notTerminalPEG
  do:
    closurePEG:
      Definition.notTerminalPEG


Definition.leftDefinitionPEG:
  sequencePEG:
    Identifier.notTerminalPEG
  do:
    sequencePEG:
      Arrow.notTerminalPEG
    do:
      sequencePEG:
        Alternation.notTerminalPEG
      do:
        Ws.notTerminalPEG


Alternation.leftDefinitionPEG:
  sequencePEG:
    Sequence.notTerminalPEG
  do:
    closurePEG:
      sequencePEG:
        Alt.notTerminalPEG
      do:
        Sequence.notTerminalPEG


Sequence.leftDefinitionPEG:
  plusPEG:
    Unit.notTerminalPEG


Unit.leftDefinitionPEG:
  sequencePEG:
    optionalPEG:
      Prefix.notTerminalPEG
  do:
    alternationPEG:
      sequencePEG:
        Identifier.notTerminalPEG
      do:
        notCheckPEG:
          Arrow.notTerminalPEG
    do:
      alternationPEG:
        sequencePEG:
          Open.notTerminalPEG
        do:
          sequencePEG:
            Alternation.notTerminalPEG
          do:
            Close.notTerminalPEG
      do:
        alternationPEG:
          Literal.notTerminalPEG
        do:
          alternationPEG:
            CaseLiteral.notTerminalPEG
          do:
            alternationPEG:
              CharClass.notTerminalPEG
            do:
              WildCard.notTerminalPEG


Prefix.leftDefinitionPEG:
  sequencePEG:
    {'?', '*', '+', ':', '&', '!', '$'}.terminalPEG
  do:
    Ws.notTerminalPEG


Identifier.leftDefinitionPEG:
  sequencePEG:
    stringOfPEG:
      sequencePEG:
        {'a'..'z', 'A'..'Z', '_'}.terminalPEG
      do:
        closurePEG:
          {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}.terminalPEG
  do:
    Ws.notTerminalPEG


Literal.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      {'\''}.terminalPEG
  do:
    sequencePEG:
      stringOfPEG:
        plusPEG:
          sequencePEG:
            notCheckPEG:
              {'\''}.terminalPEG
          do:
            alternationPEG:
              LChar.notTerminalPEG
            do:
              Hex.notTerminalPEG
    do:
      sequencePEG:
        voidPEG:
          {'\''}.terminalPEG
      do:
        Ws.notTerminalPEG


CaseLiteral.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      {'"'}.terminalPEG
  do:
    sequencePEG:
      stringOfPEG:
        plusPEG:
          sequencePEG:
            notCheckPEG:
              {'"'}.terminalPEG
          do:
            alternationPEG:
              LChar.notTerminalPEG
            do:
              Hex.notTerminalPEG
    do:
      sequencePEG:
        voidPEG:
          {'"'}.terminalPEG
      do:
        Ws.notTerminalPEG


LChar.leftDefinitionPEG:
  alternationPEG:
    sequencePEG:
      "\\".terminalPEG
    do:
      {'n', 'r', 't', '\'', '"', '\\'}.terminalPEG
  do:
    sequencePEG:
      notCheckPEG:
        "\\".terminalPEG
    do:
      sequencePEG:
        notCheckPEG:
          EndOfLine.notTerminalPEG
      do:
        anyPEG


CharClass.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      "[".terminalPEG
  do:
    sequencePEG:
      closurePEG:
        sequencePEG:
          notCheckPEG:
            "]".terminalPEG
        do:
          Range.notTerminalPEG
    do:
      sequencePEG:
        voidPEG:
          "]".terminalPEG
      do:
        Ws.notTerminalPEG


Range.leftDefinitionPEG:
  sequencePEG:
    alternationPEG:
      Char.notTerminalPEG
    do:
      Hex.notTerminalPEG
  do:
    optionalPEG:
      sequencePEG:
        voidPEG:
          "-".terminalPEG
      do:
        alternationPEG:
          Char.notTerminalPEG
        do:
          Hex.notTerminalPEG


Char.leftDefinitionPEG:
  alternationPEG:
    sequencePEG:
      "\\".terminalPEG
    do:
      {'n', 'r', 't', '-', ']', '\\'}.terminalPEG
  do:
    sequencePEG:
      notCheckPEG:
        "\\".terminalPEG
    do:
      sequencePEG:
        notCheckPEG:
          "]".terminalPEG
      do:
        sequencePEG:
          notCheckPEG:
            EndOfLine.notTerminalPEG
        do:
          anyPEG


Hex.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      "\\<".terminalPEG
  do:
    sequencePEG:
      {'0'..'9', 'A'..'F', 'a'..'f'}.terminalPEG
    do:
      sequencePEG:
        {'0'..'9', 'A'..'F', 'a'..'f'}.terminalPEG
      do:
        voidPEG:
          ">".terminalPEG


WildCard.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      ".".terminalPEG
  do:
    Ws.notTerminalPEG


Arrow.pruneDefinitionPEG:
  sequencePEG:
    voidPEG:
      "<".terminalPEG
  do:
    alternationPEG:
      LeftArrow.notTerminalPEG
    do:
      alternationPEG:
        PruneArrow.notTerminalPEG
      do:
        VoidArrow.notTerminalPEG


LeftArrow.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      "-".terminalPEG
  do:
    Ws.notTerminalPEG


PruneArrow.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      "=".terminalPEG
  do:
    Ws.notTerminalPEG


VoidArrow.leftDefinitionPEG:
  sequencePEG:
    voidPEG:
      ":".terminalPEG
  do:
    Ws.notTerminalPEG


Alt.voidDefinitionPEG:
  sequencePEG:
    "|".terminalPEG
  do:
    Ws.notTerminalPEG


Open.voidDefinitionPEG:
  sequencePEG:
    "(".terminalPEG
  do:
    Ws.notTerminalPEG


Close.voidDefinitionPEG:
  sequencePEG:
    ")".terminalPEG
  do:
    Ws.notTerminalPEG


SComment.voidDefinitionPEG:
  sequencePEG:
    "#".terminalPEG
  do:
    sequencePEG:
      closurePEG:
        sequencePEG:
          notCheckPEG:
            EndOfLine.notTerminalPEG
        do:
          anyPEG
    do:
      alternationPEG:
        EndOfLine.notTerminalPEG
      do:
        notCheckPEG:
          anyPEG


MComment.voidDefinitionPEG:
  sequencePEG:
    "/*".terminalPEG
  do:
    sequencePEG:
      closurePEG:
        alternationPEG:
          MComment.notTerminalPEG
        do:
          sequencePEG:
            notCheckPEG:
              "*/".terminalPEG
          do:
            anyPEG
    do:
      "*/".terminalPEG


EndOfLine.voidDefinitionPEG:
  alternationPEG:
    sequencePEG:
      "\x0D".terminalPEG
    do:
      optionalPEG:
        "\x0A".terminalPEG
  do:
    "\x0A".terminalPEG


Ws.voidDefinitionPEG:
  closurePEG:
    alternationPEG:
      voidPEG:
        {' ', '\t'}.terminalPEG
    do:
      alternationPEG:
        EndOfLine.notTerminalPEG
      do:
        alternationPEG:
          SComment.notTerminalPEG
        do:
          MComment.notTerminalPEG
