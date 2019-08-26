
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
leftDefinitionPEGForward(Ws)



leftDefinitionPEG(WAXEYE):
  sequencePEG:
    Ws.notTerminalPEG
  do:
    closurePEG:
      Definition.notTerminalPEG


leftDefinitionPEG(Definition):
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


leftDefinitionPEG(Alternation):
  sequencePEG:
    Sequence.notTerminalPEG
  do:
    closurePEG:
      sequencePEG:
        Alt.notTerminalPEG
      do:
        Sequence.notTerminalPEG


leftDefinitionPEG(Sequence):
  plusPEG:
    Unit.notTerminalPEG


leftDefinitionPEG(Unit):
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


leftDefinitionPEG(Prefix):
  sequencePEG:
    {'?', '*', '+', ':', '&', '!', '$'}.terminalPEG
  do:
    Ws.notTerminalPEG


leftDefinitionPEG(Identifier):
  sequencePEG:
    stringOfPEG:
      sequencePEG:
        {'a'..'z', 'A'..'Z', '_'}.terminalPEG
      do:
        closurePEG:
          {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}.terminalPEG
  do:
    Ws.notTerminalPEG


leftDefinitionPEG(Literal):
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


leftDefinitionPEG(CaseLiteral):
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


leftDefinitionPEG(LChar):
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


leftDefinitionPEG(CharClass):
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


leftDefinitionPEG(Range):
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


leftDefinitionPEG(Char):
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


leftDefinitionPEG(Hex):
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


leftDefinitionPEG(WildCard):
  sequencePEG:
    voidPEG:
      ".".terminalPEG
  do:
    Ws.notTerminalPEG


pruneDefinitionPEG(Arrow):
  alternationPEG:
    LeftArrow.notTerminalPEG
  do:
    alternationPEG:
      PruneArrow.notTerminalPEG
    do:
      VoidArrow.notTerminalPEG


leftDefinitionPEG(LeftArrow):
  sequencePEG:
    voidPEG:
      "<-".terminalPEG
  do:
    Ws.notTerminalPEG


leftDefinitionPEG(PruneArrow):
  sequencePEG:
    voidPEG:
      "<=".terminalPEG
  do:
    Ws.notTerminalPEG


leftDefinitionPEG(VoidArrow):
  sequencePEG:
    voidPEG:
      "<:".terminalPEG
  do:
    Ws.notTerminalPEG


voidDefinitionPEG(Alt):
  sequencePEG:
    "|".terminalPEG
  do:
    Ws.notTerminalPEG


voidDefinitionPEG(Open):
  sequencePEG:
    "(".terminalPEG
  do:
    Ws.notTerminalPEG


voidDefinitionPEG(Close):
  sequencePEG:
    ")".terminalPEG
  do:
    Ws.notTerminalPEG


voidDefinitionPEG(SComment):
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


voidDefinitionPEG(MComment):
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


voidDefinitionPEG(EndOfLine):
  alternationPEG:
    sequencePEG:
      "\x0D".terminalPEG
    do:
      optionalPEG:
        "\x0A".terminalPEG
  do:
    "\x0A".terminalPEG


leftDefinitionPEG(Ws):
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
