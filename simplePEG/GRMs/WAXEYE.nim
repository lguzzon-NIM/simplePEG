voidPEGRule(EndOfLine):
  alternationPEG:
    sequencePEG:
      {'\13'}.terminalPEG
    do:
      {'\10'}.terminalPEG
  do:
    {'\10'}.terminalPEG


leftPEGRule(LChar):
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


leftPEGRule(Hex):
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


leftPEGRule(Char):
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
            notTerminalPEG(EndOfLine)
        do:
          anyPEG


leftPEGRule(Range):
  sequencePEG:
    alternationPEG:
      notTerminalPEG(Char)
    do:
      notTerminalPEG(Hex)
  do:
    optionalPEG:
      sequencePeg:
        voidPEG:
          "-".terminalPEG
      do:
        alternationPEG:
          notTerminalPEG(Char)
        do:
          notTerminalPEG(Hex)


voidPEGRuleForward(Ws)


leftPEGRule(CharClass):
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
          notTerminalPEG(Range)
    do:
      sequencePEG:
        voidPEG:
          "]".terminalPEG
      do:
        notTerminalPEG(Ws)


leftPEGRule(WildCard):
  sequencePEG:
    ".".terminalPEG
  do:
    notTerminalPEG(Ws)


voidPEGRule(SComment):
  sequencePEG:
    "#".terminalPEG
  do:
    sequencePEG:
      closurePEG:
        sequencePEG:
          notCheckPEG:
            notTerminalPEG(EndOfLine)
        do:
          anyPeg
    do:
      alternationPEG:
        notTerminalPEG(EndOfLine)
      do:
        notCheckPEG:
          anyPeg


voidPEGRule(MComment):
  sequencePEG:
    "/*".terminalPEG
  do:
    sequencePeg:
      closurePEG:
        alternationPEG:
          notTerminalPEG(MComment)
        do:
          sequencePEG:
            notCheckPEG:
              "*/".terminalPEG
          do:
            anyPEG
    do:
      "*/".terminalPEG


voidPEGRule(Ws):
  closurePEG:
    alternationPEG:
      {' ', '\t'}.terminalPEG
    do:
      alternationPEG:
        notTerminalPEG(EndOfLine)
      do:
        alternationPEG:
          notTerminalPEG(SComment)
        do:
          notTerminalPEG(MComment)


leftPEGRule(Identifier):
  sequencePEG:
    stringOfPEG:
      sequencePEG:
        {'a'..'z', 'A'..'Z', '_'}.terminalPEG
      do:
        closurePEG:
          {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}.terminalPEG          
  do:
    notTerminalPEG(Ws)


voidPEGRule(Alt):
  sequencePeg:
    "|".terminalPEG
  do:
    notTerminalPEG(Ws)


leftPEGRule(Literal):
  sequencePEG:
    voidPEG:
      "'".terminalPEG
  do:
    sequencePEG:
      stringOfPEG:
        plusPEG:
          sequencePEG:
            notCheckPeg:
              "'".terminalPEG
          do:
            alternationPEG:
              notTerminalPEG(LChar)
            do:
              notTerminalPEG(Hex)

    do:
      sequencePEG:
        voidPEG:
          "'".terminalPEG
      do:
        notTerminalPEG(Ws)
        

leftPEGRule(CaseLiteral):
  sequencePEG:
    voidPEG:
      "\"".terminalPEG
  do:
    sequencePEG:
      stringOfPEG:
        plusPEG:
          sequencePEG:
            notCheckPeg:
              "\"".terminalPEG
          do:
            alternationPEG:
              notTerminalPEG(LChar)
            do:
              notTerminalPEG(Hex)

    do:
      sequencePEG:
        voidPEG:
          "\"".terminalPEG
      do:
        notTerminalPEG(Ws)


voidPEGRule(Close):
  sequencePEG:
    ")".terminalPEG
  do:
    notTerminalPEG(Ws)


voidPEGRule(Open):
  sequencePEG:
    "(".terminalPEG
  do:
    notTerminalPEG(Ws)


leftPEGRule(Prefix):
  sequencePEG:
    {'?', '*', '+', ':', '&', '!', '$'}.terminalPEG
  do:
    notTerminalPEG(Ws)
    

prunePEGRuleForward(Arrow)
leftPEGRuleForward(Alternation)
  

leftPEGRule(Unit):
  sequencePEG:
    optionalPEG:
      notTerminalPEG(Prefix)
  do:
    alternationPEG:
      sequencePEG:
        notTerminalPEG(Identifier)
      do:
        notCheckPEG:
          notTerminalPEG(Arrow)
    do:
      alternationPEG:
        sequencePEG:
          notTerminalPEG(Open)
        do:
          sequencePEG:
            notTerminalPEG(Alternation)
          do:
            notTerminalPEG(Close)
      do:
        alternationPEG:
          notTerminalPEG(Literal)
        do:
          alternationPEG:
            notTerminalPEG(CaseLiteral)
          do:
            alternationPEG:
              notTerminalPEG(CharClass)
            do:
              notTerminalPEG(WildCard)
              

leftPEGRule(Sequence):
  plusPEG:
    notTerminalPEG(Unit)


leftPEGRule(Alternation):
  sequencePEG:
    notTerminalPEG(Sequence)
  do:
    closurePEG:
      sequencePEG:
        notTerminalPEG(Alt)
      do:
        notTerminalPEG(Sequence)


leftPEGRule(VoidArrow):
  sequencePEG:
    voidPEG:
      "<:".terminalPEG
  do:
    notTerminalPEG(Ws)


leftPEGRule(PruneArrow):
  sequencePEG:
    voidPEG:
      "<=".terminalPEG
  do:
    notTerminalPEG(Ws)


leftPEGRule(LeftArrow):
  sequencePEG:
    voidPEG:
      "<-".terminalPEG
  do:
    notTerminalPEG(Ws)


prunePEGRule(Arrow):
  alternationPEG:
    notTerminalPEG(LeftArrow)
  do:
    alternationPEG:
      notTerminalPEG(PruneArrow)
    do:
      notTerminalPEG(VoidArrow)


leftPEGRule(Definition):
  sequencePEG:
    notTerminalPEG(Identifier)
  do:
    sequencePEG:
      notTerminalPEG(Arrow)
    do:
      sequencePEG:
        notTerminalPEG(Alternation)
      do:
        notTerminalPEG(Ws)


leftPEGRule(WAXEYE):
  sequencePEG:
      notTerminalPEG(Ws)
  do:
    closurePEG do:
      notTerminalPEG(Definition)


