
import streams
import strutils

import SimpleAST

import simplePEG.typeEx
import simplePEG.strProcs
import simplePEG.consts


type
  SimplePEGIndex = int
  SimplePEGLength = int
  
  SimplePEGSliceObject = object {. final .}
    FStream: Stream
    FIndex: SimplePEGIndex
    FLength: SimplePEGLength
    FAsString: string

  SimplePEGNodeObjectSeq = seq[SimplePEGNodeObject]
  SimplePEGNodeObject = object
    case FIsTerminal: bool
    of true:
      FSlice: SimplePEGSliceObject
    else:
      FName: string
      FItems: SimplePEGNodeObjectSeq

  SimplePEGSliceObjectOption = Maybe[SimplePEGSliceObject]
  SimplePEGNodeObjectOption = Maybe[SimplePEGNodeObject]
  SimplePEGNode = SimplePEGNodeObject | SimplePEGNodeObjectOption
  SimpleNodeBool = bool | SimplePEGNodeObjectOption


proc asSimpleASTNode* (aSelf: SimplePEGNode): SimpleASTNodeRef =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if not aSelf.hasValue:
      return nil
    let lSelf = aSelf.value      
  if lSelf.FIsTerminal:
    result = newSimpleASTNode(lSelf.FSlice.asString)
  else:
    if lSelf.FName.startsWith("$") and (lSelf.FItems.len > 0):
      result = newSimpleASTNode(lSelf.FName.split("$", 1)[1])
    else:
      result = newSimpleASTNode(lSelf.FName)
      if (not result.isNil) and (not lSelf.FItems.isNil):
        for lItem in lSelf.FItems:
          let lChild = lItem.asSimpleASTNode
          if not lChild.isNil:
            discard result.addChild(lChild)
          else:
            return nil


proc asString* (aSimplePEGSlice: SimplePEGSliceObject): string =
  result = aSimplePEGSlice.FAsString
  if result != "":
    let lStream = aSimplePEGSlice.FStream
    let lOldPosition = lStream.getPosition
    defer:
      lStream.setPosition(lOldPosition)
    lStream.setPosition(aSimplePEGSlice.FIndex)
    let lLength = aSimplePEGSlice.FLength
    let lString = lStream.readstr(lLength)
    if (lLength == lString.len):
      result = lString
  

proc `$`(aSimplePEGNodeObject: SimplePEGNodeObject): string =
  
  proc innerEcho(aValue: SimplePEGNodeObject, aTabs: int): string =
    if aValue.FIsTerminal:
      result = ("""
      
Is Terminal
  Slice -> $1
  Slice AsString -> $2
      """ % [$aValue.FSlice, aValue.FSlice.asString()]).indent(aTabs)
    else:
      result = ("""
      
Is Not Terminal
  Name -> $1
  Items -> $2
      """ % [$aValue.Fname, $aValue.FItems.len]).indent(aTabs)
      let lTabs = aTabs + 1
      for lItem in aValue.FItems:
        result &= innerEcho(lItem, lTabs)
  
  innerEcho(aSimplePEGNodeObject, 1)


proc startIndex(aSelf: SimplePEGNodeObject): int =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if not aSelf.hasValue:
      return -1
    let lSelf = aSelf.value      
  if lSelf.FIsTerminal:
    result = lSelf.FSlice.FIndex
  else:
    if lSelf.FItems.isNil:
      result = -1
    else:
      result = startIndex(lSelf.FItems[0])


proc endIndex(aSelf: SimplePEGNode): int =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if not aSelf.hasValue:
      return -1
    let lSelf = aSelf.value      
  if lSelf.FIsTerminal:
    result = lSelf.FSlice.FIndex + lSelf.FSlice.FLength - 1
  else:
    if lSelf.FItems.isNil:
      result = -1
    else:
      result = endIndex(lSelf.FItems[<lSelf.FItems.len])


proc valuePEG (aSelf: SimplePEGNode): string =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if not aSelf.hasValue:
      return ""
    let lSelf = aSelf.value      
  if lSelf.FIsTerminal:
    result = lSelf.FSlice.asString
  else:
    if not lSelf.FItems.isNil:
      if lSelf.FItems.len > 0:
        result = ""
        for lItem in lSelf.FItems:
          result &= lItem.valuePEG
      else:
        result = lSelf.FName
    else:
      result = lSelf.FName


template withStream (aString: string; aStream, aBody: untyped) =
  block withStream:
    let aStream {. inject .} : Stream = newStringStream(aString)
    if not aStream.isNil:
      block withStream_Body:
        aBody


proc read* (aStream: Stream, aLength: SimplePEGLength, aUsePeek: static[bool]): SimplePEGSliceObjectOption =
  if (1>aLength) or aStream.isNil:
    result = Nothing[SimplePEGSliceObject]()
  else:
    try:
      let lBeginIndex = aStream.getPosition
      when aUsePeek:
        defer:
          aStream.setPosition(lBeginIndex)
      try:
        let lEndPosition = lBeginIndex + aLength
        aStream.setPosition(lEndPosition)
        if lEndPosition == aStream.getPosition:
          result = Just(SimplePEGSliceObject(FStream: aStream, FIndex: lBeginIndex, FLength: aLength))
        else:
          result = Nothing[SimplePEGSliceObject]()
      except:
        result = Nothing[SimplePEGSliceObject]()
    except:
      result = Nothing[SimplePEGSliceObject]()


proc charInChars* (aStream: Stream, aChars: string | set[char], aUsePeek: static[bool], aSaveAsString: static[bool]): SimplePEGSliceObjectOption =
  var lRead = aStream.read(sizeof(char), aUsePeek)
  if lRead.hasValue:
    let lValue = lRead.value
    let lAsString = lValue.asString
    if lAsString[0] in aChars:
      when aSaveAsString:
        lValue.FAsString =  lAsString
      result = Just(lValue)
    else:
      result = Nothing[SimplePEGSliceObject]()
  else:
    result = Nothing[SimplePEGSliceObject]()


template charInChars* (aStream: Stream, aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, false, false)


template charInCharsPeek* (aStream: Stream, aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, true, false)


template charInCharsAsString* (aStream: Stream, aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, false, true)


template charInCharsPeekAsString* (aStream: Stream, aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, true, true)


proc stringInString* (aStream: Stream, aString: string, aCasesInsensitive: static[bool], aUsePeek: static[bool], aSaveAsString: static[bool]): SimplePEGSliceObjectOption =
  let lLength = aString.len
  var lRead = aStream.read(lLength, aUsePeek)
  if lRead.hasValue:
    let lAsString = lRead.value.asString
    if equalEx(lAsString, aString, aCasesInsensitive):
      when aSaveAsString:
        lRead.value.FAsString = lAsString
      result = Just(lRead.value)
    else:
      result = Nothing[SimplePEGSliceObject]()
  else:
    result = lRead


template stringInString* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, false, false)


template stringInStringNoCase* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, true, false, false)


template stringInStringPeekCase* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, true, false)


template stringInStringPeekNoCase* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, aString, true, true, false)


template stringInStringAsString* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, false, true)


template stringInStringNoCaseAsString* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, true, false, true)


template stringInStringPeekCaseAsString* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, true, true)


template stringInStringPeekNoCaseAsString* (aStream: Stream, aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, aString, true, true, true)


template innerPEGRuleForward(aRuleName, aParamName: untyped, atype: typedesc) =
  proc aRuleName* (aParamName: Stream): atype {. gcsafe .} #Compiler her does not "inject" aParamName correctly


template innerPEGRule(aRuleName, aParamName: untyped, atype: typedesc, aBody: untyped) =
  proc aRuleName* (aParamName: Stream): atype {. gcsafe .} = #Compiler her does not "inject" aParamName correctly
    aBody


template lResultIsTrue(aResult: SimpleNodeBool): untyped =
  when aResult is bool:
    aResult
  else:
    aResult.hasValue


template leftPEGRuleForward(aRuleName: untyped): untyped =
  innerPEGRuleForward(aRuleName, aStream, SimplePEGNodeObjectOption)
  

template leftPEGRule(aRuleName: untyped, aBody: untyped): untyped =
  innerPEGRule(aRuleName, aStream, SimplePEGNodeObjectOption):
    block leftPEGRule:
      var lStack {. inject .} = newSeqOfCap[SimplePEGNodeObject](8)
      var lResult {. inject .} = Nothing[SimplePEGNodeObject]()
      aBody
      if lResultIsTrue(lResult):
        result = Just(SimplePEGNodeObject(FIsTerminal: false, FName: astToStr(aRuleName), FItems: lStack))
      else:
        result = Nothing[SimplePEGNodeObject]()
  

template voidPEGRuleForward(aRuleName: untyped): untyped =
  innerPEGRuleForward(aRuleName, aStream, bool)
  

template voidPEGRule(aRuleName: untyped, aBody: untyped): untyped =
  innerPEGRule(aRuleName, aStream, bool):
    block voidPEGRule:
      var lResult {. inject .} = false
      aBody
      result = lResult
  

template prunePEGRuleForward(aRuleName: untyped): untyped =
  innerPEGRuleForward(aRuleName, aStream, SimplePEGNodeObjectOption)

template prunePEGRule(aRuleName: untyped, aBody: untyped): untyped =
  innerPEGRule(aRuleName, aStream, SimplePEGNodeObjectOption):
    block prunePEGRule:
      var lStack {. inject .} = newSeqOfCap[SimplePEGNodeObject](8)
      var lResult {. inject .} = Nothing[SimplePEGNodeObject]()
      aBody
      if lResultIsTrue(lResult):
        if 1 == lStack.len:
          result = Just(lStack[0])
        else:
          result = Just(SimplePEGNodeObject(FIsTerminal: false, FName: astToStr(aRuleName), FItems: lStack))
      else:
        result = Nothing[SimplePEGNodeObject]()
  

template anyPEG: untyped =
  block anyPEG:
    let lAny = aStream.read(1, false)
    when lResult is bool:
      lResult = lAny.hasValue
    else:
      if lAny.hasValue:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: true, FSlice: lAny.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


template pushStreamStatePEG = 
  {. push warning[ShadowIdent]: off .}
  let lPosition {. inject .} = aStream.getPosition
  {. pop .}
  

template popStreamStatePEG =
  aStream.setPosition(lPosition)


template pushStackStatePEG = 
  when not (lResult is bool):
    {. push warning[ShadowIdent]: off .}
    let lStackLen {. inject .} = lStack.len
    {. pop .}
  

template popStackStatePEG =
  when not (lResult is bool):
    lStack.setLen(lStackLen)


template pushPEG = 
  pushStreamStatePEG
  pushStackStatePEG


template popPEG = 
  popStackStatePEG
  popStreamStatePEG


template showStackStatePEG = 
  when not (lResult is bool):
    echo "\\/".repeat(40) & $lStack.len
    for lItem in lStack:
      echo $lItem
    echo "/\\".repeat(40) & $lStack.len


template zeroOrMorePEG(aBody: untyped): untyped =
  block zeroOrMorePEG:
    while true:
      pushPEG
      aBody
      if not lResultIsTrue(lResult):
        popPEG
        break
    when lResult is bool:
      lResult = true
    else:
      lResult = Just(SimplePEGNodeObject(FIsTerminal: false, FName: "zeroOrMorePEG"))


template oneOrMorePEG(aBody: untyped): untyped =
  block oneOrMorePEG:
    aBody
    if lResultIsTrue(lResult):
      zeroOrMorePEG:
        aBody


template optionPEG(aBody: untyped): untyped =
  block optionPEG:
    pushPEG
    aBody
    if not lResultIsTrue(lResult):
      popPEG
      when lResult is bool:
        lResult = true
      else:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: false, FName: "optionPEG"))


template orPEG(aBody, aOrBody: untyped): untyped =
  block orPEG:
    pushPEG
    aBody
    if not lResultIsTrue(lResult):
      popPEG
      aOrBody
    

template andPEG(aBody, aAndBody: untyped): untyped =
  block andPEG:
    aBody
    if lResultIsTrue(lResult):
      aAndBody


template notPEG(aBody: untyped): untyped =
  block notPEG:
    pushPEG
    aBody
    popPEG
    when lResult is bool:
      lResult = not lResult
    else:
      if lResult.hasValue:
        lResult = Nothing[SimplePEGNodeObject]()
      else:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: false, FName: "notPEG"))


template ruleRefPEG(aRuleName: untyped): untyped =
  block ruleRefPEG:
    let lRuleResult = aStream.aRuleName
    when lResult is bool:
      when lRuleResult is bool:
        lResult = lRuleResult
      else:
        lResult = lRuleResult.hasValue
    else:
      when lRuleResult is bool:
        if lRuleResult:
          lResult = Just(SimplePEGNodeObject(FIsTerminal: false, FName: astToStr(aRulename)))
        else:
          lResult = Nothing[SimplePEGNodeObject]()
      else:
        lResult = lRuleResult
        if lResult.hasValue:
          lStack.add(lResult.value)


template terminalPEG(aExpectedValue: string | set[char]): untyped =
  block terminalPEG:
    when lResult is bool:
      when (aExpectedValue is string):
        lResult = aStream.stringInString(aExpectedValue).hasValue
      else:
        lResult = aStream.charInChars(aExpectedValue).hasValue
    else:
      when (aExpectedValue is string):
        let lTerminal = aStream.stringInString(aExpectedValue)
      else:
        let lTerminal = aStream.charInChars(aExpectedValue)      
      if lTerminal.hasValue:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: true, FSlice: lTerminal.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


template stringOfPEG (aBody: untyped): untyped =
  block stringOfPEG:
    when not (lResult is bool):
      pushStackStatePEG
      aBody
      if lResultIsTrue(lResult) and (lStackLen < lStack.len):
        var lItems: SimplePEGNodeObjectSeq = newSeqOfCap[SimplePEGNodeObject](lStack.len - lStackLen)
        for lItem in lStackLen .. <lStack.len:
          lItems.add(lStack[lItem])
        popStackStatePEG
        lResult = Just(SimplePEGNodeObject(FIsTerminal: false, FName: "$", FItems: lItems))
        lResult.value.FName &= lResult.valuePEG
        lStack.add(lResult.value)
    else:
      aBody


template  discardPEG (aBody: untyped): untyped =
  block discardPEG:
    pushStackStatePEG
    aBody
    popStackStatePEG
    

voidPEGRule(EndOfLine):
    orPEG:
      andPEG:
        {'\13'}.terminalPEG
      do:
        {'\10'}.terminalPEG
    do:
      {'\10'}.terminalPEG


leftPEGRule(LChar):
  orPEG:
    andPEG:
      "\\".terminalPEG
    do:
      {'n', 'r', 't', '\'', '"', '\\'}.terminalPEG
  do:
    andPEG:
      notPEG:
        "\\".terminalPEG
    do:
      andPEG:
        notPEG:
          ruleRefPEG(EndOfLine)
      do:
        anyPEG


leftPEGRule(Hex):
  andPEG:
    discardPEG:
      "\\<".terminalPEG
  do:
    andPEG:
      {'0'..'9', 'A'..'F', 'a'..'f'}.terminalPEG
    do:
      andPEG:
        {'0'..'9', 'A'..'F', 'a'..'f'}.terminalPEG
      do:
        discardPEG:
          ">".terminalPEG


leftPEGRule(Char):
  orPEG:
    andPEG:
      "\\".terminalPEG
    do:
      {'n', 'r', 't', '-', ']', '\\'}.terminalPEG
  do:
    andPEG:
      notPEG:
        "\\".terminalPEG
    do:
      andPEG:
        notPEG:
          "]".terminalPEG
      do:
        andPEG:
          notPEG:
            ruleRefPEG(EndOfLine)
        do:
          anyPEG


leftPEGRule(Range):
  andPEG:
    orPEG:
      ruleRefPEG(Char)
    do:
      ruleRefPEG(Hex)
  do:
    optionPEG:
      andPeg:
        discardPEG:
          "-".terminalPEG
      do:
        orPEG:
          ruleRefPEG(Char)
        do:
          ruleRefPEG(Hex)


voidPEGRuleForward(Ws)


leftPEGRule(CharClass):
  andPEG:
    discardPEG:
      "[".terminalPEG
  do:
    andPEG:
      zeroOrMorePEG:
        andPEG:
          notPEG:
            "]".terminalPEG
        do:
          ruleRefPEG(Range)
    do:
      andPEG:
        discardPEG:
          "]".terminalPEG
      do:
        ruleRefPEG(Ws)


leftPEGRule(WildCard):
  andPEG:
    ".".terminalPEG
  do:
    ruleRefPEG(Ws)


voidPEGRule(SComment):
  andPEG:
    "#".terminalPEG
  do:
    zeroOrMorePEG:
      andPEG:
        notPEG:
          ruleRefPEG(EndOfLine)
      do:
        anyPeg
      orPEG:
        ruleRefPEG(EndOfLine)
      do:
        notPEG:
          anyPeg


voidPEGRule(MComment):
  andPEG:
    "/*".terminalPEG
  do:
    andPeg:
      zeroOrMorePEG:
        orPEG:
          ruleRefPEG(MComment)
        do:
          andPEG:
            notPEG:
              "*/".terminalPEG
          do:
            anyPEG
    do:
      "*/".terminalPEG


voidPEGRule(Ws):
  zeroOrMorePEG:
    orPEG:
      {' ', '\t'}.terminalPEG
    do:
      orPEG:
        ruleRefPEG(EndOfLine)
      do:
        orPEG:
          ruleRefPEG(SComment)
        do:
          ruleRefPEG(MComment)


leftPEGRule(Identifier):
  andPEG:
    stringOfPEG:
      andPEG:
        {'a'..'z', 'A'..'Z', '_'}.terminalPEG
      do:
        zeroOrMorePEG:
          {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}.terminalPEG          
  do:
    ruleRefPEG(Ws)


voidPEGRule(Alt):
  andPeg:
    "|".terminalPEG
  do:
    ruleRefPEG(Ws)


leftPEGRule(Literal):
  andPEG:
    discardPEG:
      "'".terminalPEG
  do:
    andPEG:
      stringOfPEG:
        oneOrMorePEG:
          andPEG:
            notPeg:
              "'".terminalPEG
          do:
            orPEG:
              ruleRefPEG(LChar)
            do:
              ruleRefPEG(Hex)

    do:
      andPEG:
        discardPEG:
          "'".terminalPEG
      do:
        ruleRefPEG(Ws)
        

leftPEGRule(CaseLiteral):
  andPEG:
    discardPEG:
      "\"".terminalPEG
  do:
    andPEG:
      stringOfPEG:
        oneOrMorePEG:
          andPEG:
            notPeg:
              "\"".terminalPEG
          do:
            orPEG:
              ruleRefPEG(LChar)
            do:
              ruleRefPEG(Hex)

    do:
      andPEG:
        discardPEG:
          "\"".terminalPEG
      do:
        ruleRefPEG(Ws)


voidPEGRule(Close):
  andPEG:
    ")".terminalPEG
  do:
    ruleRefPEG(Ws)


voidPEGRule(Open):
  andPEG:
    "(".terminalPEG
  do:
    ruleRefPEG(Ws)


leftPEGRule(Prefix):
  andPEG:
    {'?', '*', '+', ':', '&', '!', '$'}.terminalPEG
  do:
    ruleRefPEG(Ws)
    

prunePEGRuleForward(Arrow)
leftPEGRuleForward(Alternation)
  

leftPEGRule(Unit):
  andPEG:
    optionPEG:
      ruleRefPEG(Prefix)
  do:
    orPEG:
      andPEG:
        ruleRefPEG(Identifier)
      do:
        notPEG:
          ruleRefPEG(Arrow)
    do:
      orPEG:
        andPEG:
          ruleRefPEG(Open)
        do:
          andPEG:
            ruleRefPEG(Alternation)
          do:
            ruleRefPEG(Close)
      do:
        orPEG:
          ruleRefPEG(Literal)
        do:
          orPEG:
            ruleRefPEG(CaseLiteral)
          do:
            orPEG:
              ruleRefPEG(CharClass)
            do:
              ruleRefPEG(WildCard)
              

leftPEGRule(Sequence):
  oneOrMorePEG:
    ruleRefPEG(Unit)


leftPEGRule(Alternation):
  andPEG:
    ruleRefPEG(Sequence)
  do:
    zeroOrMorePEG:
      andPEG:
        ruleRefPEG(Alt)
      do:
        ruleRefPEG(Sequence)


leftPEGRule(VoidArrow):
  andPEG:
    discardPEG:
      "<:".terminalPEG
  do:
    ruleRefPEG(Ws)


leftPEGRule(PruneArrow):
  andPEG:
    discardPEG:
      "<=".terminalPEG
  do:
    ruleRefPEG(Ws)


leftPEGRule(LeftArrow):
  andPEG:
    discardPEG:
      "<-".terminalPEG
  do:
    ruleRefPEG(Ws)


prunePEGRule(Arrow):
  orPEG:
    ruleRefPEG(LeftArrow)
  do:
    orPEG:
      ruleRefPEG(PruneArrow)
    do:
      ruleRefPEG(VoidArrow)


leftPEGRule(Definition):
  andPEG:
    ruleRefPEG(Identifier)
  do:
    andPEG:
      ruleRefPEG(Arrow)
    do:
      andPEG:
        ruleRefPEG(Alternation)
      do:
        ruleRefPEG(Ws)


leftPEGRule(WAXEYE):
  andPEG:
      ruleRefPEG(Ws)
  do:
    zeroOrMorePEG do:
      ruleRefPEG(Definition)


when isMainModule:
  const 
    WAXEYE_GRAMMAR_WAXEYE = """
WAXEYE      <- Ws *Definition
Definition  <- Identifier Arrow Alternation Ws
Alternation <- Sequence *( Alt Sequence )
Sequence    <- +Unit
Unit        <- ?Prefix
              ( Identifier !Arrow
              | Open Alternation Close
              | Literal
              | CaseLiteral
              | CharClass
              | WildCard )
Prefix      <- [?*+:&!$] Ws
Identifier  <- $( [a-zA-Z_] *[a-zA-Z0-9_-] ) Ws
Literal     <- :['] $( +( !['] ( LChar 
                              | Hex ) ) ) :['] Ws
CaseLiteral <- :["] $( +( !["] ( LChar 
                              | Hex ) ) ) :["] Ws
LChar       <- '\\' [nrt'"\\] 
            | !'\\' !EndOfLine .
CharClass   <- :'[' *( !']' Range ) :']' Ws
Range       <- ( Char | Hex ) ?( :'-' ( Char | Hex ) )
Char        <- '\\' [nrt\-\]\\] 
            | !'\\' !']' !EndOfLine .
Hex         <- :'\\<' [0-9A-Fa-f] [0-9A-Fa-f] :'>'
WildCard    <- :'.' Ws
Arrow       <= LeftArrow 
            | PruneArrow 
            | VoidArrow
LeftArrow   <- :'<-' Ws
PruneArrow  <- :'<=' Ws
VoidArrow   <- :'<:' Ws
Alt         <: '|' Ws
Open        <: '(' Ws
Close       <: ')' Ws
SComment    <: '#' *( !EndOfLine . ) ( EndOfLine 
                                    | !. )
MComment    <: '/*' *( MComment 
                    | !'*/' . ) '*/'
EndOfLine   <: '\r' ?'\n' 
            | '\n'
Ws          <: *( [ \t] 
              | EndOfLine 
              | SComment 
              | MComment)"""
              
  proc mainModule = 
    "a <- \"12\" b <: \"34\" \"56\"".withStream(lStream):
      let lSimpleASTNode = lStream.WAXEYE().asSimpleASTNode
      if not lSimpleASTNode.isNil:
        echo lSimpleASTNode.asASTStr

    WAXEYE_GRAMMAR_WAXEYE.withStream(lStream):
      let lSimpleASTNode = lStream.WAXEYE().asSimpleASTNode
      if not lSimpleASTNode.isNil:
        echo lSimpleASTNode.asASTStr
  
  mainModule()

#nim --putenv:NIM_VERBOSITY=3 cBuild release 2> log.txt
