
import streams
import strutils

import simplePEG.typeEx
import simplePEG.strProcs
import simplePEG.consts

type
  SimplePEGIndex = int
  SimplePEGLength = int

  SimplePEG = ref SimplePEGObject not nil
  SimplePEGObject = object {. final .}
    FStream: Stream not nil

  SimplePEGSliceObject = object {. final .}
    FSimplePEG: SimplePEG
    FIndex: SimplePEGIndex
    FLength: SimplePEGLength
    FAsString: Maybe[string]

  SimplePEGNodeObjectSeq = seq[SimplePEGNodeObject]
  SimplePEGNodeObject = object
    case FIsTerminal: bool
    of true:
      FSlice: SimplePEGSliceObject
    else:
      FName: string
      FItems: SimplePEGNodeObjectSeq


proc asString* (aSimplePEGSlice: SimplePEGSliceObject): string =
  if aSimplePEGSlice.FAsString.hasValue:
    result = aSimplePEGSlice.FAsString.value
  else:
    let lOldPosition = aSimplePEGSlice.FSimplePEG.FStream.getPosition
    try:
      aSimplePEGSlice.FSimplePEG.FStream.setPosition(aSimplePEGSlice.FIndex)
      let lLength = aSimplePEGSlice.FLength
      let lString = aSimplePEGSlice.FSimplePEG.FStream.peekstr(lLength)
      if (lLength == lString.len):
        result = lString
      else:
        result = ""
    finally:
      aSimplePEGSlice.FSimplePEG.FStream.setPosition(lOldPosition)


proc `$`(aSimplePEGNodeObject: SimplePEGNodeObject): string =
  
  proc innerEcho(aValue: SimplePEGNodeObject, aTabs: int): string =
    if aValue.FIsTerminal:
      result = ("""
      Is Terminal
        Slice -> $1
        Slice AsString -> $2
        
      """.unindent % [$aValue.FSlice, aValue.FSlice.asString()]).indent(aTabs)
    else:
      result = ("""
      Not Is Terminal
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


proc endIndex(aSelf: SimplePEGNodeObject | Maybe[SimplePEGNodeObject]): int =
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


proc valuePEG (aSelf: SimplePEGNodeObject | Maybe[SimplePEGNodeObject]): string =
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


template withSimplePEG (aString: string; aBody: untyped) =
  block withSimplePEG:
    let lStream : Stream = newStringStream(aString)
    if not lStream.isNil:
      let SimplePEG {.inject.} = SimplePEG(FStream:lStream)
      block withSimplePEGBody:
        aBody


proc read* (aSimplePEG: SimplePEG, aLength: SimplePEGLength, aUsePeek: static[bool]): Maybe[SimplePEGSliceObject] =
  if 1>aLength:
    result = Nothing[SimplePEGSliceObject]()
  else:
    try:
      let lBeginIndex = aSimplePEG.FStream.getPosition
      when aUsePeek:
        defer:
          aSimplePEG.FStream.setPosition(lBeginIndex)
      try:
        let lEndPosition = lBeginIndex + aLength
        aSimplePEG.FStream.setPosition(lEndPosition)
        if lEndPosition == aSimplePEG.FStream.getPosition:
          result = Just(SimplePEGSliceObject(FSimplePEG:aSimplePEG, FIndex:lBeginIndex, FLength:aLength))
        else:
          result = Nothing[SimplePEGSliceObject]()
      except:
        result = Nothing[SimplePEGSliceObject]()
    except:
      result = Nothing[SimplePEGSliceObject]()


proc charInChars* (aSimplePEG: SimplePEG, aChars: string | set[char], aUsePeek: static[bool], aSaveAsString: static[bool]): Maybe[SimplePEGSliceObject] =
  var lRead = aSimplePEG.read(sizeof(char),aUsePeek)
  if lRead.hasValue:
    let lAsString = lRead.value.asString
    if lAsString[0] in aChars:
      when aSaveAsString:
        lRead.value.FAsString = Just(lAsString)
      result = Just(lRead.value)
    else:
      result = Nothing[SimplePEGSliceObject]()
  else:
    result = Nothing[SimplePEGSliceObject]()


template charInChars* (aSimplePEG: SimplePEG, aChars: string | set[char]): Maybe[SimplePEGSliceObject] =
  aSimplePEG.charInChars(aChars, false, false)


template charInCharsPeek* (aSimplePEG: SimplePEG, aChars: string | set[char]): Maybe[SimplePEGSliceObject] =
  aSimplePEG.charInChars(aChars, true, false)


template charInCharsAsString* (aSimplePEG: SimplePEG, aChars: string | set[char]): Maybe[SimplePEGSliceObject] =
  aSimplePEG.charInChars(aChars, false, true)


template charInCharsPeekAsString* (aSimplePEG: SimplePEG, aChars: string | set[char]): Maybe[SimplePEGSliceObject] =
  aSimplePEG.charInChars(aChars, true, true)


proc stringInString* (aSimplePEG: SimplePEG, aString: string, aCasesInsensitive: static[bool], aUsePeek: static[bool], aSaveAsString: static[bool]): Maybe[SimplePEGSliceObject] =
  let lLength = aString.len
  var lRead = aSimplePEG.read(lLength, aUsePeek)
  if lRead.hasValue:
    let lAsString = lRead.value.asString
    if equalEx(lAsString, aString, aCasesInsensitive):
      when aSaveAsString:
        lRead.value.FAsString = Just(lAsString)
      result = Just(lRead.value)
    else:
      result = Nothing[SimplePEGSliceObject]()
  else:
    result = lRead


template stringInString* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, false, false, false)


template stringInStringNoCase* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, true, false, false)


template stringInStringPeekCase* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, false, true, false)


template stringInStringPeekNoCase* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, aString, true, true, false)


template stringInStringAsString* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, false, false, true)


template stringInStringNoCaseAsString* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, true, false, true)


template stringInStringPeekCaseAsString* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, false, true, true)


template stringInStringPeekNoCaseAsString* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  aSimplePEG.stringInString(aString, aString, true, true, true)


template innerPEGRuleForward(aRuleName, aParamName: untyped, atype: typedesc) =
  proc aRuleName(aParamName : SimplePEG): atype


template innerPEGRule(aRuleName, aParamName: untyped, atype: typedesc, aBody: untyped) =
  proc aRuleName(aParamName : SimplePEG): atype =
    aBody


template voidPEGRuleForward(aRuleName: untyped): untyped =
  innerPEGRuleForward(aRuleName, aSimplePEG, bool)


template voidPEGRule(aRuleName: untyped, aBody: untyped): untyped =
  innerPEGRule(aRuleName, aSimplePEG, bool):
    var lResult {. inject .} = false
    aBody
    result = lResult
  

template lResultIsTrue(aResult: bool | Maybe[SimplePEGNodeObject]): untyped =
  when aResult is bool:
    aResult
  else:
    aResult.hasValue

template leftPEGRuleForward(aRuleName: untyped): untyped =
  innerPEGRuleForward(aRuleName, aSimplePEG, Maybe[SimplePEGNodeObject])


template leftPEGRule(aRuleName: untyped, aBody: untyped): untyped =
  innerPEGRule(aRuleName, aSimplePEG, Maybe[SimplePEGNodeObject]):
    var lStack {. inject .} = newSeqOfCap[SimplePEGNodeObject](8)
    var lResult {. inject .} = Nothing[SimplePEGNodeObject]()
    aBody
    if lResultIsTrue(lResult):
      result = Just(SimplePEGNodeObject(FIsTerminal: false,FName: astToStr(aRuleName), FItems: lStack[0..<lStack.len]))
    else:
      result = Nothing[SimplePEGNodeObject]()
  

template prunePEGRuleForward(aRuleName: untyped): untyped =
  innerPEGRuleForward(aRuleName, aSimplePEG, Maybe[SimplePEGNodeObject])

template prunePEGRule(aRuleName: untyped, aBody: untyped): untyped =
  innerPEGRule(aRuleName, aSimplePEG, Maybe[SimplePEGNodeObject]):
    var lStack {. inject .} = newSeqOfCap[SimplePEGNodeObject](8)
    assert(lStack.len==0)
    var lResult {. inject .} = Nothing[SimplePEGNodeObject]()
    aBody
    if lResultIsTrue(lResult):
      if 1 == lStack.len:
        result = Just(lStack[0])
      else:
        result = Just(SimplePEGNodeObject(FIsTerminal: false,FName: astToStr(aRuleName), FItems: lStack[0..<lStack.len]))
    else:
      result = Nothing[SimplePEGNodeObject]()
  

template anyPEG: untyped =
  block anyPEG:
    let lAny = aSimplePEG.read(1, false)
    when lResult is bool:
      lResult = lAny.hasValue
    else:
      if lAny.hasValue:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: true,FSlice: lAny.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()



template pushStreamStatePEG = 
  let lPosition {. inject .} = aSimplePEG.FStream.getPosition


template popStreamStatePEG =
  aSimplePEG.FStream.setPosition(lPosition)


template pushStackStatePEG = 
  when not (lResult is bool):
    let lStackLen {. inject .} = lStack.len


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
      lResult = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "zeroOrMorePEG"))


template oneOrMorePEG(aBody: untyped): untyped =
  block oneOrMorePEG:
    aBody
    if lResultIsTrue(lResult):
      while true:
        pushPEG
        aBody
        if not lResultIsTrue(lResult):
          popPEG
          break
      when lResult is bool:
        lResult = true
      else:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "oneOrMorePEG"))


template optionPEG(aBody: untyped): untyped =
  block optionPEG:
    pushPEG
    aBody
    if not lResultIsTrue(lResult):
      popPEG
      when lResult is bool:
        lResult = true
      else:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "optionPEG"))


template orPEG(aBody, aOrBody: untyped): untyped =
  block orPEG:
    pushPEG
    aBody
    if not lResultIsTrue(lResult):
      popPEG
      aOrBody
    

template andPEG(aBody, aOrBody: untyped): untyped =
  block andPEG:
    aBody
    if lResultIsTrue(lResult):
      aOrBody


template notPEG(aBody: untyped): untyped =
  block notPEG:
    pushPEG
    aBody
    popPEG
    when lResult is bool:
      lResult = not lResult
    else:
      if lResult.hasValue:
        result = Nothing[SimplePEGNodeObject]()
      else:
        result = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "notPEG"))


template ruleRefPEG(aRuleName: untyped): untyped =
  block ruleRefPEG:
    let lRuleResult = aSimplePEG.aRuleName
    when lResult is bool:
      when lRuleResult is bool:
        lResult = lRuleResult
      else:
        lResult = lRuleResult.hasValue
    else:
      when lRuleResult is bool:
        if lRuleResult:
          result = Just(SimplePEGNodeObject(FIsTerminal: false,FName: astToStr(aRulename)))
        else:
          result = Nothing[SimplePEGNodeObject]()
      else:
        lResult = lRuleResult
        if lResult.hasValue:
          lStack.add(lResult.value)


template terminalPEG(aExpectedValue: string | set[char]): untyped =
  block terminalPEG:
    when lResult is bool:
      when (aExpectedValue is string):
        lResult = aSimplePEG.stringInString(aExpectedValue).hasValue
      else:
        lResult = aSimplePEG.charInChars(aExpectedValue).hasValue
    else:
      when (aExpectedValue is string):
        let lTerminal = aSimplePEG.stringInString(aExpectedValue)
      else:
        let lTerminal = aSimplePEG.charInChars(aExpectedValue)      
      if lTerminal.hasValue:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: true,FSlice: lTerminal.value))
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
        let lResult = SimplePEGNodeObject(FIsTerminal: false, FName:"$", FItems:lItems)
        lStack.add(lResult)
    else:
      aBody


template  discardPEG (aBody: untyped): untyped =
  block discardPEG:
    pushStackStatePEG
    aBody
    popStackStatePEG
    

voidPEGRule(peg_WAXEYE_EndOfLine):
    orPEG:
      andPEG:
        {'\13'}.terminalPEG
      do:
        {'\10'}.terminalPEG
    do:
      {'\10'}.terminalPEG


leftPEGRule(peg_WAXEYE_LChar):
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
          ruleRefPEG(peg_WAXEYE_EndOfLine)
      do:
        anyPEG


leftPEGRule(peg_WAXEYE_Hex):
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


leftPEGRule(peg_WAXEYE_Char):
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
            ruleRefPEG(peg_WAXEYE_EndOfLine)
        do:
          anyPEG


leftPEGRule(peg_WAXEYE_Range):
  andPEG:
    orPEG:
      ruleRefPEG(peg_WAXEYE_Char)
    do:
      ruleRefPEG(peg_WAXEYE_Hex)
  do:
    optionPEG:
      andPeg:
        discardPEG:
          "-".terminalPEG
      do:
        orPEG:
          ruleRefPEG(peg_WAXEYE_Char)
        do:
          ruleRefPEG(peg_WAXEYE_Hex)


voidPEGRuleForward(peg_WAXEYE_Ws)


leftPEGRule(peg_WAXEYE_CharClass):
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
          ruleRefPEG(peg_WAXEYE_Range)
    do:
      andPEG:
        discardPEG:
          "]".terminalPEG
      do:
        ruleRefPEG(peg_WAXEYE_Ws)


leftPEGRule(peg_WAXEYE_WildCard):
  andPEG:
    ".".terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


voidPEGRule(peg_WAXEYE_SComment):
  andPEG:
    "#".terminalPEG
  do:
    zeroOrMorePEG:
      andPEG:
        notPEG:
          ruleRefPEG(peg_WAXEYE_EndOfLine)
      do:
        anyPeg
      orPEG:
        ruleRefPEG(peg_WAXEYE_EndOfLine)
      do:
        notPEG:
          anyPeg


voidPEGRule(peg_WAXEYE_MComment):
  andPEG:
    "/*".terminalPEG
  do:
    andPeg:
      zeroOrMorePEG:
        orPEG:
          ruleRefPEG(peg_WAXEYE_MComment)
        do:
          andPEG:
            notPEG:
              "*/".terminalPEG
          do:
            anyPEG
    do:
      "*/".terminalPEG


voidPEGRule(peg_WAXEYE_Ws):
  zeroOrMorePEG:
    orPEG:
      {' ', '\t'}.terminalPEG
    do:
      orPEG:
        ruleRefPEG(peg_WAXEYE_EndOfLine)
      do:
        orPEG:
          ruleRefPEG(peg_WAXEYE_SComment)
        do:
          ruleRefPEG(peg_WAXEYE_MComment)


leftPEGRule(peg_WAXEYE_Identifier):
  andPEG:
    stringOfPEG:
      andPEG:
        {'a'..'z', 'A'..'Z', '_'}.terminalPEG
      do:
        zeroOrMorePEG:
          {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}.terminalPEG          
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


voidPEGRule(peg_WAXEYE_Alt):
  andPeg:
    "|".terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


leftPEGRule(peg_WAXEYE_Literal):
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
              ruleRefPEG(peg_WAXEYE_LChar)
            do:
              ruleRefPEG(peg_WAXEYE_Hex)

    do:
      andPEG:
        discardPEG:
          "'".terminalPEG
      do:
        ruleRefPEG(peg_WAXEYE_Ws)
        

leftPEGRule(peg_WAXEYE_CaseLiteral):
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
              ruleRefPEG(peg_WAXEYE_LChar)
            do:
              ruleRefPEG(peg_WAXEYE_Hex)

    do:
      andPEG:
        discardPEG:
          "\"".terminalPEG
      do:
        ruleRefPEG(peg_WAXEYE_Ws)


voidPEGRule(peg_WAXEYE_Close):
  andPEG:
    ")".terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


voidPEGRule(peg_WAXEYE_Open):
  andPEG:
    "(".terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


leftPEGRule(peg_WAXEYE_Prefix):
  andPEG:
    {'?','*','+',':','&','!','$'}.terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)
    

prunePEGRuleForward(peg_WAXEYE_Arrow)
leftPEGRuleForward(peg_WAXEYE_Alternation)
  

leftPEGRule(peg_WAXEYE_Unit):
  andPEG:
    optionPEG:
      ruleRefPEG(peg_WAXEYE_Prefix)
  do:
    orPEG:
      andPEG:
        ruleRefPEG(peg_WAXEYE_Identifier)
      do:
        notPEG:
          ruleRefPEG(peg_WAXEYE_Arrow)
    do:
      orPEG:
        andPEG:
          ruleRefPEG(peg_WAXEYE_Open)
        do:
          andPEG:
            ruleRefPEG(peg_WAXEYE_Alternation)
          do:
            ruleRefPEG(peg_WAXEYE_Close)
      do:
        orPEG:
          ruleRefPEG(peg_WAXEYE_Literal)
        do:
          orPEG:
            ruleRefPEG(peg_WAXEYE_CaseLiteral)
          do:
            orPEG:
              ruleRefPEG(peg_WAXEYE_CharClass)
            do:
              ruleRefPEG(peg_WAXEYE_WildCard)
              

leftPEGRule(peg_WAXEYE_Sequence):
  oneOrMorePEG:
    ruleRefPEG(peg_WAXEYE_Unit)


leftPEGRule(peg_WAXEYE_Alternation):
  andPEG:
    ruleRefPEG(peg_WAXEYE_Sequence)
  do:
    zeroOrMorePEG:
      andPEG:
        ruleRefPEG(peg_WAXEYE_Alt)
      do:
        ruleRefPEG(peg_WAXEYE_Sequence)


leftPEGRule(peg_WAXEYE_VoidArrow):
  andPEG:
    discardPEG:
      "<:".terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


leftPEGRule(peg_WAXEYE_PruneArrow):
  andPEG:
    discardPEG:
      "<=".terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


leftPEGRule(peg_WAXEYE_LeftArrow):
  andPEG:
    discardPEG:
      "<-".terminalPEG
  do:
    ruleRefPEG(peg_WAXEYE_Ws)


prunePEGRule(peg_WAXEYE_Arrow):
  orPEG:
    ruleRefPEG(peg_WAXEYE_LeftArrow)
  do:
    orPEG:
      ruleRefPEG(peg_WAXEYE_PruneArrow)
    do:
      ruleRefPEG(peg_WAXEYE_VoidArrow)


leftPEGRule(peg_WAXEYE_Definition):
  andPEG:
    ruleRefPEG(peg_WAXEYE_Identifier)
  do:
    andPEG:
      ruleRefPEG(peg_WAXEYE_Arrow)
    do:
      andPEG:
        ruleRefPEG(peg_WAXEYE_Alternation)
      do:
        ruleRefPEG(peg_WAXEYE_Ws)


leftPEGRule(peg_WAXEYE_WAXEYE):
  andPEG:
      ruleRefPEG(peg_WAXEYE_Ws)
  do:
    zeroOrMorePEG do:
      ruleRefPEG(peg_WAXEYE_Definition)


proc getMessage*: string =
    result = cHelloWorld



when isMainModule:
  echo getMessage()
  
  "<-".withSimplePEG:
    let lNode = SimplePEG.peg_WAXEYE_Arrow
    echo $lNode

  # "abc_9-10_A   A".withSimplePEG:
  #   let a = SimplePEG.peg_WAXEYE_Identifier
  #   echo $a
  #   echo endIndex(a)
  
  # "/**/".withSimplePEG:
  #   echo SimplePEG.peg_WAXEYE_MComment
  # "12345aAbC6".withSimplePEG:
  #   echo "Peek -> " & $SimplePeg.charInCharsPeek("12")
  #   echo "Read -> " & $SimplePeg.charInChars("12")
  #   echo "----"
  #   echo "Peek -> " & $SimplePeg.charInCharsPeek({'1', '2'})
  #   echo "Read -> " & $SimplePeg.charInChars({'1', '2'})
  #   echo "Read -> " & $SimplePEG.stringInStringNoCase("345A")
  #   echo "Read -> " & $SimplePEG.stringInString("AbC6")
  #   echo "Read -> " & $SimplePEG.stringInStringNoCase("A")
  # " /* -- /* --- */ -- */".withSimplePEG:
  #   echo SimplePEG.peg_WAXEYE_MComment
  # " /* -- /* --- */ -- */".withSimplePEG:
  #   echo SimplePEG.peg_WAXEYE_Ws
  # "/* -- /* --- */ -- *".withSimplePEG:
  #   echo SimplePEG.peg_WAXEYE_MComment
  # "/* -- /* --- */ -- */".withSimplePEG:
  #   echo SimplePEG.peg_WAXEYE_MComment
  # "/**/".withSimplePEG:
  #   echo SimplePEG.peg_WAXEYE_MComment

