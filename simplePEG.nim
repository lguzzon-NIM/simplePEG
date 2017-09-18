
import streams

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


proc valuePEG (aSimplePEGNodeObject: SimplePEGNodeObject): string =
  if aSimplePEGNodeObject.FIsTerminal:
    result = aSimplePEGNodeObject.FSlice.asString
  else:
    result = ""
    for lItem in aSimplePEGNodeObject.FItems:
      result &= lItem.valuePEG
      

template valuePEG(aNode: Maybe[SimplePEGNodeObject]): string =
  (if aNode.hasValue: aNode.value.valuePEG  else: "")
    

template withSimplePEG (aString: string; aBody: untyped) =
  block withSimplePEG:
    let lStream : Stream = newStringStream(aString)
    if not lStream.isNil:
      var SimplePEG {.inject.} = SimplePEG(FStream:lStream)
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


template boolPEGRule(aRuleName: untyped, aBody: untyped): untyped =
  proc aRuleName(aSimplePEG: SimplePEG): bool =
    var lResult {. inject .} = false
    aBody
    result = lResult


template nodePEGRule(aRuleName: untyped, aBody: untyped): untyped =
  proc aRuleName(aSimplePEG: SimplePEG): Maybe[SimplePEGNodeObject] =
    discard
    # var lStack {. inject .} = newSeqOfCap[SimplePEGNodeObject](8)
    # var lResult {. inject .} = Nothing[SimplePEGNodeObject]()
    # aBody
    # if lResultIsTrue(lResult):
    #   result = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "aRuleName", FItems: lStack))
    # else:
    #   result = Nothing[SimplePEGNodeObject]()


template lResultIsTrue(aResult: bool | Maybe[SimplePEGNodeObject]): untyped =
  when aResult is bool:
    aResult
  else:
    aResult.hasValue


template anyPEG: untyped =
  block anyPEG:
    lResult = aSimplePEG.read(1, false).hasValue


template zeroOrMorePEG(aBody: untyped): untyped =
  block zeroOrMorePEG:
    while true:
      let lPosition = aSimplePEG.FStream.getPosition
      when (not(lResult is bool)):
        let lStackLen = lStack.len
      aBody
      if not lResultIsTrue(lResult):
        when (not(lResult is bool)):
          lStack.setLen(lStackLen)
        aSimplePEG.FStream.setPosition(lPosition)
        break
    when lResult is bool:
      lResult = true
    else:
      lResult = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "*"))


template orPEG(aBody, aOrBody: untyped): untyped =
  block orPEG:
    let lPosition = aSimplePEG.FStream.getPosition
    when (not(lResult is bool)):
      let lStackLen = lStack.len
    aBody
    if not lResultIsTrue(lResult):
      when (not(lResult is bool)):
        lStack.setLen(lStackLen)
      aSimplePEG.FStream.setPosition(lPosition)
      aOrBody


template andPEG(aBody, aOrBody: untyped): untyped =
  block andPEG:
    aBody
    if lResultIsTrue(lResult):
      aOrBody


template notPEG(aBody: untyped): untyped =
  block notPEG:
    let lPosition = aSimplePEG.FStream.getPosition
    when (not(lResult is bool)):
      let lStackLen = lStack.len
    aBody
    when (not(lResult is bool)):
      lStack.setLen(lStackLen)
    aSimplePEG.FStream.setPosition(lPosition)
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
          result = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "aRuleName"))
        else:
          result = Nothing[SimplePEGNodeObject]()
      else:
        lResult = lRuleResult


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
      else:
        lResult = Nothing[SimplePEGNodeObject]()


proc `[]`[T](s: seq[T], x: Slice[int]): seq[T] =
  ## slice operation for sequences.
  var a = x.a
  var L = x.b - a + 1
  result = newSeqOfCap[T](L)
  for i in 0.. <L: result[i] = s[i + a]


template stringOfPEG(aBody: untyped): untyped =
  block stringOfPEG:
    let lStackLen = lStack.len
    aBody
    if lResultIsTrue(lResult) and (lStackLen < lStack.len):
      var lName = ""
      echo "lName ... " & lName
      let lSlice = lStack[lStackLen .. (lStack.len-1)]
      for lItem in lSlice:
        lName &= lItem.valuePEG
      let lStringOfPEG = SimplePEGNodeObject(FIsTerminal: false,FName:lName, FItems: lSlice)
      lStack.setLen(lStackLen)
      lStack.add(lStringOfPEG)


boolPEGRule(peg_WAXEYE_EndOfLine):
    orPEG:
      andPEG:
        {'\13'}.terminalPEG
      do:
        {'\10'}.terminalPEG
    do:
      {'\10'}.terminalPEG


boolPEGRule(peg_WAXEYE_SComment):
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


boolPEGRule(peg_WAXEYE_MComment):
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


boolPEGRule(peg_WAXEYE_Ws):
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


nodePEGRule(peg_WAXEYE_Alternation):
  discard


nodePEGRule(peg_WAXEYE_Arrow):
  discard


nodePEGRule(peg_WAXEYE_Identifier):
  echo "nodePEGRule" 
  #{'a'..'z', 'A'..'Z', '_'}.terminalPEG
  # andPEG:
  #   stringOfPEG:
  #     andPEG:
  #       {'a'..'z', 'A'..'Z', '_'}.terminalPEG
  #     do:
  #       zeroOrMorePEG:
  #         {'a'..'z', 'A'..'Z', '0'..'9', '_', '-'}.terminalPEG          
  # do:
  #ruleRefPEG(peg_WAXEYE_WS)


nodePEGRule(peg_WAXEYE_Definition):
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


nodePEGRule(peg_WAXEYE_WAXEYE):
  andPEG:
      ruleRefPEG(peg_WAXEYE_Ws)
  do:
    zeroOrMorePEG do:
      ruleRefPEG(peg_WAXEYE_Definition)


nodePEGRule(peg_WAXEYE_Sequence):
  discard


nodePEGRule(peg_WAXEYE_Unit):
  discard


nodePEGRule(peg_WAXEYE_Prefix):
  discard


nodePEGRule(peg_WAXEYE_Literal):
  discard


nodePEGRule(peg_WAXEYE_CaseLiteral):
  discard


nodePEGRule(peg_WAXEYE_LChar):
  discard


nodePEGRule(peg_WAXEYE_CharClass):
  discard


nodePEGRule(peg_WAXEYE_Range):
  discard


nodePEGRule(peg_WAXEYE_Char):
  discard


nodePEGRule(peg_WAXEYE_Hex):
  discard


nodePEGRule(peg_WAXEYE_WildCard):
  discard


nodePEGRule(peg_WAXEYE_LeftArrow):
  discard


nodePEGRule(peg_WAXEYE_PruneArrow):
  discard


nodePEGRule(peg_WAXEYE_VoidArrow):
  discard


nodePEGRule(peg_WAXEYE_Alt):
  discard


nodePEGRule(peg_WAXEYE_Open):
  discard


nodePEGRule(peg_WAXEYE_Close):
  discard


nodePEGRule(peg_WAXEYE_Comma):
  discard




proc getMessage*: string =
    result = cHelloWorld



when isMainModule:
  echo getMessage()
  "12345aAbC6".withSimplePEG:
    echo "Peek -> " & $SimplePeg.charInCharsPeek("12")
    echo "Read -> " & $SimplePeg.charInChars("12")
    echo "----"
    echo "Peek -> " & $SimplePeg.charInCharsPeek({'1', '2'})
    echo "Read -> " & $SimplePeg.charInChars({'1', '2'})
    echo "Read -> " & $SimplePEG.stringInStringNoCase("345A")
    echo "Read -> " & $SimplePEG.stringInString("AbC6")
    echo "Read -> " & $SimplePEG.stringInStringNoCase("A")
  " /* -- /* --- */ -- */".withSimplePEG:
    echo SimplePEG.peg_WAXEYE_MComment
  " /* -- /* --- */ -- */".withSimplePEG:
    echo SimplePEG.peg_WAXEYE_WS
  "/* -- /* --- */ -- *".withSimplePEG:
    echo SimplePEG.peg_WAXEYE_MComment
  "/* -- /* --- */ -- */".withSimplePEG:
    echo SimplePEG.peg_WAXEYE_MComment
  "/**/".withSimplePEG:
    echo SimplePEG.peg_WAXEYE_MComment
  "abc_9-10_A   ".withSimplePEG:
    echo SimplePEG.peg_WAXEYE_Identifier
  "/**/".withSimplePEG:
    echo SimplePEG.peg_WAXEYE_MComment

