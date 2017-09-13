
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


template withSimplePEG (aString: string; aBody: untyped) =
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


template lResultIsTrue(aResult: bool | Maybe[SimplePEGNodeObject]): untyped =
  when aResult is bool:
    aResult
  else:
    aResult.hasValue

template anyPEG(aSimplePEG: SimplePEG): untyped =
  lResult = aSimplePEG.read(1, false).hasValue

template zeroOrMorePEG(aUsingStack: static[bool], aBody: untyped): untyped =
  while true:
    let lPosition = aSimplePEG.FStream.getPosition
    when aUsingStack:
      let lStackLen = lStack.len
    aBody
    if not lResultIsTrue(lResult):
      when aUsingStack:
        lStack.setLen(lStackLen)
      aSimplePEG.FStream.setPosition(lPosition)
      break
  when lResult is bool:
    lResult = true
  else:
    lResult = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "*"))


template zeroOrMorePEGStack(aBody: untyped): untyped =
  true.zeroOrMorePEG:
    aBody


template zeroOrMorePEGNoStack(aBody: untyped): untyped =
  false.zeroOrMorePEG:
    aBody


template orPEG(aUsingStack: static[bool], aBody, aOrBody: untyped): untyped =
  let lPosition = aSimplePEG.FStream.getPosition
  when aUsingStack:
    let lStackLen = lStack.len
  aBody
  if not lResultIsTrue(lResult):
    when aUsingStack:
      lStack.setLen(lStackLen)
    aSimplePEG.FStream.setPosition(lPosition)
    aOrBody


template orPEGStack(aBody, aOrBody: untyped): untyped =
  true.orPEG:
    aBody
  do:
    aOrBody


template orPEGNoStack(aBody, aOrBody: untyped): untyped =
  false.orPEG:
    aBody
  do:
    aOrBody


template andPEG(aBody, aOrBody: untyped): untyped =
  aBody
  if lResultIsTrue(lResult):
    aOrBody


template notPEG(aBody): untyped =
  let lPosition = aSimplePEG.FStream.getPosition
  aBody
  aSimplePEG.FStream.setPosition(lPosition)
  lResult = not lResult


proc peg_WAXEYE_EndOfLine(aSimplePEG: SimplePEG): bool =
  var lResult = false
  orPEGNoStack:
    andPEG:
      lResult = aSimplePEG.charInChars({'\13'}).hasValue
    do:
      lResult = aSimplePEG.charInChars({'\10'}).hasValue      
  do:
    lResult = aSimplePEG.charInChars({'\10'}).hasValue
  result = lResult


proc peg_WAXEYE_SComment(aSimplePEG: SimplePEG): bool =
  var lResult = false
  andPEG:
    lResult = aSimplePEG.charInChars({'#'}).hasValue
  do:
    zeroOrMorePEGNoStack:
      andPEG:
        notPEG:
          lResult = aSimplePEG.peg_WAXEYE_EndOfLine
      do:
        aSimplePEG.anyPeg
      orPEGNoStack:
        lResult = aSimplePEG.peg_WAXEYE_EndOfLine
      do:
        notPEG:
          aSimplePEG.anyPeg
  result = lResult


proc peg_WAXEYE_MComment(aSimplePEG: SimplePEG): bool =
  var lResult = false
  andPEG:
    lResult = aSimplePEG.stringInString("/*").hasValue
  do:
    andPeg:
      zeroOrMorePEGNoStack:
        orPEGNoStack:
          lResult = aSimplePEG.peg_WAXEYE_MComment
        do:
          andPEG:
            notPEG:
              lResult = aSimplePEG.stringInString("*/").hasValue
          do:
            aSimplePEG.anyPEG
    do:
      lResult = aSimplePEG.stringInString("*/").hasValue
  result = lResult


proc peg_WAXEYE_Ws(aSimplePEG: SimplePEG): bool =
  const lUseStack = false
  var lResult = false

  lUseStack.zeroOrMorePEG:
    lUseStack.orPEG:
      lResult = aSimplePEG.charInChars({' ', '\t'}).hasValue
    do:
      lUseStack.orPEG:
        lResult = aSimplePEG.peg_WAXEYE_EndOfLine
      do:
        lUseStack.orPEG:
          lResult = aSimplePEG.peg_WAXEYE_SComment
        do:
          lResult = aSimplePEG.peg_WAXEYE_MComment
  result = lResult

proc peg_WAXEYE_Definition(aSimplePEG: SimplePEG): Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_WAXEYE(aSimplePEG: SimplePEG): Maybe[SimplePEGNodeObject] =
  var lStack = newSeqOfCap[SimplePEGNodeObject](8)
  var lResult = aSimplePEG.peg_WAXEYE_Ws
  if lResultIsTrue(lResult):
    zeroOrMorePEGStack do:
      let lPEG = peg_WAXEYE_Definition(aSimplePEG)
      if lPEG.hasValue:
        lStack.add(lPEG.value)
  if lResultIsTrue(lResult):
    result = Just(SimplePEGNodeObject(FIsTerminal: false,FName: "WAXEYE", FItems: lStack))
  else:
    result = Nothing[SimplePEGNodeObject]()


proc peg_WAXEYE_Alternation: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Sequence: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Unit: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Prefix: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Identifier: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Literal: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_CaseLiteral: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_LChar: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_CharClass: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Range: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Char: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Hex: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_WildCard: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Arrow: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_LeftArrow: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_PruneArrow: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_VoidArrow: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Alt: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Open: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Close: Maybe[SimplePEGNodeObject] =
  discard


proc peg_WAXEYE_Comma: Maybe[SimplePEGNodeObject] =
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
