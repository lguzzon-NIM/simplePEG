




































import streams
import strutils

import simpleAST

import simplePEG/typeEx
import simplePEG/strProcs
import simplePEG/consts


type
  SimplePEGIndex = int
  SimplePEGLength = int

  SimplePEGSliceObject = object {.final.}
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
      if (not result.isNil):
        for lItem in lSelf.FItems:
          let lChild = lItem.asSimpleASTNode
          if not lChild.isNil:
            discard result.addChild(lChild)
          else:
            return nil


proc asString* (aSimplePEGSlice: SimplePEGSliceObject): string =
  result = aSimplePEGSlice.FAsString
  if result.len == 0 :
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
      result = (



































                                                                                                                                                      """

Is Terminal
  Slice -> $1
  Slice AsString -> $2
      """ %
          [$aValue.FSlice, aValue.FSlice.asString]).indent(aTabs)
    else:
      result = (
          """

Is Not Terminal
  Name -> $1
  Items -> $2
      """ % [
          $aValue.Fname, $aValue.FItems.len]).indent(aTabs)
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
    if lSelf.FItems.len == 0:
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
    if lSelf.FItems.len > 0:
      result = ""
      for lItem in lSelf.FItems:
        result &= lItem.valuePEG
    else:
      result = lSelf.FName


template withStream (
    aString: string,
    aStream, aBody: untyped) =
  block withStream:
    let aStream {.inject.}: Stream = newStringStream(aString)
    if not aStream.isNil:
      block withStream_Body:
        aBody


proc read* (aStream: Stream,
    aLength: SimplePEGLength,
    aUsePeek: static[bool]): SimplePEGSliceObjectOption =
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
          result = Just(SimplePEGSliceObject(FStream: aStream,
              FIndex: lBeginIndex,
              FLength: aLength))
        else:
          result = Nothing[SimplePEGSliceObject]()
      except:
        result = Nothing[SimplePEGSliceObject]()
    except:
      result = Nothing[SimplePEGSliceObject]()


proc charInChars* (aStream: Stream,
    aChars: string | set[char],
    aUsePeek: static[bool],
    aSaveAsString: static[bool]): SimplePEGSliceObjectOption =
  var lRead = aStream.read(sizeof(char), aUsePeek)
  if lRead.hasValue:
    let lValue = lRead.value
    let lAsString = lValue.asString
    if lAsString[0] in aChars:
      when aSaveAsString:
        lValue.FAsString = lAsString
      result = Just(lValue)
    else:
      result = Nothing[SimplePEGSliceObject]()
  else:
    result = Nothing[SimplePEGSliceObject]()


template charInChars* (aStream: Stream,
    aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, false, false)


template charInCharsPeek* (aStream: Stream,
    aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, true, false)


template charInCharsAsString* (aStream: Stream,
    aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, false, true)


template charInCharsPeekAsString* (aStream: Stream,
    aChars: string | set[char]): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, true, true)


proc stringInString* (aStream: Stream,
    aString: string,
    aCasesInsensitive: static[bool],
    aUsePeek: static[bool],
    aSaveAsString: static[bool]): SimplePEGSliceObjectOption =
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


template stringInString* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, false, false)


template stringInStringNoCase* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, true, false, false)


template stringInStringPeekCase* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, true, false)


template stringInStringPeekNoCase* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, aString, true, true, false)


template stringInStringAsString* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, false, true)


template stringInStringNoCaseAsString* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, true, false, true)


template stringInStringPeekCaseAsString* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, false, true, true)


template stringInStringPeekNoCaseAsString* (aStream: Stream,
    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString, aString, true, true, true)


template innerDefinitionPEGForward(aRuleName, aParamName: untyped,
    atype: typedesc) =
  proc aRuleName* (aParamName: Stream): atype {.gcsafe.} #Compiler her does not "inject" aParamName correctly


template innerDefinitionPEG(aRuleName, aParamName: untyped,
    atype: typedesc,
    aBody: untyped) =
  proc aRuleName* (aParamName: Stream): atype {.gcsafe.} = #Compiler her does not "inject" aParamName correctly
    aBody


template isTrue(aResult: SimpleNodeBool): untyped =
  when aResult is bool:
    aResult
  else:
    aResult.hasValue


template pushStreamStatePEG =
  {.push warning[ShadowIdent]: off.}
  let lPosition {.inject.} = aStream.getPosition
  {.pop.}


template popStreamStatePEG =
  aStream.setPosition(lPosition)


template pushStackStatePEG =
  when not (lResult is bool):
    {.push warning[ShadowIdent]: off.}
    let lStackLen {.inject.} = lStack.len
    {.pop.}


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


template leftDefinitionPEGForward(aRuleName: untyped): untyped =
  innerDefinitionPEGForward(aRuleName, aStream, SimplePEGNodeObjectOption)


template leftDefinitionPEG(aRuleName: untyped,
    aBody: untyped): untyped =
  innerDefinitionPEG(aRuleName, aStream, SimplePEGNodeObjectOption):
    block leftDefinitionPEG:
      var lStack {.inject.} = newSeqOfCap[SimplePEGNodeObject](8)
      var lResult {.inject.} = Nothing[SimplePEGNodeObject]()
      aBody
      if lResult.isTrue:
        result = Just(SimplePEGNodeObject(FIsTerminal: false, FName: astToStr(
            aRuleName), FItems: lStack))
      else:
        result = Nothing[SimplePEGNodeObject]()


template voidDefinitionPEGForward(aRuleName: untyped): untyped =
  innerDefinitionPEGForward(aRuleName, aStream, bool)


template voidDefinitionPEG(aRuleName: untyped,
    aBody: untyped): untyped =
  innerDefinitionPEG(aRuleName, aStream, bool):
    block voidDefinitionPEG:
      var lResult {.inject.} = false
      aBody
      result = lResult


template pruneDefinitionPEGForward(aRuleName: untyped): untyped =
  innerDefinitionPEGForward(aRuleName, aStream, SimplePEGNodeObjectOption)

template pruneDefinitionPEG(aRuleName: untyped, aBody: untyped): untyped =
  innerDefinitionPEG(aRuleName, aStream, SimplePEGNodeObjectOption):
    block pruneDefinitionPEG:
      var lStack {.inject.} = newSeqOfCap[SimplePEGNodeObject](8)
      var lResult {.inject.} = Nothing[SimplePEGNodeObject]()
      aBody
      if lResult.isTrue:
        if 1 == lStack.len:
          result = Just(lStack[0])
        else:
          result = Just(SimplePEGNodeObject(FIsTerminal: false,
              FName: astToStr(aRuleName), FItems: lStack))
      else:
        result = Nothing[SimplePEGNodeObject]()


template anyPEG: untyped =
  block anyPEG:
    let lAny = aStream.read(1, false)
    when lResult is bool:
      lResult = lAny.hasValue
    else:
      if lAny.hasValue:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: true,
            FSlice: lAny.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


template voidPEG* (aBody: untyped): untyped =
  block voidPEG:
    pushStackStatePEG
    aBody
    popStackStatePEG


template closurePEG* (aBody: untyped): untyped =
  block closurePEG:
    while true:
      pushPEG
      aBody
      if not lResult.isTrue:
        popPEG
        break
    when lResult is bool:
      lResult = true
    else:
      lResult = Just(SimplePEGNodeObject(FIsTerminal: false,
          FName: "closurePEG"))


template plusPEG* (aBody: untyped): untyped =
  block plusPEG:
    aBody
    if lResult.isTrue:
      closurePEG:
        aBody


template optionalPEG* (aBody: untyped): untyped =
  block optionalPEG:
    pushPEG
    aBody
    if not lResult.isTrue:
      popPEG
      when lResult is bool:
        lResult = true
      else:
        lResult = Just(SimplePEGNodeObject(FIsTerminal: false,
            FName: "optionalPEG"))


template checkPEG* (aBody: untyped): untyped =
  block checkPEG:
    pushPEG
    aBody
    popPEG


template notCheckPEG* (aBody: untyped): untyped =
  block notCheckPEG:
    checkPEG:
      aBody
    when lResult is bool:
      lResult = not lResult
    else:
      if lResult.hasValue:
        lResult = Nothing[SimplePEGNodeObject]()
      else:
        lResult =
          Just(SimplePEGNodeObject(FIsTerminal: false, FName: "notCheckPEG"))


template stringOfPEG* (aBody: untyped): untyped =
  block stringOfPEG:
    when not (lResult is bool):
      pushStackStatePEG
      aBody
      if lResult.isTrue and (lStackLen < lStack.len):
        var lItems: SimplePEGNodeObjectSeq =
          newSeqOfCap[SimplePEGNodeObject](lStack.len - lStackLen)
        for lItem in lStackLen .. lStack.len.pred:
          lItems.add(lStack[lItem])
        popStackStatePEG
        lResult =
          Just(SimplePEGNodeObject(FIsTerminal: false, FName: "$",
              FItems: lItems))
        lResult.value.FName &= lResult.valuePEG
        lStack.add(lResult.value)
    else:
      aBody


template sequencePEG* (aBody, aAndBody: untyped): untyped =
  block sequencePEG:
    aBody
    if lResult.isTrue:
      aAndBody


template alternationPEG* (aBody, aOrBody: untyped): untyped =
  block alternationPEG:
    pushPEG
    aBody
    if not lResult.isTrue:
      popPEG
      aOrBody


template notTerminalPEG* (aRuleName: untyped): untyped =
  block notTerminalPEG:
    let lRuleResult = aStream.aRuleName
    when lResult is bool:
      when lRuleResult is bool:
        lResult = lRuleResult
      else:
        lResult = lRuleResult.hasValue
    else:
      when lRuleResult is bool:
        if lRuleResult:
          lResult =
            Just(SimplePEGNodeObject(FIsTerminal: false,
                                     FName: astToStr(aRulename)))
        else:
          lResult = Nothing[SimplePEGNodeObject]()
      else:
        lResult = lRuleResult
        if lResult.hasValue:
          lStack.add(lResult.value)


template terminalPEG* (aExpectedValue: string | set[char]): untyped =
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
        lResult =
          Just(SimplePEGNodeObject(FIsTerminal: true, FSlice: lTerminal.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


template terminalNoCasePEG* (aExpectedValue: string): untyped =
  block terminalPEG:
    when lResult is bool:
      lResult = aStream.stringInStringNoCase(aExpectedValue).hasValue
    else:
      let lTerminal = aStream.stringInString(aExpectedValue)
      if lTerminal.hasValue:
        lResult =
          Just(SimplePEGNodeObject(FIsTerminal: true, FSlice: lTerminal.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


include simplePEG/GRMs/WAXEYE


proc getWAXEYENim (aSimpleASTNodeRef: SimpleASTNodeRef): string =

  proc getWAXEYENim_Inner (aSimpleASTNodeRef: SimpleASTNodeRef,
      aForwardDefinitions: var string, aSpaces: int = 0): string =
    result = ""
    if not aSimpleASTNodeRef.isNil:
      var lSpaces = aSpaces
      let lChildren = aSimpleASTNodeRef.children
      case aSimpleASTNodeRef.name
      of "Definition":
        result &= "\n\n"
        case lChildren[1].name
        of "LeftArrow":
          aForwardDefinitions &= "leftDefinitionPEGForward(" &
            lChildren[0].value &
            ")\n"
          result &= "leftDefinitionPEG(" & lChildren[0].value & "):\n"
        of "VoidArrow":
          aForwardDefinitions &= "voidDefinitionPEGForward(" &
            lChildren[0].value &
            ")\n"
          result &= "voidDefinitionPEG(" & lChildren[0].value & "):\n"
        of "PruneArrow":
          aForwardDefinitions &= "pruneDefinitionPEGForward(" &
            lChildren[0].value &
            ")\n"
          result &= "pruneDefinitionPEG(" & lChildren[0].value & "):\n"
        lSpaces += 2
        for lIndex in 2 .. lChildren.len.pred:
          result &= getWAXEYENim_Inner(lChildren[lIndex],
              aForwardDefinitions,
              lSpaces)
        result &= "\n"
      of "Alternation":
        if lChildren.len > 1:
          result &= " ".repeat(lSpaces) & "alternationPEG:\n"
          result &= getWAXEYENim_Inner(lChildren[0],
              aForwardDefinitions,
              lSpaces + 2)
          for lIndex in 1 .. lChildren.len.pred(2):
            result &= "\n"
            result &= " ".repeat(lSpaces) & "do:\n"
            lSpaces += 2
            result &= " ".repeat(lSpaces) & "alternationPEG:\n"
            result &= getWAXEYENim_Inner(lChildren[lIndex],
                aForwardDefinitions,
                lSpaces + 2)
          result &= "\n"
          result &= " ".repeat(lSpaces) & "do:\n"
          result &= getWAXEYENim_Inner(lChildren[lChildren.len.pred],
              aForwardDefinitions,
              lSpaces + 2)
        else:
          result &= getWAXEYENim_Inner(lChildren[0], aForwardDefinitions,
              lSpaces)
      of "Sequence":
        if lChildren.len > 1:
          result &= " ".repeat(lSpaces) & "sequencePEG:\n"
          result &= getWAXEYENim_Inner(lChildren[0],
              aForwardDefinitions,
              lSpaces + 2)
          for lIndex in 1 .. lChildren.len.pred(2):
            result &= "\n"
            result &= " ".repeat(lSpaces) & "do:\n"
            lSpaces += 2
            result &= " ".repeat(lSpaces) & "sequencePEG:\n"
            result &= getWAXEYENim_Inner(lChildren[lIndex],
                aForwardDefinitions,
                lSpaces + 2)
          result &= "\n"
          result &= " ".repeat(lSpaces) & "do:\n"
          result &= getWAXEYENim_Inner(lChildren[lChildren.len.pred],
              aForwardDefinitions,
              lSpaces + 2)
        else:
          result &= getWAXEYENim_Inner(lChildren[0],
              aForwardDefinitions,
              lSpaces)
      of "Unit":
        var lStartIndex = 0
        if lChildren[0].name == "Prefix":
          lStartIndex = 1
          case lChildren[0].children[0].name
          of "?":
            result &= " ".repeat(lSpaces) & "optionalPEG:\n"
          of "*":
            result &= " ".repeat(lSpaces) & "closurePEG:\n"
          of "+":
            result &= " ".repeat(lSpaces) & "plusPEG:\n"
          of ":":
            result &= " ".repeat(lSpaces) & "voidPEG:\n"
          of "&":
            result &= " ".repeat(lSpaces) & "checkPEG:\n"
          of "!":
            result &= " ".repeat(lSpaces) & "notCheckPEG:\n"
          of "$":
            result &= " ".repeat(lSpaces) & "stringOfPEG:\n"
          lSpaces += 2
        for lIndex in lStartIndex .. lChildren.len.pred:
          result &= getWAXEYENim_Inner(lChildren[lIndex],
              aForwardDefinitions,
              lSpaces)
      of "WildCard":
        result &= " ".repeat(lSpaces) & "anyPEG"
      of "Literal":
        result &= " ".repeat(lSpaces) &
            "\"" &
            aSimpleASTNodeRef.value.replace("\\'", "'").replace("\\r",
                "\\x0D").replace("\\n", "\\x0A") &
            "\".terminalPEG"
      of "CaseLiteral":
        result &= " ".repeat(lSpaces) &
            "\"" &
            aSimpleASTNodeRef.value.replace("\\'", "'").replace("\\r",
                "\\x0D").replace("\\n", "\\x0A") &
            "\".terminalNoCasePEG"
      of "Char":
        let lValue = aSimpleASTNodeRef.value
        result = lValue
        if lValue[0] == '\\':
          case lValue[1]
          of 'n':
            result = "\\x0A"
          of 'r':
            result = "\\x0D"
          of '-':
            result = "-"
          of ']':
            result = "]"
          else:
            discard
        else:
          case lValue[0]
          of '\'':
            result = "\\'"
          else:
            discard
      of "Hex":
        result &= "\\x" & aSimpleASTNodeRef.value.toUpperAscii
      of "Range":
        if lChildren.len == 1:
          result &= "'" &
              getWAXEYENim_Inner(lChildren[0], aForwardDefinitions) &
              "'"
        else:
          result &= "'" &
              getWAXEYENim_Inner(lChildren[0], aForwardDefinitions) &
              "'..'" &
              getWAXEYENim_Inner(lChildren[1], aForwardDefinitions) &
              "'"
      of "CharClass":
        result &= " ".repeat(lSpaces) & "{"
        var lAdd = false
        for lChild in lChildren:
          if lAdd:
            result &= ", "
          else:
            lAdd = true
          result &= getWAXEYENim_Inner(lChild, aForwardDefinitions, lSpaces)
        result &= "}.terminalPEG"
      of "Identifier":
        result = aSimpleASTNodeRef.value
        let lParent = aSimpleASTNodeRef.parent
        if not(lParent.isNil):
          if lParent.name == "Unit":
            result = " ".repeat(lSpaces) & result & ".notTerminalPEG"
      else:
        for lChild in lChildren:
          result &= getWAXEYENim_Inner(lChild, aForwardDefinitions, lSpaces)

  result = ""
  var aForwardDefinitions = ""
  result = getWAXEYENim_Inner(aSimpleASTNodeRef, aForwardDefinitions)
  result = aForwardDefinitions & "\n" & result


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
              | MComment)
"""

  proc mainModule =
    # "a <- \"12\" b <: \"34\" \"56\"".withStream(lStream):
    #   let lSimpleASTNode = lStream.WAXEYE().asSimpleASTNode
    #   if not lSimpleASTNode.isNil:
    #     echo lSimpleASTNode.asASTStr

    WAXEYE_GRAMMAR_WAXEYE.withStream(lStream):
      let lSimpleASTNode = lStream.WAXEYE.asSimpleASTNode
      if not lSimpleASTNode.isNil:
        echo lSimpleASTNode.asASTStr
        echo lSimpleASTNode.getWAXEYENim

  mainModule() #[ 
  nim --putenv:NIM_VERBOSITY=3 cBuild release 2> log.txt
  nim cRun release > log.txt
 ]#
