
import strutils

import simpleAST

import simplePEG/typeEx
import simplePEG/strProcs



type
  SimplePEGIndex = int
  SimplePEGLength = int

  SimplePegInputStream = ref SimplePegInputStreamObject
  SimplePegInputStreamObject {.acyclic, final.} = object
    fIndex: SimplePEGIndex
    fString: string

  SimplePEGSliceObject {.acyclic, final.} = object
    fStream: SimplePegInputStream
    fIndex: SimplePEGIndex
    fLength: SimplePEGLength
    fAsString: string

  SimplePEGNodeObjectSeq = seq[SimplePEGNodeObject]
  SimplePEGNodeObject {.acyclic, final.} = object
    case fIsTerminal: bool
    of true:
      fSlice: SimplePEGSliceObject
    else:
      fName: string
      fItems: SimplePEGNodeObjectSeq

  SimplePEGSliceObjectOption = Maybe[SimplePEGSliceObject]
  SimplePEGNodeObjectOption = Maybe[SimplePEGNodeObject]
  SimplePEGNode = SimplePEGNodeObject | SimplePEGNodeObjectOption
  SimpleNodeBool = bool | SimplePEGNodeObjectOption

  SimplePegCharSet = string | set[char]


template getPosition(aStream: SimplePegInputStream): untyped =
  aStream.fIndex


template setPosition(aStream: SimplePegInputStream,
                     aPosition: int): untyped =
  aStream.fIndex = clamp(aPosition, 0, aStream.fString.len)


template readstr(aStream: SimplePegInputStream,
                 aLength: int): untyped =
  aStream.fString.substr(aStream.fIndex,
                         aStream.fIndex + aLength.pred)


func asSimpleASTNode*(aSelf: SimplePEGNode): SimpleASTNodeRef =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if (not aSelf.hasValue):
      return nil
    let lSelf = aSelf.value
  if (lSelf.fIsTerminal):
    result = newSimpleASTNode(lSelf.fSlice.asString)
  else:
    result = newSimpleASTNode(lSelf.fName)
    if (not result.isNil):
      for lItem in lSelf.fItems:
        let lChild = lItem.asSimpleASTNode
        if (not lChild.isNil):
          discard result.addChild(lChild)
        else:
          return nil


func asString*(aSimplePEGSlice: SimplePEGSliceObject): string =
  result = aSimplePEGSlice.fAsString
  if ((0 == result.len) and (0 < aSimplePEGSlice.fLength)):
    let lStream = aSimplePEGSlice.fStream
    let lOldPosition = lStream.getPosition
    defer:
      lStream.setPosition(lOldPosition)
    lStream.setPosition(aSimplePEGSlice.fIndex)
    let lLength = aSimplePEGSlice.fLength
    let lString = lStream.readstr(lLength)
    if (lLength == lString.len):
      result = lString


func `$`(aSimplePEGNodeObject: SimplePEGNodeObject): string =

  func innerEcho(aValue: SimplePEGNodeObject,
                 aTabs: int): string =
    if (aValue.fIsTerminal):
      result = (
          """

Is Terminal
  Slice -> $1
  Slice AsString ->[$2]<-
      """ % [
              $aValue.fSlice,
              aValue.fSlice.asString]).indent(aTabs)
    else:
      result = (
          """

Is Not Terminal
  Name -> $1
  Items -> $2
      """ % [
              $aValue.fName,
              $aValue.fItems.len]).indent(aTabs)
      let lTabs = aTabs + 1
      for lItem in aValue.fItems:
        result &= innerEcho(lItem, lTabs)

  innerEcho(aSimplePEGNodeObject, 1)


func startIndex(aSelf: SimplePEGNodeObject): int =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if (not aSelf.hasValue):
      return -1
    let lSelf = aSelf.value
  if (lSelf.fIsTerminal):
    result = lSelf.fSlice.fIndex
  else:
    if (0 == lSelf.fItems.len):
      result = -1
    else:
      result = startIndex(lSelf.fItems[0])


func endIndex(aSelf: SimplePEGNode): int =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if (not aSelf.hasValue):
      return -1
    let lSelf = aSelf.value
  if (lSelf.fIsTerminal):
    result = lSelf.fSlice.fIndex + lSelf.fSlice.fLength - 1
  else:
    if (lSelf.fItems.isNil):
      result = -1
    else:
      result = endIndex(lSelf.fItems[lSelf.fItems.len.pred])


func valuePEG (aSelf: SimplePEGNode): string =
  when aSelf is SimplePEGNodeObject:
    let lSelf = aSelf
  else:
    if (not aSelf.hasValue):
      return ""
    let lSelf = aSelf.value
  if (lSelf.fIsTerminal):
    result = lSelf.fSlice.asString
  else:
    if (lSelf.fItems.len > 0):
      for lItem in lSelf.fItems:
        result &= lItem.valuePEG
    else:
      result = lSelf.fName


template withStream (aString: string,
                     aStream,
                     aBody: untyped) =
  block withStream:
    let aStream {.inject.} = SimplePegInputStream(fIndex: 0, fString: aString)
    if (not aStream.isNil):
      block withStream_Body:
        aBody


func read*(aStream: SimplePegInputStream,
           aLength: SimplePEGLength,
           aUsePeek: static[bool]): SimplePEGSliceObjectOption =
  if (1 > aLength) or aStream.isNil:
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
        if (lEndPosition == aStream.getPosition):
          result = Just(SimplePEGSliceObject(fStream: aStream,
                                             fIndex: lBeginIndex,
                                             fLength: aLength))
        else:
          result = Nothing[SimplePEGSliceObject]()
      except:
        result = Nothing[SimplePEGSliceObject]()
    except:
      result = Nothing[SimplePEGSliceObject]()


func charInChars*(aStream: SimplePegInputStream,
                  aChars: SimplePegCharSet,
                  aUsePeek: static[bool],
                  aSaveAsString: static[bool]): SimplePEGSliceObjectOption =
  var lRead = aStream.read(sizeof(char), aUsePeek)
  if (lRead.hasValue):
    let lValue = lRead.value
    let lAsString = lValue.asString
    if (lAsString[0] in aChars):
      when aSaveAsString:
        lValue.fAsString = lAsString
      result = Just(lValue)
    else:
      result = Nothing[SimplePEGSliceObject]()
  else:
    result = Nothing[SimplePEGSliceObject]()


template charInChars*(aStream: SimplePegInputStream,
                      aChars: SimplePegCharSet): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, false, false)


template charInCharsPeek*(aStream: SimplePegInputStream,
                          aChars: SimplePegCharSet): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, true, false)


template charInCharsAsString*(aStream: SimplePegInputStream,
                              aChars: SimplePegCharSet): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, false, true)


template charInCharsPeekAsString*(aStream: SimplePegInputStream,
                                  aChars: SimplePegCharSet): SimplePEGSliceObjectOption =
  aStream.charInChars(aChars, true, true)


func stringInString*(aStream: SimplePegInputStream,
    aString: string,
    aCasesInsensitive: static[bool],
    aUsePeek: static[bool],
    aSaveAsString: static[bool]): SimplePEGSliceObjectOption =
  let lLength = aString.len
  var lRead = aStream.read(lLength,
                           aUsePeek)
  if (lRead.hasValue):
    let lAsString = lRead.value.asString
    if (equalEx(lAsString,
                aString,
                aCasesInsensitive)):
      when aSaveAsString:
        lRead.value.fAsString = lAsString
      result = Just(lRead.value)
    else:
      result = Nothing[SimplePEGSliceObject]()
  else:
    result = lRead


template stringInString*(aStream: SimplePegInputStream,
                          aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         false,
                         false,
                         false)


template stringInStringNoCase*(aStream: SimplePegInputStream,
                                aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         true,
                         false,
                         false)


template stringInStringPeekCase*(aStream: SimplePegInputStream,
                                  aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         false,
                         true,
                         false)


template stringInStringPeekNoCase*(aStream: SimplePegInputStream,
                                    aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         aString,
                         true,
                         true,
                         false)


template stringInStringAsString*(aStream: SimplePegInputStream,
                                  aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         false,
                         false,
                         true)


template stringInStringNoCaseAsString*(aStream: SimplePegInputStream,
                                        aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         true,
                         false,
                         true)


template stringInStringPeekCaseAsString*(aStream: SimplePegInputStream,
                                          aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         false,
                         true,
                         true)


template stringInStringPeekNoCaseAsString*(aStream: SimplePegInputStream,
                                            aString: string): SimplePEGSliceObjectOption =
  aStream.stringInString(aString,
                         aString,
                         true,
                         true,
                         true)


template innerDefinitionPEGForward(aRuleName,
                                   aParamName: untyped,
                                   atype: typedesc) =
  #Compiler her does not "inject" aParamName correctly
  func aRuleName*(aParamName: SimplePegInputStream): atype {.gcsafe.}


template innerDefinitionPEG(aRuleName, aParamName: untyped,
                            atype: typedesc,
                            aBody: untyped) =
  #Compiler her does not "inject" aParamName correctly
  func aRuleName*(aParamName: SimplePegInputStream): atype {.gcsafe.} =
    aBody


template isTrue(aResult: SimpleNodeBool): untyped =
  when aResult is bool:
    aResult
  else:
    aResult.hasValue


template pushStreamStatePEG =
  let lPosition {.inject.} = aStream.getPosition


template popStreamStatePEG =
  aStream.setPosition(lPosition)


template pushStackStatePEG =
  when not (lResult is bool):
    let lStackLen {.inject.} = lStack.len


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
  innerDefinitionPEGForward(aRuleName,
                            aStream,
                            SimplePEGNodeObjectOption)

template leftDefinitionPEG(aRuleName: untyped,
                           aBody: untyped): untyped =
  innerDefinitionPEG(aRuleName,
                     aStream,
                     SimplePEGNodeObjectOption):
    block leftDefinitionPEG:
      var lStack {.inject.} = newSeqOfCap[SimplePEGNodeObject](8)
      var lResult {.inject.} = Nothing[SimplePEGNodeObject]()
      aBody
      if (lResult.isTrue):
        if (0 == lStack.len):
          lStack.add(SimplePEGNodeObject(fIsTerminal: true,
                                         fSlice: SimplePEGSliceObject(
                                             fAsString: "")))
        result = Just(SimplePEGNodeObject(fIsTerminal: false,
                                          fName: astToStr(aRuleName),
                                          fItems: lStack))
      else:
        result = Nothing[SimplePEGNodeObject]()


template voidDefinitionPEGForward(aRuleName: untyped): untyped =
  innerDefinitionPEGForward(aRuleName,
                            aStream,
                            bool)


template voidDefinitionPEG(aRuleName: untyped,
                           aBody: untyped): untyped =
  innerDefinitionPEG(aRuleName,
                     aStream,
                     bool):
    block voidDefinitionPEG:
      var lResult {.inject.} = false
      aBody
      result = lResult


template pruneDefinitionPEGForward(aRuleName: untyped): untyped =
  innerDefinitionPEGForward(aRuleName,
                            aStream,
                            SimplePEGNodeObjectOption)


template pruneDefinitionPEG(aRuleName: untyped,
                            aBody: untyped): untyped =
  innerDefinitionPEG(aRuleName,
                     aStream,
                     SimplePEGNodeObjectOption):
    block pruneDefinitionPEG:
      var lStack {.inject.} = newSeqOfCap[SimplePEGNodeObject](8)
      var lResult {.inject.} = Nothing[SimplePEGNodeObject]()
      aBody
      if (lResult.isTrue):
        if (1 == lStack.len):
          result = Just(lStack[0])
        else:
          if (0 == lStack.len):
            lStack.add(SimplePEGNodeObject(fIsTerminal: true,
                                           fSlice: SimplePEGSliceObject(
                                               fAsString: "")))
          result = Just(SimplePEGNodeObject(fIsTerminal: false,
                                            fName: astToStr(aRuleName),
                                            fItems: lStack))
      else:
        result = Nothing[SimplePEGNodeObject]()


template anyPEG: untyped =
  block anyPEG:
    let lAny = aStream.read(1,
                            false)
    when lResult is bool:
      lResult = lAny.hasValue
    else:
      if (lAny.hasValue):
        lResult = Just(SimplePEGNodeObject(fIsTerminal: true,
                                           fSlice: lAny.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


template voidPEG*(aBody: untyped): untyped =
  block voidPEG:
    pushStackStatePEG
    aBody
    popStackStatePEG


template closurePEG*(aBody: untyped): untyped =
  block closurePEG:
    while true:
      pushPEG
      aBody
      if (not lResult.isTrue):
        popPEG
        break
    when lResult is bool:
      lResult = true
    else:
      lResult = Just(SimplePEGNodeObject(fIsTerminal: false,
                                         fName: "closurePEG"))


template plusPEG*(aBody: untyped): untyped =
  block plusPEG:
    aBody
    if (lResult.isTrue):
      closurePEG:
        aBody


template optionalPEG*(aBody: untyped): untyped =
  block optionalPEG:
    pushPEG
    aBody
    if (not lResult.isTrue):
      popPEG
      when lResult is bool:
        lResult = true
      else:
        lResult = Just(SimplePEGNodeObject(fIsTerminal: false,
                                           fName: "optionalPEG"))


template checkPEG*(aBody: untyped): untyped =
  block checkPEG:
    pushPEG
    aBody
    popPEG


template notCheckPEG*(aBody: untyped): untyped =
  block notCheckPEG:
    checkPEG:
      aBody
    when lResult is bool:
      lResult = not lResult
    else:
      if (lResult.hasValue):
        lResult = Nothing[SimplePEGNodeObject]()
      else:
        lResult =
          Just(SimplePEGNodeObject(fIsTerminal: false,
                                   fName: "notCheckPEG"))


template stringOfPEG*(aBody: untyped): untyped =
  block stringOfPEG:
    when not (lResult is bool):
      pushStackStatePEG
      aBody
      if (lResult.isTrue and (lStackLen < lStack.len)):
        var lItems: SimplePEGNodeObjectSeq =
          newSeqOfCap[SimplePEGNodeObject](lStack.len - lStackLen)
        for lItem in lStackLen .. lStack.len.pred:
          lItems.add(lStack[lItem])
        popStackStatePEG
        lResult =
          Just(SimplePEGNodeObject(fIsTerminal: false,
                                   fName: "stringOfPEG",
                                   fItems: lItems))
        lResult.value.fName = lResult.valuePEG
        lResult.value.fItems.setLen(0)
        lStack.add(lResult.value)
    else:
      aBody


template sequencePEG*(aBody,
                      aAndBody: untyped): untyped =
  block sequencePEG:
    aBody
    if (lResult.isTrue):
      aAndBody


template alternationPEG*(aBody,
                         aOrBody: untyped): untyped =
  block alternationPEG:
    pushPEG
    aBody
    if (not lResult.isTrue):
      popPEG
      aOrBody


template notTerminalPEG*(aRuleName: untyped): untyped =
  block notTerminalPEG:
    let lRuleResult = aStream.aRuleName
    when lResult is bool:
      when lRuleResult is bool:
        lResult = lRuleResult
      else:
        lResult = lRuleResult.hasValue
    else:
      when lRuleResult is bool:
        if (lRuleResult):
          lResult =
            Just(SimplePEGNodeObject(fIsTerminal: false,
                                     fName: astToStr(aRulename)))
        else:
          lResult = Nothing[SimplePEGNodeObject]()
      else:
        lResult = lRuleResult
        if (lResult.hasValue):
          lStack.add(lResult.value)


template terminalPEG*(aExpectedValue: SimplePegCharSet): untyped =
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
      if (lTerminal.hasValue):
        lResult =
          Just(SimplePEGNodeObject(fIsTerminal: true,
                                   fSlice: lTerminal.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


template terminalNoCasePEG*(aExpectedValue: string): untyped =
  block terminalPEG:
    when lResult is bool:
      lResult = aStream.stringInStringNoCase(aExpectedValue).hasValue
    else:
      let lTerminal = aStream.stringInString(aExpectedValue)
      if (lTerminal.hasValue):
        lResult =
          Just(SimplePEGNodeObject(fIsTerminal: true,
                                   fSlice: lTerminal.value))
        lStack.add(lResult.value)
      else:
        lResult = Nothing[SimplePEGNodeObject]()


include simplePEG/GRMs/WAXEYE




func getWAXEYENim (aSimpleASTNodeRef: SimpleASTNodeRef): string =

  func getWAXEYENim_Inner (aSimpleASTNodeRef: SimpleASTNodeRef,
                           aForwardDefinitions: var string,
                           aSpaces: int = 0): string =
    if (not aSimpleASTNodeRef.isNil):
      var lSpaces = aSpaces
      let lChildren = aSimpleASTNodeRef.children
      case aSimpleASTNodeRef.name
      of "Definition":
        result &= "\n\n"
        let lValue = lChildren[0].value
        let lArrowType = lChildren[1].name.noRight("Arrow".len).toLower
        aForwardDefinitions &= "$2DefinitionPEGForward($1)\n" % [lValue, lArrowType]
        result &= "$1.$2DefinitionPEG:\n" % [lValue, lArrowType]
        lSpaces += 2
        for lIndex in 2 .. lChildren.len.pred:
          result &= getWAXEYENim_Inner(lChildren[lIndex],
                                       aForwardDefinitions,
                                       lSpaces)
        result &= "\n"
      of "Alternation":
        if (lChildren.len > 1):
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
          result &= getWAXEYENim_Inner(lChildren[0],
                                       aForwardDefinitions,
                                       lSpaces)
      of "Sequence":
        if (lChildren.len > 1):
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
        if ("Prefix" == lChildren[0].name):
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
        if (lValue[0] == '\\'):
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
        if (lChildren.len == 1):
          result &= "'" &
              getWAXEYENim_Inner(lChildren[0],
                                 aForwardDefinitions) &
              "'"
        else:
          result &= "'" &
              getWAXEYENim_Inner(lChildren[0],
                                 aForwardDefinitions) &
              "'..'" &
              getWAXEYENim_Inner(lChildren[1],
                                 aForwardDefinitions) &
              "'"
      of "CharClass":
        result &= " ".repeat(lSpaces) & "{"
        var lAdd = false
        for lChild in lChildren:
          let lChild_Inner = getWAXEYENim_Inner(lChild,
                                                aForwardDefinitions,
                                                lSpaces)
          if (lAdd):
            result &= ", "
          else:
            lAdd = true
          result &= lChild_Inner
        result &= "}.terminalPEG"
      of "Identifier":
        result = aSimpleASTNodeRef.value
        let lParent = aSimpleASTNodeRef.parent
        if (not(lParent.isNil)):
          if ("Unit" == lParent.name):
            result = " ".repeat(lSpaces) & result & ".notTerminalPEG"
      else:
        for lChild in lChildren:
          result &= getWAXEYENim_Inner(lChild,
                                       aForwardDefinitions,
                                       lSpaces)

  var aForwardDefinitions = ""
  result = getWAXEYENim_Inner(aSimpleASTNodeRef,
                              aForwardDefinitions)
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
            |  !'\\' !EndOfLine .
CharClass   <- :'[' *( !']' Range ) :']' Ws
Range       <- ( Char | Hex ) ?( :'-' ( Char | Hex ) )
Char        <- '\\' [nrt\-\]\\]
            |  !'\\' !']' !EndOfLine .
Hex         <- :'\\<' [0-9A-Fa-f] [0-9A-Fa-f] :'>'
WildCard    <- :'.' Ws
Arrow       <= :'<' ( LeftArrow
                    |  PruneArrow
                    |  VoidArrow )
LeftArrow   <- :'-' Ws
PruneArrow  <- :'=' Ws
VoidArrow   <- :':' Ws
Alt         <: '|' Ws
Open        <: '(' Ws
Close       <: ')' Ws
SComment    <: '#' *( !EndOfLine . ) ( EndOfLine
                                     | !. )
MComment    <: '/*' *( MComment
                     | !'*/' . ) '*/'
EndOfLine   <: '\r' ?'\n'
            |  '\n'
Ws          <: *( :[ \t]
                |  EndOfLine
                |  SComment
                |  MComment)
"""
  # when NimVersion > "0.18.0":
  #   {.experimental: "notnil".}

  # type
  #     MainModulTest1NodeRef* = ref MainModulTest1Object
  #     MainModulTest1Node* = MainModulTest1NodeRef not nil
  #     MainModulTest1Object* = object
  #       FName: string

  # proc mainModulTest1(aName: string = ""):MainModulTest1Node {. inline .}=
  #   result = MainModulTest1Node()
  #   result.Fname = aName

  # proc mainModulTest2:string =
  #   for index in [1,2,3]:
  #     echo "[$1]" % $index
  #     result &= mainModulTest1().FName

  # proc mainModule =
  #   let B = mainModulTest2()
  #   echo "[$1]" % B

  # proc mainModule =
  #   WAXEYE_GRAMMAR_WAXEYE.withStream(lStream):
  #     # echo "<>$1<>" % lStream.readstr(0)
  #     echo "<>$1<>" % lStream.fString.substr(0,-1)
  #     echo "<>$1<>" % lStream.fString.substr(0,0)
  #     echo "<>$1<>" % lStream.fString.substr(0,1)


  proc mainModule =
  #   # "a <- \"12\" b <: \"34\" \"56\"".withStream(lStream):
  #   #   let lSimpleASTNode = lStream.WAXEYE().asSimpleASTNode
  #   #   if (not lSimpleASTNode.isNil):
  #   #     echo lSimpleASTNode.asASTStr

    WAXEYE_GRAMMAR_WAXEYE.withStream(lStream):
      let lWAXEYE = lStream.WAXEYE
      # echo $lWAXEYE
      let lSimpleASTNode = lWAXEYE.asSimpleASTNode
      if (not lSimpleASTNode.isNil):
        # echo lSimpleASTNode.asASTStr
        echo lSimpleASTNode.getWAXEYENim

  mainModule()
  #[
  nim --putenv:NIM_VERBOSITY=3 cBuild release 2> log.txt
  nim cRun release > log.txt
  ]#
