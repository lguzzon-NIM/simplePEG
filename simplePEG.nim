
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
    FSlices: seq[SimplePEGSliceObject]

  SimplePEGSliceObject = object {. final .}
    FSimplePEG: SimplePEG
    FSlicesIndex: SimplePEGIndex
    FIndex: SimplePEGIndex
    FLength: SimplePEGLength
    FAsString: Maybe[string]

template original(aSimplePEGSlice: SimplePEGSliceObject): untyped = 
  aSimplePEGSlice.FSimplePEG.FSlices[aSimplePEGSlice.FSlicesIndex]

proc invalidate(aSimplePEGSlice: SimplePEGSliceObject): SimplePEGSliceObject =
  aSimplePEGSlice.FSimplePEG.FSlices[aSimplePEGSlice.FSlicesIndex].FAsString = Nothing[string]()
  result = aSimplePEGSlice.FSimplePEG.FSlices[aSimplePEGSlice.FSlicesIndex]
  

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
        aSimplePEGSlice.original.FAsString = Just(lString)
      else:
        result = ""
    finally:
      aSimplePEGSlice.FSimplePEG.FStream.setPosition(lOldPosition)


template withSimplePEG(aString: string; aBody: untyped) =
  let lStream : Stream = newStringStream(aString)
  if not lStream.isNil:
    var SimplePEG {.inject.} = SimplePEG(FStream:lStream, FSlices:newSeqOfCap[SimplePEGSliceObject](64))
    aBody


proc read* (aSimplePEG: SimplePEG, aLength: SimplePEGLength, aUsePeek: static[bool]): Maybe[SimplePEGSliceObject] =
  if 1>aLength: 
    result = Nothing[SimplePEGSliceObject]()
  else:
    try:
      let lBeginIndex = aSimplePEG.FStream.getPosition
      try: 
        let lEndPosition = lBeginIndex + aLength
        aSimplePEG.FStream.setPosition(lEndPosition)
        if lEndPosition == aSimplePEG.FStream.getPosition:
          result = Just(SimplePEGSliceObject(FSimplePEG:aSimplePEG, FSlicesIndex:aSimplePEG.FSlices.len, FIndex:lBeginIndex, FLength:aLength))
          aSimplePEG.FSlices.add(result.value)
        else: 
          result = Nothing[SimplePEGSliceObject]()
      except: 
        result = Nothing[SimplePEGSliceObject]()
      finally:
        when aUsePeek:
          aSimplePEG.FStream.setPosition(lBeginIndex)
        else:
          discard
    except: 
      result = Nothing[SimplePEGSliceObject]()


proc charInChars* (aSimplePEG: SimplePEG, aChars: string | set[char], aUsePeek: static[bool]): Maybe[SimplePEGSliceObject] =
  let lRead = aSimplePEG.read(sizeof(char),aUsePeek)
  if lRead.hasValue:
    let lAsString = lRead.value.asString
    if lAsString[0] in aChars:
      lRead.value.original.FAsString = Just(lAsString)
      result = Just(lRead.value.original)
    else: 
      result = Nothing[SimplePEGSliceObject]()
  else: 
    result = Nothing[SimplePEGSliceObject]()


template charInChars* (aSimplePEG: SimplePEG, aChars: string | set[char]): Maybe[SimplePEGSliceObject] =
  charInChars(aSimplePEG, aChars, false)


template charInCharsPeek* (aSimplePEG: SimplePEG, aChars: string | set[char]): Maybe[SimplePEGSliceObject] =
  aSimplePEG.charInChars(aChars, true)


proc stringInString* (aSimplePEG: SimplePEG, aString: string, aCasesInsensitive: static[bool], aUsePeek: static[bool]): Maybe[SimplePEGSliceObject] =
  let lLength = aString.len
  let lRead = aSimplePEG.read(lLength, aUsePeek)
  if lRead.hasValue:
    let lAsString = lRead.value.asString
    if equalEx(lRead.value.asString, aString, aCasesInsensitive):
      lRead.value.original.FAsString = Just(lAsString)
      result = Just(lRead.value.original)
    else: 
      result = Nothing[SimplePEGSliceObject]()
  else: 
    result = lRead


template stringInString* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  stringInString(aSimplePEG, aString, false, false)


template stringInStringNoCase* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  stringInString(aSimplePEG, aString, true, false)


template stringInStringPeekCase* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  stringInString(aSimplePEG, aString, false, true)


template stringInStringPeekNoCase* (aSimplePEG: SimplePEG, aString: string): Maybe[SimplePEGSliceObject] =
  stringInString(aSimplePEG, aString, true, true)
  
      
proc getMessage*: string =
    result = cHelloWorld



when isMainModule: 
  echo getMessage()
  "12345aAbC6".withSimplePEG do:
    echo "Peek -> " & $SimplePeg.charInCharsPeek("12")
    echo SimplePeg.FSlices[0]
    echo SimplePeg.FSlices[0].invalidate()
    echo SimplePeg.FSlices[0].asString
    echo SimplePeg.FSlices[0]
    echo "Read -> " & $SimplePeg.charInChars("12")
    echo "----"
    echo "Peek -> " & $SimplePeg.charInCharsPeek({'1', '2'})
    echo "Read -> " & $SimplePeg.charInChars({'1', '2'})
    echo "Read -> " & $SimplePEG.stringInStringNoCase("345A")
    echo "Read -> " & $SimplePEG.stringInString("AbC6")
    echo "Read -> " & $SimplePEG.stringInStringNoCase("A")
