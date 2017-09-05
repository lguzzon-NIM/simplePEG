
import streams

import simplePEG.typeEx
import simplePEG.strProcs
import simplePEG.consts

type
  simplePEG = tuple
    FStream : Stream


template withSimplePEG(aString: string; aBody: untyped) =
  let lStream : Stream = newStringStream(aString)
  if not lStream.isNil:
    let SimplePEG {.inject.} = (FStream:lStream)
    aBody


proc readChar* (aSimplePEG: simplePEG): Maybe[char] =
  result.hasValue = (aSimplePEG.FStream.readData(addr(result.value), sizeof(result.value)) == 1)


proc readCharInChars* (aSimplePEG: simplePEG, aChars: string): Maybe[char] =
  result = aSimplePEG.readChar
  result.hasValue = result.hasValue and (result.value in aChars)


proc readCharInChars* (aSimplePEG: simplePEG, aChars: set[char]): Maybe[char] =
  result = aSimplePEG.readChar
  result.hasValue = result.hasValue and (result.value in aChars)


proc peekChar* (aSimplePEG: simplePEG): Maybe[char] =
  result.hasValue = (aSimplePEG.FStream.peekData(addr(result.value), sizeof(result.value)) == 1)


proc peekCharInChars* (aSimplePEG: simplePEG, aChars: string): Maybe[char] =
  result = aSimplePEG.peekChar
  result.hasValue = result.hasValue and (result.value in aChars)


proc peekCharInChars* (aSimplePEG: simplePEG, aChars: set[char]): Maybe[char] =
  result = aSimplePEG.peekChar
  result.hasValue = result.hasValue and (result.value in aChars)


proc readStringInString* (aSimplePEG: simplePEG, aString: string, aCasesInsensitive: static[bool]): Maybe[string] =
  let lLength = aString.len
  result.value = aSimplePEG.FStream.readstr(lLength)
  result.hasValue = (result.value.len == lLength) and (equalEx(result.value, aString, aCasesInsensitive))
  if result.hasValue: result.value.shallowCopy(aString)


template readStringInStringNoCase(aSimplePEG: simplePEG, aString: string): Maybe[string] =
  readStringInString(aSimplePEG, aString, true)


template readStringInString* (aSimplePEG: simplePEG, aString: string): Maybe[string] =
  readStringInString(aSimplePEG, aString, false)
  

proc getMessage*: string =
    result = cHelloWorld



when isMainModule: 
    echo getMessage()
    var lSimplePEG: simplePEG; 
    "12345aAbC6".withSimplePEG do:
      echo SimplePeg.peekCharInChars("12")
      echo SimplePeg.readCharInChars("12")
      echo SimplePeg.peekCharInChars({'1', '2'})
      echo SimplePeg.readCharInChars({'1', '2'})
      echo SimplePEG.readStringInStringNoCase("345A")
      lSimplePEG = SimplePEG
    echo lSimplePEG.readStringInStringNoCase("AbC6")
    echo lSimplePEG.readStringInStringNoCase("A")
