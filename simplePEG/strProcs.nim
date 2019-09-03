
import strutils


template equalEx*(aLeft,
                  aRight: string,
                  aCasesInsensitive: bool = false): untyped =
  when aCasesInsensitive:
    (cmpIgnoreCase(aLeft, aRight) == 0)
  else:
    (aLeft == aRight)

template noRight*(aString: string,
                  aLength: int): string =
  substr(aString, 0, aString.len - aLength - 1)
