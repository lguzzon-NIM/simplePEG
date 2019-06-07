
import strutils


template equalEx* (aLeft, aRight: string, aCasesInsensitive: bool = false): untyped = 
  when aCasesInsensitive:
    (cmpIgnoreCase(aLeft, aRight) == 0)
  else:
    (aLeft == aRight)
