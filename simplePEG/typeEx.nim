

type
  Maybe* [T] =  object {. final .}
    case hasValue*: bool
    of true: value*: T
    else: discard


proc Just* [T](aValue: T): Maybe[T] =
  result = Maybe[T](hasValue: true, value: aValue)


proc Nothing* [T]: Maybe[T] =
  result = Maybe[T](hasValue: false)

