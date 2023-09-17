
type
  Maybe*[T] {.final.} = object
    case hasValue*: bool
    of true: value*: T
    else: discard


template Just*[T](aValue: T): Maybe[T] =
  Maybe[T](hasValue: true,
            value: aValue)

func Nothing*[T]: Maybe[T] {.inline.} =
  Maybe[T](hasValue: false)
