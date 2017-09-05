

type
  Maybe*[T] = tuple
    hasValue: bool
    value: T

proc Just*[T](value: T): Maybe[T] =
  result.hasValue = true
  result.value = value

proc Nothing*[T]: Maybe[T] =
  result.hasValue = false

