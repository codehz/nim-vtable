import unittest

import vtable

trait ToString:
  method `$`*(self: ref ToString): string

type Wrapper[T] = object of RootObj
  value: T

forall do (T: typed):
  impl Wrapper[T], ToString:
    method `$`*(self: ref Wrapper[T]): string = $self.value

suite "generic impl":
  test "basic":
    proc forceeval(e: ref ToString): string = $e
    var intwrap = new Wrapper[int]
    intwrap.value = 5
    check forceeval(intwrap) == "5"