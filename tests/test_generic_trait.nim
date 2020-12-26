import unittest

import vtable

trait Generic[T: typed]:
  method getTypeName*(self: ref Generic[T]): string

type Simple = object of RootObj

forall do (T: typed):
  impl Simple, Generic[T]:
    method getTypeName*(self: ref Simple): string = $T

suite "generic trait":
  test "simple":
    proc t(T: typedesc, gen: ref Generic[T]): string {.nimcall.} = gen.getTypeName()
    var sim = new Simple
    check t(int, toGeneric[int] sim) == "int"