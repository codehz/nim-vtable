import unittest

import vtable

trait Generic[T: typed]:
  method getTypeName*(self: ref Generic[T]): string

type Simple = object of RootObj

forall do (T: typed):
  impl Simple, Generic[T]:
    method getTypeName*(self: ref Simple): string = $T

type Complex[T] = object of RootObj

forall do (T: typed):
  impl Complex[T], Generic[T]:
    method getTypeName*(self: ref Complex[T]): string = $T

type Ugly[T] = object of RootObj

forall do (T, R: typed):
  impl Ugly[R], Generic[T]:
    method getTypeName*(self: ref Ugly[R]): string = static: $T & ":" & $R

suite "generic trait":
  test "simple":
    proc t(T: typedesc, gen: ref Generic[T]): string {.nimcall.} = gen.getTypeName()
    var sim = new Simple
    check t(int, toGeneric[int] sim) == "int"

  test "complex":
    proc t(T: typedesc, gen: ref Generic[T]): string {.nimcall.} = gen.getTypeName()
    var sim = new Complex[int]
    check t(int, sim) == "int"

  test "ugly":
    proc t(T: typedesc, gen: ref Generic[T]): string {.nimcall.} = gen.getTypeName()
    var sim = new Ugly[string]
    check t(int, toGeneric[int, string](sim)) == "int:string"
