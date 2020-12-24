import unittest

import vtable

trait If1:
  method action*(self: ref If1, lhs, rhs: int): int

trait If2:
  method action*(self: ref If2, lhs, rhs: int): int

type Clazz = object of RootObj

impl Clazz, If1:
  method action*(self: ref Clazz, lhs, rhs: int): int = lhs + rhs

impl Clazz, If2:
  method action*(self: ref Clazz, lhs, rhs: int): int = lhs * rhs

suite "multiple interface":
  test "if1":
    proc t1(v: ref If1, a, b: int): int = v.action(a, b)
    check 3 == t1(new Clazz, 1, 2)
    check 4 == t1(new Clazz, 2, 2)

  test "if2":
    proc t2(v: ref If2, a, b: int): int = v.action(a, b)
    check 2 == t2(new Clazz, 1, 2)
    check 4 == t2(new Clazz, 2, 2)

  test "generic":
    proc tg[T](v: ref T, a, b: int): int = v.action(a, b)
    check 3 == tg[If1](new Clazz, 1, 2)
    check 2 == tg[If2](new Clazz, 1, 2)