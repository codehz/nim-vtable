import unittest

import vtable

trait MyInterface:
  method sayName*(self: ref MyInterface)
  method add*(self: ref MyInterface, lhs, rhs: int): int

type MyClass = object of RootObj
  name: string

proc newMyClass(name: string): ref MyClass =
  new result
  result[].name = name

type MyClass2 = object of RootObj
  first_name, last_name: string

proc newMyClass2(first_name, last_name: string): ref MyClass2 =
  new result
  result[].first_name = first_name
  result[].last_name = last_name

impl MyClass, MyInterface:
  method sayName(self: ref MyClass) =
    echo "I'm ", self.name

  method add(self: ref MyClass, lhs, rhs: int): int =
    lhs + rhs

impl MyClass2, MyInterface:
  method sayName(self: ref MyClass2) =
    echo "HAHAHA ", self.first_name, self.last_name

  method add(self: ref MyClass2, lhs, rhs: int): int =
    lhs * rhs

proc testmethod(x: ref MyInterface, a: int): int =
  echo x.repr
  x.sayName()
  x.add(a, 2)

suite "basic test":
  test "simple":
    check 3 == testmethod(newMyClass("A1"), 1)
    check 4 == testmethod(newMyClass2("A1", "B2"), 2)