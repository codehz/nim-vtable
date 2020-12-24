Simple vtable for nim
======

DOCS: WIP

example usage:

```nim
trait MyInterface:
  method sayName*(self: ref MyInterface)
  method add*(self: ref MyInterface, lhs, rhs: int): int

type MyClass = object of RootObj
  name: string

proc newMyClass(name: string): ref MyClass =
  new result
  result[].name = name

impl MyClass, MyInterface:
  method sayName(self: ref MyClass) =
    echo "I'm ", self.name

  method add(self: ref MyClass, lhs, rhs: int): int =
    lhs + rhs

proc testmethod(x: ref MyInterface, a: int): int =
  x.sayName()
  x.add(a, 2)

check 3 == testmethod(newMyClass(1))
```