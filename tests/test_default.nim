import unittest

import vtable

trait DefaultImpl:
  method normal*(self: ref DefaultImpl): int
  method optional*(self: ref DefaultImpl): int =
    self.normal() + 1

type UseDefault = object of RootObj

impl UseDefault, DefaultImpl:
  method normal*(self: ref UseDefault): int = 1

type DefAll = object of RootObj

impl DefAll, DefaultImpl:
  method normal*(self: ref DefAll): int = 5
  method optional*(self: ref DefAll): int = 8

suite "default implemention test":
  test "use default":
    var obj = new UseDefault
    check obj.normal() == 1
    check obj.optional() == 2
  test "define all":
    var obj = new DefAll
    check obj.normal() == 5
    check obj.optional() == 8