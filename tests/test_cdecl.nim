import unittest

import vtable

trait CallConv:
  method nimconv*(self: ref CallConv)
  method cdecl*(self: ref CallConv) {.cdecl.}

type Obj = object of RootObj

impl Obj, CallConv:
  method nimconv(self: ref Obj) {.nimcall.} =
    echo "nimcall"
  method cdecl(self: ref Obj) {.cdecl.} =
    echo "cdecl"

suite "Test callconv":
  test "placeholder":
    var obj = new Obj
    obj.nimconv()
    obj.cdecl()