import macros, tables

proc resolveTypeDesc*(T: NimNode): NimNode =
  let impl = getTypeImpl T
  assert impl.kind == nnkBracketExpr
  impl[0].expectKind nnkSym
  assert impl[0].strVal == "typeDesc"
  impl[1]

type InputIdentInfo* = object
  name*: string
  name_id*: NimNode
  params*: seq[NimNode]

proc `==`*(a, b: InputIdentInfo): bool =
  if a.name != b.name: return false
  # FIXME: compare deeper
  return true

proc generateGenericParams*(params: seq[NimNode]): NimNode =
  if params.len == 0:
    return newEmptyNode()
  result = nnkGenericParams.newNimNode()
  for param in params:
    param.expectKind nnkExprColonExpr
    result.add nnkIdentDefs.newTree(
      param[0],
      param[1],
      newEmptyNode()
    )

proc generateGenericBracket*(params: seq[NimNode], id: NimNode): NimNode =
  if params.len == 0: return id
  result = nnkBracketExpr.newNimNode()
  result.add id
  for param in params:
    result.add param[0]

proc parseInputIdentInfo*(node: NimNode): InputIdentInfo =
  case node.kind:
  of nnkIdent: return InputIdentInfo(
    name: node.strVal,
    name_id: node,
    params: @[])
  of nnkBracketExpr: return InputIdentInfo(name: node[0].strVal, name_id: node[
      0], params: node[1..^1])
  else: error "invalid ident node"

proc parseFromTypedIdentInfo*(node: NimNode): InputIdentInfo =
  case node.kind:
  of nnkSym: return InputIdentInfo(name: node.strVal, name_id: node, params: @[])
  of nnkBracketExpr:
    var params = newSeq[NimNode]()
    for it in node[1..^1]:
      params.add nnkExprColonExpr.newTree(
        it,
        it.resolveTypeDesc.getType
      )
    return InputIdentInfo(
      name: node[0].strVal,
      name_id: node[0],
      params: params)
  else: error "invalid ident node"

proc definedIdentInfo*(node: NimNode): tuple[value: string, exported: bool] =
  case node.kind:
  of nnkAccQuoted: return (value: node[0].strVal, exported: false)
  of nnkIdent: return (value: node.strVal, exported: false)
  of nnkPostfix:
    node[0].expectIdent "*"
    result = definedIdentInfo(node[1])
    result.exported = true
  else:
    error "invalid ident node"

iterator mapParams*(arr: openarray[NimNode]): tuple[name, desc: NimNode] =
  for item in arr:
    item.expectKind nnkIdentDefs
    for name in item[0..^3]:
      name.expectKind nnkIdent
      yield (name: name, desc: item[^2])

proc replaceAllIdent*(source: NimNode, id: string, target: NimNode) =
  for idx, child in source:
    if child.kind == nnkIdent:
      if child.strVal == id:
        source[idx] = target
    else:
      child.replaceAllIdent id, target

proc replaceAllSymbol*(source: NimNode, sym: string, target: NimNode) =
  for idx, child in source:
    case child.kind:
    of nnkBracketExpr:
      # fix for typechecked node
      let tmp = nnkBracketExpr.newNimNode()
      for sub in child:
        if sub.kind == nnkSym and sub.strVal == sym: tmp.add target
        else: tmp.add sub
      source[idx] = tmp
    of nnkSym:
      if child.strVal == sym:
        source[idx] = target
    else:
      child.replaceAllSymbol sym, target

proc vtType*(T: NimNode): NimNode {.compileTime.} =
  let impl = getTypeImpl resolveTypeDesc T
  impl.expectKind nnkObjectTy
  return impl[2][0][1][0]

proc vtDefinition*(impl: NimNode):
  OrderedTable[string, tuple[sym: NimNode, optional: bool]] {.compileTime.} =
  impl.expectKind nnkObjectTy
  for item in impl[2]:
    item.expectKind nnkIdentDefs
    item.expectLen 3
    item[0].expectKind nnkSym
    result[item[0].strVal] = (
      sym: item[0],
      optional: (item[1].kind == nnkBracketExpr))

proc concatParams*(a, b: seq[NimNode]): seq[NimNode] =
  result = newSeq[NimNode]()
  for it in a:
    result.add it
  for it in b:
    var matched = false
    for oth in a:
      if oth[0] == it[0]:
        matched = true
        break
    if matched: break
    result.add it

proc `==`*(a, b: seq[NimNode]): bool =
  if a.len != b.len: return false
  for i in 0..<a.len:
    if a[i][0] != b[i][0]:
      return false
  return true
