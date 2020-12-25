{.experimental: "dynamicBindSym".}
import macros, tables, strutils

type Interface*[VT] = object
  vtbl*: ptr VT
  raw*: ref RootObj

proc identInfo*(node: NimNode): tuple[value: string, exported: bool] =
  case node.kind:
  of nnkIdent: return (value: node.strVal, exported: false)
  of nnkPostfix:
    node[0].expectIdent "*"
    node[1].expectKind nnkIdent
    return (value: node[1].strVal, exported: true)
  else:
    error "invalid ident node"

proc fromIdentInfo*(value: string, exported: bool): NimNode =
  result = ident value
  if exported:
    result = nnkPostfix.newTree(
      ident "*",
      result
    )

iterator paramNames(arr: openarray[NimNode]): NimNode =
  for item in arr:
    item.expectKind nnkIdentDefs
    for name in item[0..^3]:
      name.expectKind nnkIdent
      yield name

macro trait*(name: untyped, body: untyped) =
  name.expectKind nnkIdent
  body.expectKind nnkStmtList
  result = newStmtList()
  var typesec = newNimNode nnkTypeSection
  let namestr = name.strVal
  let vt_id = ident "vt" & namestr
  var vtds = newNimNode nnkRecList
  var defs = newSeq[NimNode]()
  for it in body:
    it.expectKind nnkMethodDef
    it[2].expectKind nnkEmpty
    let mid = it[0].identInfo
    var vtm = newNimNode nnkIdentDefs
    vtm.add ident mid.value
    var vtmd = newNimNode nnkProcTy
    let vtmdfp = it[3].copy()
    vtmdfp.expectMinLen 2
    let selfsym = genSym(nskParam, "self")
    vtmdfp[1].expectLen 3
    vtmdfp[1][1].expectKind nnkRefTy
    vtmdfp[1][2].expectKind nnkEmpty
    vtmdfp[1][1][0].expectIdent namestr
    vtmdfp[1][1][0] = bindSym "RootObj"
    vtmd.add vtmdfp
    if it[4].kind == nnkPragma:
      vtmd.add it[4].copy()
    else:
      vtmd.add nnkPragma.newTree ident "nimcall"
    vtm.add vtmd
    vtm.add newEmptyNode()
    vtds.add vtm

    let vfp = it[3].copy()
    vfp[1][0] = selfsym
    var vfn = nnkProcDef.newTree(
      it[0],
      newEmptyNode(),
      newEmptyNode(),
      vfp,
      newEmptyNode(),
      newEmptyNode(),
    )
    var vfnbody = newStmtList()
    var vfnbodycall = newNimNode nnkCall
    vfnbodycall.add nnkDotExpr.newTree(
      nnkDotExpr.newTree(
        selfsym,
        ident "vtbl"
      ),
      ident mid.value
    )
    vfnbodycall.add nnkDotExpr.newTree(
      selfsym,
      ident "raw"
    )
    for param in vtmdfp[2..^1].paramNames:
      vfnbodycall.add param
    vfnbody.add vfnbodycall
    vfn.add vfnbody
    defs.add vfn
  typesec.add nnkTypeDef.newTree(
    nnkPragmaExpr.newTree(
      vt_id,
      nnkPragma.newTree(ident "pure")
    ),
    newEmptyNode(),
    nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode(),
      vtds
    )
  )
  typesec.add nnkTypeDef.newTree(
    name,
    newEmptyNode(),
    nnkBracketExpr.newTree(
      bindSym "Interface",
      vt_id
    )
  )
  result.add typesec
  result.add defs

proc resolveTypeDesc(T: NimNode): NimNode =
  let impl = getTypeImpl T
  assert impl.kind == nnkBracketExpr
  impl[0].expectKind nnkSym
  assert impl[0].strVal == "typeDesc"
  impl[1]

proc vtType(T: NimNode): NimNode {.compileTime.} =
  let impl = getTypeImpl resolveTypeDesc T
  impl.expectKind nnkObjectTy
  return impl[2][0][1][0]

proc vtDefinition(impl: NimNode): OrderedTable[string, tuple[sym, def: NimNode]] {.compileTime.} =
  impl.expectKind nnkObjectTy
  for item in impl[2]:
    item.expectKind nnkIdentDefs
    item.expectLen 3
    item[0].expectKind nnkSym
    item[1].expectKind nnkProcTy
    result[item[0].strVal] = (sym: item[0], def: item[1])

proc implRefObject(clazz, iface, body: NimNode): NimNode =
  let namestr = clazz.strVal
  let impl_id = ident "impl" & iface.strVal & "For" & namestr
  let cvt_id = ident "to" & iface.strVal
  let ifaceT = iface.vtType()
  var defs = ifaceT.getTypeImpl().vtDefinition

  result = newStmtList()
  result.add quote do:
    var `impl_id`: `ifaceT`
  var staticblock = newStmtList()
  for def in body:
    def.expectKind nnkMethodDef
    let name = def[0].identInfo.value
    let defsym = defs[name].sym
    var params = def[3].copy()
    params.expectMinLen 2
    params[1].expectLen 3
    params[1][1].expectKind nnkRefTy
    params[1][2].expectKind nnkEmpty
    params[1][1][0].expectIdent namestr
    params[1][1][0] = bindSym "RootObj"
    let selfname = params[1][0]
    def[6].expectKind nnkStmtList
    var xbody = newStmtList()
    xbody.add quote do:
      let `selfname` {.used.} = cast[ref `clazz`](`selfname`)
    for item in def[6]:
      xbody.add item
    var dlam = nnkLambda.newTree(
      newEmptyNode(),
      newEmptyNode(),
      newEmptyNode(),
      params,
      def[4].copy(),
      newEmptyNode(),
      xbody
    )
    staticblock.add quote do:
      `impl_id`.`defsym` = `dlam`
    defs.del name
  if defs.len != 0:
    var s = "some function not defined in impl block: \n"
    for k, v in defs:
      s &= "undefined reference to $1: $2\n".format(k, v.def.repr)
    error s.strip()
  result.add quote do:
    `staticblock`
    converter `cvt_id`*(self: ref `clazz`): ref `iface` =
      new result
      result[].vtbl = addr `impl_id`
      result[].raw = self

macro impl*(clazz: typed, iface: typed, body: untyped) =
  clazz.expectKind nnkSym
  iface.expectKind nnkSym
  body.expectKind nnkStmtList
  let clazztype = clazz.resolveTypeDesc().getType()
  echo treeRepr clazztype
  if clazztype.kind == nnkObjectTy:
    if clazztype[1] != bindSym "RootObj":
      error "require inherited with RootObj"
    return implRefObject(clazz, iface, body)
  else:
    error "Invalid class type: " & $clazztype.kind