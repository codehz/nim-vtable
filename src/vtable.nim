import macros, tables, strutils, options

export isSome, get, some

type Interface*[VT] = object
  vtbl*: ptr VT
  raw*: ref RootObj

type InputIdentInfo = object
  name: string
  params: seq[NimNode]

proc `==`(a, b: InputIdentInfo): bool =
  if a.name != b.name: return false
  # FIXME: compare deeper
  return true

proc generateGenericParams(info: InputIdentInfo): NimNode =
  if info.params.len == 0:
    return newEmptyNode()
  result = nnkGenericParams.newNimNode()
  for param in info.params:
    param.expectKind nnkExprColonExpr
    result.add nnkIdentDefs.newTree(
      param[0],
      param[1],
      newEmptyNode()
    )

proc generateGenericBracket(info: InputIdentInfo, id: NimNode): NimNode =
  if info.params.len == 0: return id
  result = nnkBracketExpr.newNimNode()
  result.add id
  for param in info.params:
    result.add param[0]

proc parseInputIdentInfo(node: NimNode): InputIdentInfo =
  case node.kind:
  of nnkIdent: return InputIdentInfo(name: node.strVal, params: @[])
  of nnkBracketExpr: return InputIdentInfo(name: node[0].strVal, params: node[1..^1])
  else: error "invalid ident node"

proc definedIdentInfo(node: NimNode): tuple[value: string, exported: bool] =
  case node.kind:
  of nnkIdent: return (value: node.strVal, exported: false)
  of nnkPostfix:
    node[0].expectIdent "*"
    node[1].expectKind nnkIdent
    return (value: node[1].strVal, exported: true)
  else:
    error "invalid ident node"

iterator paramNames(arr: openarray[NimNode]): NimNode =
  for item in arr:
    item.expectKind nnkIdentDefs
    for name in item[0..^3]:
      name.expectKind nnkIdent
      yield name

macro trait*(name: untyped, body: untyped) =
  let nameidinfo = parseInputIdentInfo(name)
  body.expectKind nnkStmtList
  result = newStmtList()
  var typesec = newNimNode nnkTypeSection
  let namestr = nameidinfo.name
  let namegen = nameidinfo.generateGenericParams
  let vt_id = ident "vt" & namestr
  var vtds = newNimNode nnkRecList
  var defs = newSeq[NimNode]()
  for it in body:
    it.expectKind nnkMethodDef
    it[2].expectKind nnkEmpty
    let hasDefault = it[6].kind == nnkStmtList
    let mid = it[0].definedIdentInfo
    var vtm = newNimNode nnkIdentDefs
    vtm.add ident mid.value
    var vtmd = newNimNode nnkProcTy
    let vtmdfp = it[3].copy()
    vtmdfp.expectMinLen 2
    vtmdfp[1].expectLen 3
    vtmdfp[1][1].expectKind nnkRefTy
    vtmdfp[1][2].expectKind nnkEmpty
    assert vtmdfp[1][1][0].parseInputIdentInfo == name.parseInputIdentInfo
    vtmdfp[1][1][0] = bindSym "RootObj"
    vtmd.add vtmdfp
    if it[4].kind == nnkPragma:
      vtmd.add it[4].copy()
    else:
      vtmd.add nnkPragma.newTree ident "nimcall"
    if hasDefault:
      vtm.add nnkBracketExpr.newTree(
        bindSym "Option",
        vtmd
      )
    else:
      vtm.add vtmd
    vtm.add newEmptyNode()
    vtds.add vtm

    let vfp = it[3].copy()
    let selfsym = vfp[1][0]
    var vfn = nnkProcDef.newTree(
      it[0],
      newEmptyNode(),
      namegen,
      vfp,
      newEmptyNode(),
      newEmptyNode(),
    )
    let basechain = newDotExpr(newDotExpr(selfsym, ident "vtbl"), ident mid.value)
    if hasDefault:
      var vfnbodycall = newNimNode nnkCall
      vfnbodycall.add newCall(
        newDotExpr(
          basechain,
          bindSym "get"
        ),
      )
      vfnbodycall.add newDotExpr(selfsym, ident "raw")
      for param in vtmdfp[2..^1].paramNames:
        vfnbodycall.add param
      var velbody = it[6].copy()
      vfn.add nnkIfStmt.newTree(
        nnkElifBranch.newTree(
          newCall(
            newDotExpr(
              basechain,
              bindSym "isSome"
            )
          ),
          newStmtList(vfnbodycall)
        ),
        nnkElse.newTree(velbody)
      )
    else:
      var vfnbodycall = newNimNode nnkCall
      vfnbodycall.add basechain
      vfnbodycall.add newDotExpr(selfsym, ident "raw")
      for param in vtmdfp[2..^1].paramNames:
        vfnbodycall.add param
      vfn.add newStmtList(vfnbodycall)
    defs.add vfn
  typesec.add nnkTypeDef.newTree(
    nnkPragmaExpr.newTree(
      vt_id,
      nnkPragma.newTree(ident "pure")
    ),
    namegen,
    nnkObjectTy.newTree(
      newEmptyNode(),
      newEmptyNode(),
      vtds
    )
  )
  typesec.add nnkTypeDef.newTree(
    ident namestr,
    namegen,
    nnkBracketExpr.newTree(
      bindSym "Interface",
      nameidinfo.generateGenericBracket vt_id
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

proc vtDefinition(impl: NimNode): OrderedTable[string, tuple[sym: NimNode, optional: bool]] {.compileTime.} =
  impl.expectKind nnkObjectTy
  for item in impl[2]:
    item.expectKind nnkIdentDefs
    item.expectLen 3
    item[0].expectKind nnkSym
    result[item[0].strVal] = (sym: item[0], optional: (item[1].kind == nnkBracketExpr))

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
    let name = def[0].definedIdentInfo.value
    let origdef = defs[name]
    let defsym = origdef.sym
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
    if origdef.optional:
      staticblock.add quote do:
        `impl_id`.`defsym` = some `dlam`
    else:
      staticblock.add quote do:
        `impl_id`.`defsym` = `dlam`
    defs.del name
  if defs.len != 0:
    var s = "some function not defined in impl block: \n"
    var hasmand = false
    for k, v in defs:
      if not v.optional:
        hasmand = true
        s &= "undefined reference to $1\n".format(k)
    if hasmand:
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
  if clazztype.kind == nnkObjectTy:
    if clazztype[1] != bindSym "RootObj":
      error "require inherited with RootObj"
    return implRefObject(clazz, iface, body)
  else:
    error "Invalid class type: " & $clazztype.kind