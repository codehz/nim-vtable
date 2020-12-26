import macros, tables, strutils, options
export isSome, get, some

proc resolveTypeDesc(T: NimNode): NimNode =
  let impl = getTypeImpl T
  assert impl.kind == nnkBracketExpr
  impl[0].expectKind nnkSym
  assert impl[0].strVal == "typeDesc"
  impl[1]

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

proc generateGenericParams(params: seq[NimNode]): NimNode =
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

proc generateGenericBracket(params: seq[NimNode], id: NimNode): NimNode =
  if params.len == 0: return id
  result = nnkBracketExpr.newNimNode()
  result.add id
  for param in params:
    result.add param[0]

proc parseInputIdentInfo(node: NimNode): InputIdentInfo =
  case node.kind:
  of nnkIdent: return InputIdentInfo(name: node.strVal, params: @[])
  of nnkBracketExpr: return InputIdentInfo(name: node[0].strVal, params: node[1..^1])
  else: error "invalid ident node"

proc parseFromTypedIdentInfo(node: NimNode): InputIdentInfo =
  case node.kind:
  of nnkSym: return InputIdentInfo(name: node.strVal, params: @[])
  of nnkBracketExpr:
    var params = newSeq[NimNode]()
    for it in node[1..^1]:
      params.add nnkExprColonExpr.newTree(
        it,
        it.resolveTypeDesc.getType
      )
    return InputIdentInfo(name: node[0].strVal, params: params)
  else: error "invalid ident node"

proc definedIdentInfo(node: NimNode): tuple[value: string, exported: bool] =
  case node.kind:
  of nnkAccQuoted: return (value: node[0].strVal, exported: false)
  of nnkIdent: return (value: node.strVal, exported: false)
  of nnkPostfix:
    node[0].expectIdent "*"
    result = definedIdentInfo(node[1])
    result.exported = true
  else:
    error "invalid ident node"

iterator mapParams(arr: openarray[NimNode]): tuple[name, desc: NimNode] =
  for item in arr:
    item.expectKind nnkIdentDefs
    for name in item[0..^3]:
      name.expectKind nnkIdent
      yield (name: name, desc: item[^2])

proc replaceAllIdent(source: NimNode, id: string, target: NimNode) =
  for idx, child in source:
    if child.kind == nnkIdent:
      if child.strVal == id:
        source[idx] = target
    else:
      child.replaceAllIdent id, target

proc replaceAllSymbol(source: NimNode, sym: string, target: NimNode) =
  for idx, child in source:
    case child.kind:
    of nnkBracketExpr:
      # fix for typechecked node
      let tmp = nnkBracketExpr.newNimNode()
      for sub in child:
        if sub.strVal == sym: tmp.add target
        else: tmp.add sub
      source[idx] = tmp
    of nnkSym:
      if child.strVal == sym:
        source[idx] = target
    else:
      child.replaceAllSymbol sym, target

macro forall*(body: untyped) =
  body.expectKind nnkDo
  body[0].expectKind nnkEmpty
  body[1].expectKind nnkEmpty
  body[2].expectKind nnkEmpty
  body[3].expectKind nnkFormalParams
  body[3][0].expectKind nnkEmpty
  body[4].expectKind nnkEmpty
  body[5].expectKind nnkEmpty
  body[6].expectKind nnkStmtList
  result = newStmtList()
  let typesec = nnkTypeSection.newNimNode()
  let xbody = body[6].copy()
  for def in body[3][1..^1].mapParams():
    let tmp = genSym(nskType, def.name.strVal)
    replaceAllIdent(xbody, def.name.strVal, tmp)
    typesec.add nnkTypeDef.newTree(
      tmp,
      newEmptyNode(),
      def.desc
    )
  result.add typesec
  result.add xbody

macro trait*(name: untyped{nkIdent | nkBracketExpr}, body: untyped{nkStmtList}) =
  let nameidinfo = parseInputIdentInfo(name)
  body.expectKind nnkStmtList
  result = newStmtList()
  var typesec = newNimNode nnkTypeSection
  let namestr = nameidinfo.name
  let namegen = nameidinfo.params.generateGenericParams
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
      for param in vtmdfp[2..^1].mapParams:
        vfnbodycall.add param.name
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
      for param in vtmdfp[2..^1].mapParams:
        vfnbodycall.add param.name
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
      nameidinfo.params.generateGenericBracket vt_id
    )
  )
  result.add typesec
  result.add defs

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

proc concatParams(a, b: seq[NimNode]): seq[NimNode] =
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

proc `==`(a, b: seq[NimNode]): bool =
  if a.len != b.len: return false
  for i in 0..<a.len:
    if a[i][0] != b[i][0]:
      return false
  return true

macro impl*(clazz: typed{nkSym | nkBracketExpr}, iface: typed{nkSym | nkBracketExpr}, body: untyped{nkStmtList}) =
  let clazzinfo = clazz.parseFromTypedIdentInfo
  let ifaceinfo = iface.parseFromTypedIdentInfo
  let namestr = clazzinfo.name
  let impl_id = ident "impl" & ifaceinfo.name & "For" & namestr
  let cvt_id = ident "to" & ifaceinfo.name
  let combinedparams = concatParams(ifaceinfo.params, clazzinfo.params)
  let ifaceT = iface.vtType()
  var defs = ifaceT.getTypeImpl().vtDefinition

  let fblock = newStmtList()
  fblock.add quote do:
    var `impl_id` {.global.}: `ifaceT`
  let onceblock = newStmtList()
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
    assert params[1][1][0].parseInputIdentInfo == clazzinfo
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
      onceblock.add quote do:
        `impl_id`.`defsym` = some `dlam`
    else:
      onceblock.add quote do:
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
  fblock.add quote do:
    once:
      `onceblock`
  let retype = if ifaceinfo.params.len == 0 or ifaceinfo.params == clazzinfo.params: nnkConverterDef else: nnkProcDef
  result = retype.newTree(
    nnkPostfix.newTree(
      ident "*",
      cvt_id
    ),
    newEmptyNode(),
    combinedparams.generateGenericParams(),
    nnkFormalParams.newTree(
      nnkRefTy.newTree(iface),
      nnkIdentDefs.newTree(
        ident "self",
        nnkRefTy.newTree(clazz),
        newEmptyNode()
      )
    ),
    newEmptyNode(),
    newEmptyNode()
  )
  result.add quote do:
    `fblock`
    new result
    result[].vtbl = addr `impl_id`
    result[].raw = self
  if combinedparams.len != 0:
    for param in combinedparams:
      let gen = ident param[0].strVal & "`gen"
      result.replaceAllSymbol param[0].strVal, gen