import macros, tables, options
import vtable/utils
export isSome, get, some

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
  echo result.repr