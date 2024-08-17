import { BytecodeSecondOrder, compileFunctionPrototype, getCommonType, getOperatorTable, loadModule, popStack, popValues, propagateLiteralType, propagatedLiteralAst, pushBytecode, resolveScope, unknownToAst, visitParseNode } from "./compiler"
import { compileExportedFunctionTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall, insertFunctionDefinition } from "./compiler_functions"
import { Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd, ParseFold, ParseForExpr, ParseWhileExpr, Module, pushSubCompilerState, createScope, TaskContext, CompilerError, AstType, OperatorAst, CompilerFunction, CallAst, RawPointerType, SubscriptAst, IntType, expectType, SetSubscriptAst, ParserFunctionParameter, FunctionType, Binding, StringType, ValueFieldAst, LetAst, BindingAst, createStatements, StringAst, FloatType, DoubleType, CompilerFunctionCallContext, Vm, expectAst, NumberAst, Type, UserCallAst, hashValues, NeverType, IfAst, BoolType, VoidAst, LoopObject, CompileTimeObjectType, u64Type, FunctionDefinition, ParserFunctionDecl, StatementsAst, isTypeScalar, IntLiteralType, FloatLiteralType, isAst, isType, isTypeCheckError, InterleaveAst, ContinueInterAst, CompTimeObjAst, ParseEvalFunc, SetAst, DefaultConsAst, WhileAst, BoolAst, isArray, ExpansionSelector, ParseNote, ExpansionCompilerState, ParseBoolean, ParseOr, ParseBreak, filterNotNull, ParseTuple, ParseNot, ParseLetConst, ParseConcurrency, ParseCompTime, ParseNil, CompilerCallable, isCompilerCallable, ParseVoid } from "./defs"
import { Event, Task, TaskDef, isTask } from "./tasks"

const createExpansionState = (debugName: string, location: SourceLocation): ExpansionCompilerState => {
  const iteratorListIdentifier = new ParseFreshIden(createAnonymousToken(''), new FreshBindingToken('iterator_list'))
  const breakIden = new ParseFreshIden(createAnonymousToken(''), new FreshBindingToken('break'))
  const metaResultIden = new ParseFreshIden(createAnonymousToken(''), new FreshBindingToken('result_expr'))
  return { debugName, loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null, filterNode: null, whileNode: null, optimiseSimple: false, breakIden, location, lets: [], metaResult: metaResultIden, metaResultIden, loopBodyMeta: [] }
}
const canOptimiseSimpleIterator = (closure: Closure) => {
  return (closure as any)._canOptimiseSimpleIterator
}
const setOptimiseSimpleIterator = (closure: Closure) => {
  (closure as any)._canOptimiseSimpleIterator = true
}

const getLength = (token: Token, expr: ParseNode) => new ParseCall(token, new ParseCompilerIden(createAnonymousToken(''), 'lenfn'), [expr], [])

export const forLoopSugar = (out: BytecodeWriter, node: ParseFor) => {
  const expansion = createExpansionState('forin', node.token.location)
  const result = visitExpansion(out, expansion, node.expr)
  compilerAssert(!expansion.fold, "Fold not supported in for loop")

  const fnIden = node.left instanceof ParseIdentifier ? node.left : new ParseFreshIden(node.token, new FreshBindingToken('it'))
  const fnParams: ParserFunctionParameter[] = [{ name: fnIden, storage: null, type: null }]
  const extract = node.left instanceof ParseTuple ? new ParseLet(node.token, node.left, null, fnIden) : null
  const continueBlock = new ParseBlock(node.token, 'continue', null, new ParseStatements(node.token, filterNotNull([extract, node.body])))
  const decl = createAnonymousParserFunctionDecl("for", node.token, fnParams, continueBlock)
  const fn = new ParseFunction(node.token, decl)

  let iterator: ParseNode
  if (expansion.selectors.length > 0) {
    expansion.optimiseSimple = expansion.selectors.length === 1
    expansion.loopBodyNode = new ParseCall(node.token, fn, [result], [])

    iterator = compileExpansionToParseNode(out, expansion, node)
  } else {
    const iterateFn = new ParseCompilerIden(createAnonymousToken(''), 'iteratefn');
    iterator = new ParseCall(node.token, iterateFn, [node.expr], [fn])
  }

  const breakBlock = new ParseBlock(node.token, 'break', null, new ParseStatements(node.token, [iterator]))
  visitParseNode(out, breakBlock)
}
export const forExprSugar = (out: BytecodeWriter, node: ParseForExpr) => {
  compilerAssert(false, "Not implemented")
}

export const whileExprSugar = (out: BytecodeWriter, node: ParseWhileExpr) => {
  compilerAssert(false, "Not implemented")
}

export const foldSugar = (out: BytecodeWriter, node: ParseFold) => {
  const expansion = out.state.expansion
  compilerAssert(expansion, "Expected expansion locus for fold operator")
  expansion.fold = { iden: new ParseFreshIden(node.token, new FreshBindingToken('fold_iden')), initial: node.expr }
  visitParseNode(out, expansion.fold.iden)
}
export const sliceSugar = (out: BytecodeWriter, node: ParseSlice, assignValue: ParseNode | null) => {
  compilerAssert(out.state.expansion, "Expected expansion locus for slice operator")
  // const selectorIndex = out.state.expansion.selectors.length

  const indexIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('i'))
  const elemIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('elem'))
  let setterIdentifier: ParseNode | null = null
  let finalNode: ParseNode = elemIdentifier
  const selector = { node: node.expr, start: node.start, end: node.end, step: node.step, elemIdentifier, setterIdentifier, indexIdentifier }
  if (assignValue) {
    out.state.expansion.setterSelector = selector
    visitParseNode(out, assignValue)
    return
  }
  out.state.expansion.selectors.push(selector)
  visitParseNode(out, finalNode)
}

export const listComprehensionSugar = (out: BytecodeWriter, node: ParseListComp) => {
  let list: ParseNode = new ParseList(node.token, node.exprs)
  if (node.exprs.length === 1 && node.exprs[0] instanceof ParseExpand) {
    list = node.exprs[0].expr
  }
  const trx = node.mapping[0]
  const reducer = node.reduce!
  // TODO: Do this without looking up transduce in scope?
  const call = new ParseCall(node.token, new ParseIdentifier(createAnonymousToken('transduce')), [list], [trx, reducer])
  visitParseNode(out, call)
}
class ArrayConstructorCompiler {
  calls: Ast[] = []
  constructorBinding: Binding | null = null
  arrayConstructor: Ast | null = null
  elemType: Type | null = null
  elemTypes: Type[] = []
  constructorCreated: Event<{}, CompilerError> = new Event()
  constructor(public numExprs: number) {}
}
const createArrayConstructorCompiler = new ExternalFunction('createArrayConstructorCompiler', VoidType, (ctx, values) => {
  const [numExprs] = values;
  compilerAssert(typeof numExprs === 'number', "Expected number", { numExprs })
  return new ArrayConstructorCompiler(numExprs)
})
const arrayConstructorTypeCheck = new ExternalFunction('arrayConstructorTypeCheck', VoidType, (ctx, values) => {
  const [constructor, value] = values;
  compilerAssert(constructor instanceof ArrayConstructorCompiler, "Expected array constructor", { constructor })
  compilerAssert(isAst(value), "Expected ast", { value });
  constructor.elemTypes.push(value.type)
  if (constructor.elemTypes.length < constructor.numExprs) return Task.waitFor(constructor.constructorCreated)
  constructor.elemType = getCommonType(constructor.elemTypes)
  return createConstructor(ctx.compilerState.vm, constructor)
})
const arrayConstructorCreateAppend = new ExternalFunction('arrayConstructorCreateAppend', VoidType, (ctx, values) => {
  const [constructor, value] = values;
  compilerAssert(constructor instanceof ArrayConstructorCompiler, "Expected array constructor", { constructor })
  compilerAssert(isAst(value), "Expected ast", { value });
  const binding = constructor.constructorBinding
  compilerAssert(binding, "Expected constructor binding", { constructor })
  const vm = ctx.compilerState.vm
  const call_ = createMethodCall(vm, new BindingAst(binding.type, vm.location, binding), 'append', [constructor.elemType], [value])
  return call_.chainFn((task, _) => { const ast = expectAst(vm.stack.pop()); return Task.of(ast) })
})
const arrayConstructorAddAppendCall = new ExternalFunction('arrayConstructorAddAppendCall', VoidType, (ctx, values) => {
  const [constructor, appendCall] = values;
  compilerAssert(constructor instanceof ArrayConstructorCompiler, "Expected array constructor", { constructor })
  compilerAssert(isAst(appendCall), "Expected ast", { appendCall });
  constructor.calls.push(appendCall)
  return null
})

const createConstructor = (vm: Vm, constructor: ArrayConstructorCompiler) => {
  const constructorArgs = [new NumberAst(IntType, vm.location, 0)]
  return (
    TaskDef(loadModule, vm.location, 'array')
    .chainFn((task, module_) => TaskDef(resolveScope, module_.compilerState.scope, 'array_create'))
    .chainFn((task, func) => createCallAstFromValueAndPushValue(vm, func, [constructor.elemType], constructorArgs))
    .chainFn((task, _) => {
      constructor.arrayConstructor = expectAst(vm.stack.pop())
      constructor.constructorBinding = new Binding("", constructor.arrayConstructor.type)
      constructor.constructorCreated.success({})
      return Task.of(null)
    })
  )
}

const arrayConstructorFinish = new ExternalFunction('arrayConstructorFinish', VoidType, (ctx, values) => {
  const [constructor] = values;
  compilerAssert(constructor instanceof ArrayConstructorCompiler, "Expected array constructor", { constructor })
  compilerAssert(constructor.arrayConstructor, "Expected array constructor", { constructor })
  const binding = constructor.constructorBinding
  compilerAssert(binding, "Expected list binding", { constructor })
  const let_ = new LetAst(VoidType, ctx.location, binding, constructor.arrayConstructor)
  const bindingAst = new BindingAst(binding.type, ctx.location, binding)
  return createStatements(ctx.location, [let_, ...constructor.calls, bindingAst])
})

const appendValuePartialFn = (() => {
  const token = createAnonymousToken('')
  const consIdenParam = new ParseIdentifier(createAnonymousToken('cons'))
  const exprParam = new ParseIdentifier(createAnonymousToken('expr'))

  const exprQuote = new ParseQuote(token, exprParam)
  const iden = new ParseFreshIden(token, new FreshBindingToken('elem'))
  const let_ = new ParseLet(token, iden, null, exprQuote)
  const call = new ParseCall(token, new ParseValue(token, arrayConstructorTypeCheck), [consIdenParam, iden], [])
  const call2 = new ParseCall(token, new ParseValue(token, arrayConstructorCreateAppend), [consIdenParam, iden], [])
  const call3 = new ParseCall(token, new ParseValue(token, arrayConstructorAddAppendCall), [consIdenParam, call2], [])
  const meta_ = new ParseMeta(token, new ParseStatements(token, [let_, call, call3]))
  const decl = createAnonymousParserFunctionDecl('appendValue', token, [], meta_)

  const params: ParserFunctionParameter[] = [
    { name: consIdenParam, storage: null, type: null },
    { name: exprParam, storage: null, type: null }
  ]
  return createAnonymousParserFunctionDecl('appendValuePartial', token, params, new ParseFunction(token, decl))
})()

export const listConstructorSugar = (out: BytecodeWriter, node: ParseList) => {
  const listConstructorIden = new ParseFreshIden(node.token, new FreshBindingToken('list'))
  const numExprs = node.exprs.length
  const call_ = new ParseCall(node.token, new ParseValue(node.token, createArrayConstructorCompiler), [new ParseNumber(createAnonymousToken(numExprs))], [])
  visitParseNode(out, new ParseLetConst(node.token, listConstructorIden, call_))
  pushBytecode(out, node.token, { type: 'pop' })
  const fns = node.exprs.map(expr => {
    if (expr instanceof ParseExpand) {
      const expansion = createExpansionState('loop', node.token.location)
      let result = visitExpansion(out, expansion, expr.expr)
      const iden = new ParseFreshIden(node.token, new FreshBindingToken('elem'))
      const exprQuote = new ParseQuote(node.token, result)
      const let_ = new ParseLet(node.token, iden, null, exprQuote)
      const call = new ParseCall(node.token, new ParseValue(node.token, arrayConstructorTypeCheck), [listConstructorIden, iden], [])
      const call2 = new ParseCall(node.token, new ParseValue(node.token, arrayConstructorCreateAppend), [listConstructorIden, iden], [])
      expansion.loopBodyNode = new ParseMeta(node.token, new ParseStatements(node.token, [let_, call, call2]))
      const expand = compileExpansionToParseNode(out, expansion, node)
      const expandQuote = new ParseQuote(node.token, expand)
      const call3 = new ParseCall(node.token, new ParseValue(node.token, arrayConstructorAddAppendCall), [listConstructorIden, expandQuote], [])
      const meta_ = new ParseMeta(node.token, call3)
      const fn = createAnonymousParserFunctionDecl('appendIterator', node.token, [], meta_)
      return new ParseFunction(node.token, fn)
    } else {
      return new ParseCall(node.token, new ParseFunction(node.token, appendValuePartialFn), [listConstructorIden, new ParseQuote(node.token, expr)], [])
    }
  }).filter(x => x) as ParseFunction[]

  visitParseNode(out, new ParseCompTime(node.token, new ParseConcurrency(node.token, fns)))
  visitParseNode(out, new ParseMeta(node.token, new ParseCall(node.token, new ParseValue(node.token, arrayConstructorFinish), [listConstructorIden], [])))
}

const createArrayIterator = (token: Token, subCompilerState: SubCompilerState, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }) => {

  const yieldParam = new ParseFreshIden(token, new FreshBindingToken('yield'))
  const indexIdentifier = selector.indexIdentifier
  compilerAssert(indexIdentifier)
  const fnParams: ParserFunctionParameter[] = [{ name: yieldParam, storage: null, type: null }]

  const letNodeNode = new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('node')), null, selector.node)
  let lengthNode: ParseNode = getLength(token, letNodeNode.left)
  if (selector.end) {
    const offsetNode = new ParseOperator(createAnonymousToken("+"), [lengthNode, selector.end])
    const lenCond = new ParseOperator(createAnonymousToken("<"), [selector.end, new ParseNumber(createAnonymousToken('0'))])
    lengthNode = new ParseIf(token, true, lenCond, offsetNode, new ParseElse(token, selector.end))
  }
  let letLengthNode: ParseNode = new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('length')), null, lengthNode)
  const letIndexNode = new ParseLet(token, indexIdentifier, null, selector.start ?? new ParseNumber(createAnonymousToken('0')))
  const incNode = new ParseOpEq(createAnonymousToken("+="), letIndexNode.left, selector.step ?? new ParseNumber(createAnonymousToken('1')))
  const condNode = new ParseOperator(createAnonymousToken("<"), [letIndexNode.left, letLengthNode.left])
  const subscriptIterator = new ParseSubscript(token, letNodeNode.left, indexIdentifier, false)
  const loopBody = new ParseCall(createAnonymousToken(''), yieldParam, [subscriptIterator], [])
  const whileStmts = new ParseStatements(token, [loopBody, incNode])
  const loop = new ParseWhile(token, condNode, whileStmts)

  const fnBody = new ParseStatements(createAnonymousToken(''), [letNodeNode, letIndexNode, letLengthNode, loop])
  const decl = createAnonymousParserFunctionDecl(`array_iterator`, createAnonymousToken(''), fnParams, fnBody)
  const funcDef = insertFunctionDefinition(subCompilerState.globalCompiler, decl)
  const closure = new Closure(funcDef, subCompilerState.scope, subCompilerState)
  return { closure, yieldParam, indexIdentifier, subscriptIterator }
}

const createArraySetterIterator2 = (token: Token, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }) => {
  
  const yieldParam = new ParseFreshIden(token, new FreshBindingToken('yield'))
  const valueIdentifier = new ParseFreshIden(token, new FreshBindingToken('value'))
  const indexIdentifier = selector.indexIdentifier
  compilerAssert(indexIdentifier)
  const fnParams: ParserFunctionParameter[] = [{ name: yieldParam, storage: null, type: null }]

  let lengthNode: ParseNode = getLength(token, selector.node)
  if (selector.end) {
    const offsetNode = new ParseOperator(createAnonymousToken("+"), [lengthNode, selector.end])
    const lenCond = new ParseOperator(createAnonymousToken("<"), [selector.end, new ParseNumber(createAnonymousToken('0'))])
    lengthNode = new ParseIf(token, true, lenCond, offsetNode, new ParseElse(token, selector.end))
  }
  let letLengthNode: ParseNode = new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('length')), null, lengthNode)
  const letIndexNode = new ParseLet(token, indexIdentifier, null, selector.start ?? new ParseNumber(createAnonymousToken('0')))
  const incNode = new ParseOpEq(createAnonymousToken("+="), letIndexNode.left, selector.step ?? new ParseNumber(createAnonymousToken('1')))
  const condNode = new ParseOperator(createAnonymousToken("<"), [letIndexNode.left, letLengthNode.left])
  const subscriptIterator = new ParseSubscript(token, selector.node, indexIdentifier, false)
  const loopBody = new ParseLet(token, valueIdentifier, null, new ParseCall(createAnonymousToken(''), yieldParam, [], []))
  const setNode = new ParseSet(token, subscriptIterator, valueIdentifier)
  const whileStmts = new ParseStatements(token, [loopBody, setNode, incNode])
  const loop = new ParseWhile(token, condNode, whileStmts)

  const fnBody = new ParseStatements(createAnonymousToken(''), [letIndexNode, letLengthNode, loop])
  return createAnonymousParserFunctionDecl(`array_setter_iterator`, createAnonymousToken(''), fnParams, fnBody)
}


const letIn = (token: Token, node: ParseNode, f: (iden: ParseFreshIden) => ParseNode[]) => {
  const iden = new ParseFreshIden(token, new FreshBindingToken('let_in'))
  const let_ = new ParseLet(token, iden, null, node)
  return new ParseStatements(token, [let_, ...f(iden)])
}

const metaIf = (token: Token, cond: ParseNode, then: ParseNode, else_: ParseNode | null) => new ParseMeta(token, new ParseIf(token, false, cond, new ParseQuote(token, then), 
    new ParseElse(token, else_ ? new ParseQuote(token, else_) : new ParseNil(token))))


const createIteratorSliceLoopPartialFn = (() => {
  const token = createAnonymousToken('')

  // It would be good to have some instructions in the bytecode for manipulating arrays (create/append/reduce)
  // but maybe we need to improve the metaprogramming system first
  // Also How about [... if ...] sugar?

  const consumeParam = new ParseFreshIden(token, new FreshBindingToken('consume'))
  const yieldParam = new ParseFreshIden(token, new FreshBindingToken('yield'))
  const indexIdentifier = new ParseFreshIden(token, new FreshBindingToken('index'))
  compilerAssert(indexIdentifier)
  const fnParams: ParserFunctionParameter[] = [
    { name: consumeParam, storage: null, type: null },
    { name: yieldParam, storage: null, type: null },
  ]
  const startIden = new ParseIdentifier(createAnonymousToken('start'))
  const endIden = new ParseIdentifier(createAnonymousToken('end'))
  const stepIden = new ParseIdentifier(createAnonymousToken('step'))
  // const indexIden = new ParseFreshIden(token, new FreshBindingToken('index'))

  const letStartNode = metaIf(token, startIden, 
    new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('start')), null, startIden), null)
  const letEndNode = metaIf(token, endIden,
    new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('end')), null, endIden), null)
  const letStepNode = metaIf(token, stepIden, 
    new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('step')), null, stepIden), null)
  const letIndexNode = new ParseLet(token, indexIdentifier, null, new ParseNumber(createAnonymousToken('0')))
  const incNode = new ParseOpEq(createAnonymousToken("+="), indexIdentifier, new ParseNumber(createAnonymousToken('1')))
  const consumedValue = new ParseFreshIden(token, new FreshBindingToken('slice'))
  const trueNode = new ParseBoolean(createAnonymousToken('true'))
  
  const loopCondNode = metaIf(token, endIden, new ParseOperator(createAnonymousToken("<"), [indexIdentifier, endIden]), trueNode)
  
  const yieldCondsIden = new ParseFreshIden(token, new FreshBindingToken('yield_conds'))
  const letYieldConds = new ParseLetConst(token, yieldCondsIden, new ParseNil(createAnonymousToken('')))
  const foo1 = new ParseCompTime(token, new ParseIf(token, true, startIden,
      new ParseSet(createAnonymousToken(''), yieldCondsIden, 
        new ParseQuote(token, new ParseOperator(createAnonymousToken(">="), [indexIdentifier, startIden]))), null)
  )
  const check = metaIf(token, startIden, new ParseOperator(createAnonymousToken("-"), [indexIdentifier, startIden]), indexIdentifier)
  
  const modCond = new ParseQuote(token, new ParseOperator(createAnonymousToken("=="), [
      new ParseOperator(createAnonymousToken("mod"), [check, stepIden]), 
      new ParseNumber(createAnonymousToken('0'))]))
  const modCondIf = letIn(token, modCond, (modCond) => {
    return [new ParseSet(token, yieldCondsIden, new ParseIf(token, true, yieldCondsIden, 
        new ParseQuote(token, new ParseAnd(createAnonymousToken("and"), [yieldCondsIden, modCond])), 
        new ParseElse(token, modCond)))]})
  const foo2 = new ParseCompTime(token, new ParseIf(token, false, stepIden, modCondIf, null))
  
  const yieldCall: ParseNode = new ParseCall(createAnonymousToken(''), yieldParam, [consumedValue], [])
  const wrappedYieldCall = new ParseMeta(token, letIn(token, new ParseQuote(token, yieldCall), (yieldCall) => [
    new ParseIf(token, false, yieldCondsIden, 
      new ParseQuote(token, new ParseIf(token, false, yieldCondsIden, yieldCall, null)), 
      new ParseElse(token, new ParseQuote(token, yieldCall)))
  ]))

  const consumeCall = new ParseLet(token, consumedValue, null, new ParseCall(createAnonymousToken(''), consumeParam, [], []))
  const loopBody = new ParseStatements(token, filterNotNull([consumeCall, wrappedYieldCall, incNode]))
  const loop = new ParseWhile(token, loopCondNode, loopBody)

  const fnBody = new ParseStatements(createAnonymousToken(''), filterNotNull([letStartNode, letEndNode, letStepNode, letIndexNode, letYieldConds, foo1, foo2, loop]))
  const decl = createAnonymousParserFunctionDecl(`array_iterator_slice`, createAnonymousToken(''), fnParams, fnBody)

  {
    const consumeParam2 = new ParseFreshIden(token, new FreshBindingToken('consume'))
    const yieldParam2 = new ParseFreshIden(token, new FreshBindingToken('yield'))
    const fnParams2: ParserFunctionParameter[] = [
      { name: consumeParam2, storage: null, type: null },
      { name: yieldParam2, storage: null, type: null },
      // TODO: Support passing null values, otherwise this doesn't work, so I had to make them typeArgs
      // { name: startIden, storage: null, type: null },
      // { name: endIden, storage: null, type: null },
      // { name: stepIden, storage: null, type: null }
    ]
    const closureCall = new ParseCall(token, new ParseFunction(createAnonymousToken(''), decl), [consumeParam2, yieldParam2], [])
    const fnBody2 = new ParseStatements(createAnonymousToken(''), [closureCall])
    const decl2 = createAnonymousParserFunctionDecl(`array_iterator_slice_partial`, createAnonymousToken(''), fnParams2, fnBody2)
    decl2.typeParams = [startIden, endIden, stepIden]
    return decl2
  }
})()

const createIteratorSliceLoop = (ctx: CompilerFunctionCallContext, token: Token, subCompilerState: SubCompilerState, consume: CompilerCallable, originalYieldFn: CompilerCallable, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }) => {

  // TODO: Make this inlined

  const consumeParam2 = new ParseFreshIden(token, new FreshBindingToken('consume'))
  const yieldParam2 = new ParseFreshIden(token, new FreshBindingToken('yield'))
  const fnParams2: ParserFunctionParameter[] = [{ name: consumeParam2, storage: null, type: null }, { name: yieldParam2, storage: null, type: null }]
  const start = new ParseMeta(token, selector.start ? selector.start : new ParseNil(createAnonymousToken('')))
  const end = new ParseMeta(token, selector.end ? selector.end : new ParseNil(createAnonymousToken('')))
  const step = new ParseMeta(token, selector.step ? selector.step : new ParseNil(createAnonymousToken('')))
  const closureCall = new ParseCall(token, new ParseFunction(createAnonymousToken(''), createIteratorSliceLoopPartialFn), [consumeParam2, yieldParam2], [start, end, step])

  const fnBody2 = new ParseStatements(createAnonymousToken(''), [closureCall])
  const decl2 = createAnonymousParserFunctionDecl(`array_iterator_slice_loop`, createAnonymousToken(''), fnParams2, fnBody2)
  const funcDef2 = insertFunctionDefinition(subCompilerState.globalCompiler, decl2)
  const closure2 = new Closure(funcDef2, subCompilerState.scope, subCompilerState)

  return createCallAstFromValue(ctx, closure2, [], [new CompTimeObjAst(VoidType, ctx.location, consume), new CompTimeObjAst(VoidType, ctx.location, originalYieldFn)])
}

const createIteratorSliceIterator = (token: Token, subCompilerState: SubCompilerState, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }, iterator: CompilerCallable): CompilerCallable => {

  const newIteratorClosure = (iteratorArg: Ast): Task<Ast, CompilerError> => {
    compilerAssert(isAst(iteratorArg))
    const originalYieldFn = iteratorArg instanceof CompTimeObjAst ? iteratorArg.value : iteratorArg
    compilerAssert(originalYieldFn instanceof Closure || originalYieldFn instanceof CompilerFunction, "Expected function")
    const interleave = interleaveHelper()
    const ctx: CompilerFunctionCallContext = { location: token.location, compilerState: subCompilerState, resultAst: undefined, typeCheckResult: undefined }

    const yieldA = (
      interleave.waitForEntryBinding()
      .chainFn((task, entryParam) => {
        const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
        const fn1 = new CompilerFunction('consume', (ctx, typeArgs, args) => {
          const stmts = createStatements(ctx.location, [
            interleave.continueEntry(ctx.location),
            new BindingAst(entryParam.type, ctx.location, entryParam)
          ])
          return Task.of(stmts)
        })
        return createIteratorSliceLoop(fnctx1, token, ctx.compilerState, fn1, originalYieldFn, selector)
      })
    );

    const fn2 = new CompilerFunction('produce', (ctx, typeArgs, args) => {
      const [bindingAst] = args
      compilerAssert(isAst(bindingAst))
      const stmts = createStatements(ctx.location, [
        interleave.createSetter(ctx.location, bindingAst),
        interleave.continueOther(ctx.location)])
      return Task.of(stmts)
    })
    const fnctx2: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const yieldB = createCallAstFromValue(fnctx2, iterator, [], [new CompTimeObjAst(VoidType, ctx.location, fn2)])

    return (
      Task.concurrency<unknown, CompilerError>([yieldA, yieldB, interleave.waitForEntryBinding()])
      .chainFn((task, args) => {
        const [a, b, entryParam] = args
        compilerAssert(isAst(a))
        compilerAssert(isAst(b))
        compilerAssert(ctx.location)
        compilerAssert(entryParam instanceof Binding)
        return Task.of(
          new StatementsAst(VoidType, ctx.location, [
            new LetAst(VoidType, ctx.location, entryParam, new DefaultConsAst(entryParam.type, ctx.location)),
            interleave.buildAst(ctx.location, a, b)
          ])
        )
      })
    )
  }

  const closure = new CompilerFunction('iteratorslice', (ctx, typeArgs, args) => {
    return newIteratorClosure(expectAst(args[0]))
  })

  return closure;

}


const interleaveHelper = () => {
  const interleaveBinding = new Binding("intrlv", VoidType)
  const entryLabels: Binding[] = []
  const otherLabels: Binding[] = []
  const createEntryLabel = () => {
    entryLabels.push(new Binding(`inter_entry${entryLabels.length + 1}`, VoidType))
    return entryLabels[entryLabels.length - 1]
  }
  const createElseLabel = () => {
    otherLabels.push(new Binding(`inter_else${otherLabels.length + 1}`, VoidType))
    return otherLabels[otherLabels.length - 1]
  }
  // Else block has a label for the entry point of the block
  // but the Entry block doesn't. This is reflected in the IR output
  createElseLabel()

  const entryParamEvent = new Event<Binding, CompilerError>()
  const getOrCreateParamBinding = (type: Type) => {
    if (entryParamEvent._success) {
      compilerAssert(entryParamEvent._success.type === type, "The producer/consumer types do not match: $type, $type2", { type, type2: entryParamEvent._success.type, entry: entryParamEvent._success })
      return entryParamEvent._success
    }
    const entryParam = new Binding('prd', type)
    entryParamEvent.success(entryParam)
    return entryParam
  }
  const continueEntry = (location: SourceLocation) => new ContinueInterAst(VoidType, location, interleaveBinding, createEntryLabel())
  const continueOther = (location: SourceLocation) => new ContinueInterAst(VoidType, location, interleaveBinding, createElseLabel())
  const waitForEntryBinding = () => Task.waitFor(entryParamEvent)
  const buildAst = (location: SourceLocation, a: Ast, b: Ast) => {
    return new InterleaveAst(VoidType, location, interleaveBinding, entryLabels, otherLabels, a, b)
  }
  const createSetter = (location: SourceLocation, value: Ast) => {
    propagatedLiteralAst(value)
    const entryParam = getOrCreateParamBinding(value.type)
    return new SetAst(VoidType, location, entryParam, value)
  }
  return {
    createEntryLabel, createElseLabel, continueEntry, continueOther, waitForEntryBinding, 
    getOrCreateParamBinding, createSetter, buildAst
  }
}

const iteratableToCallable = (ctx: CompilerFunctionCallContext, token: Token, iteratable: Ast, selector: ExpansionSelector) => {
  if (iteratable instanceof CompTimeObjAst && isCompilerCallable(iteratable.value)) {
    let closure = iteratable.value
    if (selector.start || selector.end || selector.step) {
      closure = createIteratorSliceIterator(token, ctx.compilerState, selector, closure)
    }
    return closure
  } else if (isAst(iteratable) && iteratable.type.typeInfo.metaobject['iterate']) {
    const iterateFn = iteratable.type.typeInfo.metaobject['iterate']
    compilerAssert(iterateFn instanceof Closure, "Expected closure")
    let closure: CompilerCallable = new CompilerFunction("iteratetoclosure", (ctx, typeArgs, args) => {
      const [iteratorArg] = args
      compilerAssert(iteratorArg instanceof CompTimeObjAst, "Expected comp time obj", { iteratorArg })
      return createCallAstFromValue(ctx, iterateFn, [iteratorArg.value], [iteratable])
    })
    if (selector.start || selector.end || selector.step) {
      closure = createIteratorSliceIterator(token, ctx.compilerState, selector, closure)
    }
    return closure
  }
  compilerAssert(selector.indexIdentifier)
  const iterator = createArrayIterator(token, ctx.compilerState, selector)
  return iterator.closure
}

const visitExpansion = (out: BytecodeWriter, expansion: ExpansionCompilerState, node: ParseNode) => {
  // Because we want to use the expansion state to insert a loop construct
  // around the expansion body we need to evaluate the body first.
  // Visit the body of the expansion node but output to a fresh bytecode
  // array which we will append later using a ParseBytecode.
  // This is in combination with sliceSugar below

  compilerAssert(!out.state.expansion, "Already in expansion state")

  out.state.expansion = expansion
  const bytecode = { code: [], locations: [] }
  if (node instanceof ParseIf) {
    const bytecode = { code: [], locations: [] }
    visitParseNode({ location: expansion.location, bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.condition)
    expansion.filterNode = new ParseBytecode(node.token, bytecode)
    node = node.trueBody
  }
  if (node instanceof ParseWhileExpr) {
    const bytecode = { code: [], locations: [] }
    visitParseNode({ location: expansion.location, bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.condition)
    expansion.whileNode = new ParseBytecode(node.token, bytecode)
    node = node.body
  }
  visitParseNode({ location: expansion.location, bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node)
  out.state.expansion = null
  return expansion.loopBodyNode = new ParseBytecode(node.token, bytecode)
}

const compileExpansionToParseNode = (out: BytecodeWriter, expansion: ExpansionCompilerState, node: ParseNode) => {

  compilerAssert(expansion.loopBodyNode, "Expected loop body")

  if (expansion.filterNode) {
    expansion.loopBodyNode = new ParseIf(node.token, false, expansion.filterNode, expansion.loopBodyNode, null)
  }
  if (expansion.whileNode) {
    const break_ = new ParseBreak(node.token, expansion.breakIden, null)
    const cond = new ParseNot(createAnonymousToken(''), expansion.whileNode)
    const ifBreak = new ParseIf(node.token, false, cond, break_, null)
    expansion.loopBodyNode = new ParseStatements(node.token, [ifBreak, expansion.loopBodyNode])
  }
  if (expansion.loopBodyMeta.length) {
    expansion.loopBodyNode = new ParseMeta(node.token, new ParseStatements(node.token, [...expansion.loopBodyMeta, new ParseQuote(node.token, expansion.loopBodyNode)]))
  }

  const zipCallables = expansion.selectors.map(selector => {
    return new ParseEvalFunc(node.token, (vm) => {
      const iterator = vm.stack.pop()
      compilerAssert(isAst(iterator), "Expected ast", { iterator })
      const fnctx: CompilerFunctionCallContext = { location: vm.location, compilerState: vm.context.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
      const callable = iteratableToCallable(fnctx, node.token, iterator, selector)
      vm.stack.push(callable)
      return 
    }, [new ParseQuote(node.token, selector.node)], [])
  })

  const setterResult = (stmts: ParseNode[]) => {
    const setter = createArraySetterIterator2(createAnonymousToken(''), expansion.setterSelector!)
    const stmtsNode = new ParseStatements(node.token, [...stmts, expansion.loopBodyNode!])
    const decl = createAnonymousParserFunctionDecl(`${expansion.debugName}_setter`, createAnonymousToken(''), [], stmtsNode)
    return new ParseCall(node.token, new ParseFunction(node.token, setter), [new ParseFunction(node.token, decl)], [])
  }

  const whileResult = (stmts: ParseNode[]) => {
    const stmtsNode = new ParseStatements(node.token, [...stmts, expansion.loopBodyNode!])
    return new ParseWhile(node.token, new ParseBoolean(createAnonymousToken('true')), stmtsNode)
  }

  const zipResult = expansion.setterSelector ? setterResult : whileResult

  const zips = expansion.selectors.map((selector, i) => {
    return { callable: zipCallables[i], elemIden: selector.elemIdentifier }
  })

  let loopNode: ParseNode
  if (zips.length === 1 && !expansion.setterSelector) {
    const declParams: ParserFunctionParameter[] = [{ name: zips[0].elemIden, storage: null, type: null }]
    const decl1 = createAnonymousParserFunctionDecl(`${expansion.debugName}_consume`, createAnonymousToken(''), declParams, expansion.loopBodyNode)
    const consume = new ParseFunction(node.token, decl1)
    const consumer = new ParseFreshIden(node.token, new FreshBindingToken('consumer'))
    const letConsumer = new ParseLetConst(node.token, consumer, zips[0].callable)
    const call_ = new ParseCall(node.token, consumer, [consume], [])
    loopNode = new ParseStatements(node.token, [letConsumer, call_])
  } else {
    loopNode = zipHelper(expansion.debugName, zips, zipResult)
  }

  const lets: ParseNode[] = [...expansion.lets]
  const final: ParseNode[] = [loopNode]
  if (expansion.fold) {
    lets.push(new ParseLet(node.token, expansion.fold.iden, null, expansion.fold.initial))
    final.push(expansion.fold.iden)
  }

  const stmts = new ParseStatements(node.token, [...lets, ...final])
  const block =  new ParseBlock(createAnonymousToken(''), 'break', expansion.breakIden, stmts)
  return new ParseMeta(node.token, new ParseStatements(node.token, [
    new ParseLet(node.token, expansion.metaResultIden, null, new ParseQuote(node.token, block)),
    expansion.metaResult
  ]))
}


const generatorHelper = (debugName: string, consumeParam: ParseFreshIden, consumer: ParseNode, produceParam: ParseFreshIden, producer: ParseNode) => {
  const token = createAnonymousToken('')

  const bparams: ParserFunctionParameter[] = [{ name: consumeParam, type: null, storage: null }]
  const b1 = createAnonymousParserFunctionDecl(`${debugName}_zip_consumer`, token, bparams, consumer)
  const b = new ParseFunction(token, b1)

  const cparams: ParserFunctionParameter[] = [{ name: produceParam, type: null, storage: null }]
  const c1 = createAnonymousParserFunctionDecl(`${debugName}_zip_producer`, token, cparams, producer)
  const c = new ParseFunction(token, c1)

  return new ParseMeta(token, new ParseCall(createAnonymousToken(''), new ParseValue(createAnonymousToken(''), generator2), [b, c], []))
}

const createConsumerProducer = (token: Token, callable: ParseNode, produceFn: ParseNode) => {
  const name = new ParseFreshIden(token, new FreshBindingToken('callable'))
  const call_ = new ParseCall(token, name, [produceFn], [])
  return new ParseStatements(token, [new ParseLetConst(token, name, callable), call_])
}


type ZipIterator = { callable: ParseNode, elemIden: ParseFreshIden }

const zipHelper = (debugName: string, zips: ZipIterator[], result: (stmts: ParseNode[]) => ParseNode, index: number = 0, letStmts: ParseNode[] = []): ParseNode => {
  const token = createAnonymousToken('')

  if (index === zips.length) return result(letStmts)

  const { elemIden, callable } = zips[index]
  const consumeParam1 = new ParseFreshIden(token, new FreshBindingToken('consume'))
  const produceParam1 = new ParseFreshIden(token, new FreshBindingToken('produce'))

  const let_ = new ParseLet(token, elemIden, null, new ParseCall(token, consumeParam1, [], []))
  
  const innerProducer = zipHelper(debugName, zips, result, index + 1, [...letStmts, let_])
  const consumerProducer = createConsumerProducer(token, callable, produceParam1)
  return generatorHelper(`${debugName}_zip${index}`, consumeParam1, innerProducer, produceParam1, consumerProducer)
}

export const expandDotsSugar = (out: BytecodeWriter, node: ParseExpand) => {
  const expansion = createExpansionState('loop', node.token.location)
  let result = visitExpansion(out, expansion, node.expr)
  if (expansion.fold) {
    const set_ = new ParseSet(node.token, expansion.fold.iden, result)
    expansion.loopBodyNode = new ParseStatements(node.token, [set_, expansion.fold.iden])
  }
  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}

export const expandFuncAllSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('all', node.token.location)
  expansion.optimiseSimple = true
  const result = visitExpansion(out, expansion, node)

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  expansion.fold = { iden: new ParseFreshIden(node.token, new FreshBindingToken('fold_iden')), initial: new ParseBoolean((createAnonymousToken('true'))) }
  const cond = new ParseNot(node.token, result)
  const set_ = new ParseSet(node.token, expansion.fold.iden, new ParseBoolean(createAnonymousToken('false')))
  const break_ = new ParseBreak(node.token, expansion.breakIden, null)
  expansion.loopBodyNode = new ParseIf(node.token, false, cond, new ParseStatements(noteNode.token, [set_, break_]), null)

  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}

export const expandFuncAnySugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('any', node.token.location)
  expansion.optimiseSimple = true
  const cond = visitExpansion(out, expansion, node)

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  expansion.fold = { iden: new ParseFreshIden(node.token, new FreshBindingToken('fold_iden')), initial: new ParseBoolean((createAnonymousToken('false'))) }
  const set_ = new ParseSet(node.token, expansion.fold.iden, new ParseBoolean(createAnonymousToken('true')))
  const break_ = new ParseBreak(node.token, expansion.breakIden, null)
  expansion.loopBodyNode = new ParseIf(node.token, false, cond, new ParseStatements(noteNode.token, [set_, break_]), null)

  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}
export const expandFuncSumSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  let node = args[0]
  const expansion = createExpansionState('sum', node.token.location)
  const result = visitExpansion(out, expansion, node)
  compilerAssert(!expansion.fold, "Fold not supported in this context")

  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  expansion.fold = { iden: new ParseFreshIden(node.token, new FreshBindingToken('fold_iden')), initial: new ParseNumber((createAnonymousToken('0'))) }
  expansion.loopBodyNode = new ParseOperator(createAnonymousToken('+'), [result, expansion.fold.iden])
  expansion.loopBodyNode = new ParseSet(node.token, expansion.fold.iden, expansion.loopBodyNode)

  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}
export class DeferredLetBinding {
  type: Type
  binding: Binding | null
  letAst: LetAst
  constructor() {}
}

const createDeferObject = new ExternalFunction('createDeferObject', CompileTimeObjectType, (ctx, values) => {
  return new CompTimeObjAst(CompileTimeObjectType, ctx.location, new DeferredLetBinding())
})
const deferAssignSet = new ExternalFunction('deferAssignSet', VoidType, (ctx, values) => {
  let [value, defer] = values
  if (defer instanceof CompTimeObjAst) defer = defer.value
  compilerAssert(defer instanceof DeferredLetBinding, "Expected deferred let binding", { defer })
  compilerAssert(isAst(value), "Expected ast", { value, defer })
  compilerAssert(value instanceof LetAst, "Expected let ast", { value, defer })
  if (!defer.letAst) Object.assign(defer, { letAst: value, type: value.type, binding: value.binding })
  compilerAssert(defer.type === value.type, "Type mismatch", { defer, value })
})
const deferToResultAst = new ExternalFunction('deferToResultAst', VoidType, (ctx, values) => {
  let [stmts, defer] = values
  if (defer instanceof CompTimeObjAst) defer = defer.value
  compilerAssert(defer instanceof DeferredLetBinding, "Expected deferred let binding", { defer })
  compilerAssert(isAst(stmts), "Expected ast", { stmts, defer })
  const binding = defer.binding
  compilerAssert(binding, "Expected binding", { defer })
  const bindingAst = new BindingAst(binding.type, ctx.location, binding)
  return createStatements(ctx.location, [defer.letAst, stmts, bindingAst])
})
const deferToBinding = new ExternalFunction('deferToBinding', VoidType, (ctx, values) => {
  let [defer] = values
  if (defer instanceof CompTimeObjAst) defer = defer.value
  compilerAssert(defer instanceof DeferredLetBinding, "Expected deferred let binding", { defer })
  const binding = defer.binding
  compilerAssert(binding, "Expected binding", { defer })
  return new BindingAst(binding.type, ctx.location, binding)
})

const createFreshLet = new ExternalFunction('createFreshLet', VoidType, (ctx, values) => {
  let [value] = values
  compilerAssert(isAst(value), "Expected ast", { value })
  const binding = new Binding("defer", value.type)
  return new LetAst(binding.type, ctx.location, binding, value)
})

const createDefaultFromType = new ExternalFunction('createDefaultFromType', VoidType, (ctx, values) => {
  let [type] = values
  compilerAssert(isType(type), "Expected type", { type })
  return new DefaultConsAst(type, ctx.location)
})
const maxOfType = new ExternalFunction('maxOfType', VoidType, (ctx, values) => {
  let [type] = values
  compilerAssert(isType(type), "Expected type", { type })
  compilerAssert(type.typeInfo.metaobject['max'], "Type does not have a max value", { type })
  return new NumberAst(type, ctx.location, type.typeInfo.metaobject['max'] as number)
})
const minOfType = new ExternalFunction('maxOfType', VoidType, (ctx, values) => {
  let [type] = values
  compilerAssert(isType(type), "Expected type", { type })
  compilerAssert(type.typeInfo.metaobject['min'], "Type does not have a min value", { type })
  return new NumberAst(type, ctx.location, type.typeInfo.metaobject['min'] as number)
})

const typeOf = new ExternalFunction('typeOf', VoidType, (ctx, values) => {
  let [value] = values
  compilerAssert(isAst(value), "Expected ast", { value })
  return value.type
})

const callV = (token: Token, value: unknown, args: ParseNode[], typeArgs: ParseNode[]) => {
  return new ParseCall(token, new ParseValue(token, value), args, [])
}
const createTypeOf = (token: Token, value: ParseNode) => {
  return callV(token, typeOf, [new ParseQuote(token, value)], [])
}
const createDefaultOf = (token: Token, originalResult: ParseNode) => {
  const typeOf_ = callV(token, typeOf, [new ParseQuote(token, originalResult)], [])
  return callV(token, createDefaultFromType, [typeOf_], [])
}

const createDeferTypeCheckingIden = (expansion: ExpansionCompilerState, debugName: string, defaultNode: ParseNode) => {
  const token = createAnonymousToken('')
  const deferIden = new ParseFreshIden(token, new FreshBindingToken(debugName))
  const createDefer = new ParseCall(createAnonymousToken(''), new ParseValue(token, createDeferObject), [], [])
  expansion.lets.push(new ParseLetConst(token, deferIden, createDefer))
  
  const toLet_ = callV(token, createFreshLet, [defaultNode], [])
  expansion.loopBodyMeta.push(callV(token, deferAssignSet, [toLet_, deferIden], []))
  expansion.metaResult = callV(token, deferToResultAst, [new ParseQuote(token, expansion.metaResult), new ParseQuote(token, deferIden)], [])
  return new ParseMeta(token, callV(token, deferToBinding, [new ParseQuote(token, deferIden)], []))
}

export const expandFuncFirstSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('first', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const iden = createDeferTypeCheckingIden(expansion, 'first', createDefaultOf(node.token, result))
  const break_ = new ParseBreak(node.token, expansion.breakIden, null)
  const set_ = new ParseSet(node.token, iden, result)
  expansion.loopBodyNode = new ParseStatements(node.token, [set_, break_])
  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}

export const expandFuncLastSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('last', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const iden = createDeferTypeCheckingIden(expansion, 'last', createDefaultOf(node.token, result))
  expansion.loopBodyNode = new ParseSet(node.token, iden, result)
  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}

export const expandFuncMinSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('min', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const initial = callV(node.token, maxOfType, [createTypeOf(node.token, result)], [])
  const iden = createDeferTypeCheckingIden(expansion, 'min', initial)
  const comp = new ParseOperator(createAnonymousToken('<'), [result, iden])
  const set_ = new ParseSet(node.token, iden, result)
  expansion.loopBodyNode = new ParseIf(node.token, false, comp, set_, null)
  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}

export const expandFuncMaxSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('max', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")
  
  const initial = callV(node.token, minOfType, [createTypeOf(node.token, result)], [])
  const iden = createDeferTypeCheckingIden(expansion, 'max', initial)
  const comp = new ParseOperator(createAnonymousToken('>'), [result, iden])
  const set_ = new ParseSet(node.token, iden, result)
  expansion.loopBodyNode = new ParseIf(node.token, false, comp, set_, null)
  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}


export const generatorInternal = (
  ctx: CompilerFunctionCallContext, 
  consumeFn: (yieldedValue: Ast) => Task<Ast, CompilerError>, 
  produceFn: (yieldFn: (value: Ast) => Task<Ast, CompilerError>) => Task<Ast, CompilerError>
): Task<Ast, CompilerError> => {

  const interleave = interleaveHelper()

  const yieldA = (
    interleave.waitForEntryBinding()
    .chainFn((task, entryParam) => {
      const stmts = createStatements(ctx.location, [
        interleave.continueEntry(ctx.location),
        new BindingAst(entryParam.type, ctx.location, entryParam)
      ])
      return consumeFn(stmts)
    })
  ); 
  const yieldFn = (value: Ast): Task<Ast, CompilerError> => {
    compilerAssert(isAst(value))
    const stmts = createStatements(ctx.location, [
      interleave.createSetter(ctx.location, value),
      interleave.continueOther(ctx.location)])
    return Task.of(stmts)
  }
  const yieldB = produceFn(yieldFn)

  return (
    Task.concurrency<unknown, CompilerError>([yieldA, yieldB, interleave.waitForEntryBinding()])
    .chainFn((task, args) => {
      const [a, b, entryParam] = args
      compilerAssert(isAst(a))
      compilerAssert(isAst(b))
      compilerAssert(ctx.location)
      compilerAssert(entryParam instanceof Binding)
      return Task.of(
        new StatementsAst(VoidType, ctx.location, [
          new LetAst(VoidType, ctx.location, entryParam, new DefaultConsAst(entryParam.type, ctx.location)),
          interleave.buildAst(ctx.location, a, b)
        ])
      )
    })
  )
}



export const expandFuncConcatSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  const token = noteNode.token
  const nodes: ParseNode[] = []
  for (const node of args) {
    const expansion = createExpansionState('concat', node.token.location)
    const result = visitExpansion(out, expansion, node)

    if (expansion.selectors.length === 0) {
      nodes.push(node)
      continue
    }

    compilerAssert(!expansion.fold, "Fold not supported in this context")

    const param = new ParseFreshIden(node.token, new FreshBindingToken('yieldFn'))
    expansion.loopBodyNode = new ParseCall(createAnonymousToken(''), param, [result], [])
    expansion.optimiseSimple = expansion.selectors.length === 1 // TODO: Check this is correct in all cases
    expansion.debugName = `concat${nodes.length}`

    const expansionParseNode = compileExpansionToParseNode(out, expansion, node)

    const func = createAnonymousParserFunctionDecl("iterate", 
      createAnonymousToken(''), [{ name: param, type: null, storage: null }], expansionParseNode)

    nodes.push(new ParseFunction(token, func))
  }

  visitParseNode(out, new ParseCall(token, new ParseValue(token, concat), nodes, []))
}

export const concat = new ExternalFunction("concat", VoidType, (ctx, values_) => {

  const closureBody = (valueBinding: unknown): Task<Ast, CompilerError> => {
    compilerAssert(valueBinding instanceof CompTimeObjAst)
    const outFn = valueBinding.value
    compilerAssert(outFn instanceof Closure || outFn instanceof CompilerFunction, "Expected function", { valueBinding })

    // TODO: Try and support optimised CompilerFunctions
    if (outFn instanceof Closure && canOptimiseSimpleIterator(outFn)) {
      const iteratorCalls = values_.map((closure, i) => {
        const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }

        if (!(closure instanceof Closure)) { // Support for constant values
          const value = unknownToAst(ctx.location, closure)
          return createCallAstFromValue(fnctx1, outFn, [], [propagatedLiteralAst(value)])
        }
        return createCallAstFromValue(fnctx1, closure, [], [new CompTimeObjAst(VoidType, ctx.location, outFn)])
      })
      return Task.all(iteratorCalls).chainFn((task, callAsts) => {
        callAsts.forEach(x => compilerAssert(isAst(x)))
        return Task.of(new StatementsAst(VoidType, ctx.location, callAsts))
      })
    }

    const interleave = interleaveHelper()

    const iteratorCalls = values_.map((closure, i) => {
      if (!(closure instanceof Closure)) { // Support for constant values
        const value = unknownToAst(ctx.location, closure)
        return Task.of(createStatements(ctx.location, [
          interleave.createSetter(ctx.location, propagatedLiteralAst(value)),
          interleave.continueEntry(ctx.location),
        ]))
      }

      const yieldA = new CompilerFunction("yieldA", (ctx, typeArgs, args) => {
        const [valueAst] = args
        compilerAssert(isAst(valueAst), "Expected AST", { valueAst })
        return Task.of(createStatements(ctx.location, [
          interleave.createSetter(ctx.location, valueAst),
          interleave.continueEntry(ctx.location),
        ]))
      })
      const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
      return createCallAstFromValue(fnctx1, closure, [], [new CompTimeObjAst(VoidType, ctx.location, yieldA)])
    })

    const callC = (
      interleave.waitForEntryBinding()
      .chainFn((task, entryParam) => {
        const fnctx: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
        return (
          createCallAstFromValue(fnctx, outFn, [], [new BindingAst(entryParam.type, ctx.location, entryParam)])
          .chainFn((task, arg) => {
            const cont = interleave.continueOther(ctx.location)
            const stmts = createStatements(ctx.location, [arg, cont])
            const loop = new WhileAst(VoidType, ctx.location, new BoolAst(BoolType, ctx.location, true), stmts)
            return Task.of(loop)
          })
        )
      })
    )

    return (
      Task.concurrency<unknown, CompilerError>([callC, interleave.waitForEntryBinding(), ...iteratorCalls])
      .chainFn((task, args) => {
        const [resultFn, entryParam, ...callAsts] = args

        callAsts.forEach(x => compilerAssert(isAst(x)))
        compilerAssert(isAst(resultFn))
        compilerAssert(entryParam instanceof Binding)
        compilerAssert(ctx.location)

        const entryStmts = createStatements(ctx.location, callAsts as Ast[])
        const stmts = createStatements(ctx.location, [
          new LetAst(VoidType, ctx.location, entryParam, new DefaultConsAst(entryParam.type, ctx.location)),
          interleave.buildAst(ctx.location, entryStmts, resultFn)
        ])

        return Task.of(stmts)
        
      })
    )
    
  }
  return new CompilerFunction("concat", (ctx, typeArgs, args) => closureBody(args[0]))
})

export const generator = new CompilerFunction("generator", (ctx, typeArgs, args) => {
  const consumeFn = args[0] instanceof CompTimeObjAst ? args[0].value : undefined
  const produceFn = args[1] instanceof CompTimeObjAst ? args[1].value : undefined
  compilerAssert(consumeFn instanceof Closure, "Expected function", { consumeFn })
  compilerAssert(produceFn instanceof Closure, "Expected function", { produceFn })

  return generatorInternal(ctx, (ast) => {
    const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const fn1 = new CompilerFunction("consume", (ctx, typeArgs, args) => Task.of(ast))
    return createCallAstFromValue(fnctx1, consumeFn, [], [new CompTimeObjAst(VoidType, ctx.location, fn1)])
  }, (yieldFn) => {
    const fn2 = new CompilerFunction("produce", (ctx, typeArgs, args) => {
      const [bindingAst] = args
      compilerAssert(isAst(bindingAst))
      return yieldFn(bindingAst)
    })
    const fnctx2: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    return createCallAstFromValue(fnctx2, produceFn, [], [new CompTimeObjAst(VoidType, ctx.location, fn2)])
  })

})

const generator2 = new ExternalFunction("generator2", VoidType, (ctx, values) => {

  const [consumeFn, produceFn] = values

  return generatorInternal(ctx, (ast) => {
    const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const fn1 = new CompilerFunction("consume", (ctx, typeArgs, args) => Task.of(ast))
    return createCallAstFromValue(fnctx1, consumeFn, [], [new CompTimeObjAst(VoidType, ctx.location, fn1)])
  }, (yieldFn) => {
    const fn2 = new CompilerFunction("produce", (ctx, typeArgs, args) => {
      const [bindingAst] = args
      compilerAssert(isAst(bindingAst))
      return yieldFn(bindingAst)
    })
    const fnctx2: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    return createCallAstFromValue(fnctx2, produceFn, [], [new CompTimeObjAst(VoidType, ctx.location, fn2)])
  })
})
