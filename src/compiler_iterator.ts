import { BytecodeSecondOrder, compileFunctionPrototype, getCommonType, getOperatorTable, loadModule, popStack, popValues, propagateLiteralType, propagatedLiteralAst, pushBytecode, resolveScope, unknownToAst, visitParseNode } from "./compiler"
import { compileExportedFunctionTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall, insertFunctionDefinition } from "./compiler_functions"
import { Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd, ParseFold, ParseForExpr, ParseWhileExpr, Module, pushSubCompilerState, createScope, TaskContext, CompilerError, AstType, OperatorAst, CompilerFunction, CallAst, RawPointerType, SubscriptAst, IntType, expectType, SetSubscriptAst, ParserFunctionParameter, FunctionType, Binding, StringType, ValueFieldAst, LetAst, BindingAst, createStatements, StringAst, FloatType, DoubleType, CompilerFunctionCallContext, Vm, expectAst, NumberAst, Type, UserCallAst, hashValues, NeverType, IfAst, BoolType, VoidAst, LoopObject, CompileTimeObjectType, u64Type, FunctionDefinition, ParserFunctionDecl, StatementsAst, isTypeScalar, IntLiteralType, FloatLiteralType, isAst, isType, isTypeCheckError, InterleaveAst, ContinueInterAst, CompTimeObjAst, ParseEvalFunc, SetAst, DefaultConsAst, WhileAst, BoolAst, isArray, ExpansionSelector, ParseNote, ExpansionCompilerState, ParseBoolean, ParseOr, ParseBreak, filterNotNull, ParseTuple, ParseNot, ParseLetConst, ParseConcurrency, ParseCompTime } from "./defs"
import { Event, Task, TaskDef, isTask } from "./tasks"


type ExpansionZipState = {
  expansion: ExpansionCompilerState,
  setterSelector: ExpansionSelector | null,
  iteratorList: IteratorState[],
  totalIterators: number,
  bindingValues: BindingAst[],
  lets: Ast[],
  resultClosure: Closure,
  location: SourceLocation,
  compilerState: SubCompilerState,
  
}
type IteratorState = {
  closure: Closure,
  selector: ExpansionSelector
}
const createExpansionState = (debugName: string, location: SourceLocation): ExpansionCompilerState => {
  const iteratorListIdentifier = new ParseFreshIden(createAnonymousToken(''), new FreshBindingToken('iterator_list'))
  const breakIden = new ParseFreshIden(createAnonymousToken(''), new FreshBindingToken('break'))
  return { debugName, loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null, filterNode: null, whileNode: null, optimiseSimple: false, breakIden, location }
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
      const exprQuote = new ParseQuote(node.token, expr)
      const iden = new ParseFreshIden(node.token, new FreshBindingToken('elem'))
      const let_ = new ParseLet(node.token, iden, null, exprQuote)
      const call = new ParseCall(node.token, new ParseValue(node.token, arrayConstructorTypeCheck), [listConstructorIden, iden], [])
      const call2 = new ParseCall(node.token, new ParseValue(node.token, arrayConstructorCreateAppend), [listConstructorIden, iden], [])
      const call3 = new ParseCall(node.token, new ParseValue(node.token, arrayConstructorAddAppendCall), [listConstructorIden, call2], [])
      const meta_ = new ParseMeta(node.token, new ParseStatements(node.token, [let_, call, call3]))

      const fn = createAnonymousParserFunctionDecl('appendValue', node.token, [], meta_)
      return new ParseFunction(node.token, fn)
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

const createArraySetterIterator = (token: Token, subCompilerState: SubCompilerState, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }) => {
  
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
  const decl = createAnonymousParserFunctionDecl(`array_setter_iterator`, createAnonymousToken(''), fnParams, fnBody)
  const funcDef = insertFunctionDefinition(subCompilerState.globalCompiler, decl)
  const closure = new Closure(funcDef, subCompilerState.scope, subCompilerState)
  return { closure, yieldParam, indexIdentifier, subscriptIterator, valueIdentifier }
}

const createIteratorSliceLoop = (token: Token, subCompilerState: SubCompilerState, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }) => {

  const consumeParam = new ParseFreshIden(token, new FreshBindingToken('consume'))
  const yieldParam = new ParseFreshIden(token, new FreshBindingToken('yield'))
  const indexIdentifier = selector.indexIdentifier
  compilerAssert(indexIdentifier)
  const fnParams: ParserFunctionParameter[] = [
    { name: consumeParam, storage: null, type: null },
    { name: yieldParam, storage: null, type: null },
  ]

  const letStartNode = selector.start ? new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('start')), null, selector.start) : null
  const letEndNode = selector.end ? new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('end')), null, selector.end) : null
  const letStepNode = selector.step ? new ParseLet(token, new ParseFreshIden(token, new FreshBindingToken('step')), null, selector.step) : null
  const letIndexNode = new ParseLet(token, indexIdentifier, null, new ParseNumber(createAnonymousToken('0')))
  const incNode = new ParseOpEq(createAnonymousToken("+="), letIndexNode.left, new ParseNumber(createAnonymousToken('1')))
  const consumedValue = new ParseFreshIden(token, new FreshBindingToken('slice'))
  const trueNode = new ParseBoolean(createAnonymousToken('true'))
  const loopCondNode = letEndNode ? new ParseOperator(createAnonymousToken("<"), [letIndexNode.left, letEndNode.left]) : trueNode
  let yieldCall: ParseNode = new ParseCall(createAnonymousToken(''), yieldParam, [consumedValue], [])
  const yieldConds: ParseNode[] = []
  if (letStartNode) {
    yieldConds.push(new ParseOperator(createAnonymousToken(">="), [letIndexNode.left, letStartNode.left]))
  }
  if (letStepNode) {
    let check: ParseNode = letIndexNode.left
    if (letStartNode) check = new ParseOperator(createAnonymousToken("-"), [check, letStartNode.left])
    const mod = new ParseOperator(createAnonymousToken("mod"), [check, letStepNode.left])
    yieldConds.push(new ParseOperator(createAnonymousToken("=="), [mod, new ParseNumber(createAnonymousToken('0'))]))
  }
  const condNode = yieldConds.length > 0 ? yieldConds.reduce((prev, curr) => new ParseAnd(createAnonymousToken('and'), [prev, curr])) : null
  if (condNode) yieldCall = new ParseIf(token, false, condNode, yieldCall, null)
  const consumeCall = new ParseLet(token, consumedValue, null, new ParseCall(createAnonymousToken(''), consumeParam, [], []))
  const loopBody = new ParseStatements(token, filterNotNull([consumeCall, yieldCall, incNode]))
  const loop = new ParseWhile(token, loopCondNode, loopBody)

  const fnBody = new ParseStatements(createAnonymousToken(''), filterNotNull([letStartNode, letEndNode, letStepNode, letIndexNode, loop]))
  const decl = createAnonymousParserFunctionDecl(`array_iterator`, createAnonymousToken(''), fnParams, fnBody)
  const funcDef = insertFunctionDefinition(subCompilerState.globalCompiler, decl)
  const closure = new Closure(funcDef, subCompilerState.scope, subCompilerState)
  return { closure, yieldParam, indexIdentifier }
}

const createIteratorSliceIterator = (token: Token, subCompilerState: SubCompilerState, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }, iterator: Closure): Closure => {

  const fnctx: CompilerFunctionCallContext = { location: token.location, compilerState: subCompilerState, resultAst: undefined, typeCheckResult: undefined }
  const newIteratorClosure = closureHelper(fnctx, 'iteratorslice', ['x'], (iteratorArg) => {
    compilerAssert(isAst(iteratorArg))
    const originalYieldFn = iteratorArg instanceof CompTimeObjAst ? iteratorArg.value : iteratorArg
    compilerAssert(originalYieldFn instanceof Closure, "Expected closure")
    const interleave = interleaveHelper()
    const ctx: CompilerFunctionCallContext = { location: token.location, compilerState: subCompilerState, resultAst: undefined, typeCheckResult: undefined }

    const yieldA = (
      interleave.waitForEntryBinding()
      .chainFn((task, entryParam) => {
        const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
        const fn1 = closureHelper(ctx, 'consume', [], () => {
          const stmts = createStatements(ctx.location, [
            interleave.continueEntry(ctx.location),
            new BindingAst(entryParam.type, ctx.location, entryParam)
          ])
          return Task.of(stmts)
        })
        const loopClosure = createIteratorSliceLoop(token, ctx.compilerState, selector)
        return createCallAstFromValue(fnctx1, loopClosure.closure, [], [new CompTimeObjAst(VoidType, ctx.location, fn1), new CompTimeObjAst(VoidType, ctx.location, originalYieldFn)])
      })
    );

    const fn2 = closureHelper(ctx, 'produce', ['x'], (bindingAst) => {
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

  })

  return newIteratorClosure;

}

const closureHelper = (ctx: CompilerFunctionCallContext, debugName: string, params: string[], func: (...args: unknown[]) => Ast | Task<Ast, CompilerError>) => {
  
  const fnParams: ParserFunctionParameter[] = params.map(name => ({
    name: new ParseIdentifier(createAnonymousToken(name)), storage: null, type: null
  }))

  const evalFunc = (vm: Vm) => {
    const args = [...new Array(params.length)].map(() => vm.stack.pop())

    const res = func(...args)
    if (!isTask(res)) {
      vm.stack.push(res); return Task.success()
    }
    return res.chainFn((task, arg) => {
      vm.stack.push(arg); return Task.success()
    });
    
  }
  
  const fnBody = new ParseEvalFunc(createAnonymousToken(''), evalFunc, fnParams.map(x => x.name), [])
  const decl = createAnonymousParserFunctionDecl(debugName, createAnonymousToken(''), fnParams, fnBody)
  const funcDef = insertFunctionDefinition(ctx.compilerState.globalCompiler, decl)
  const fn = new Closure(funcDef, ctx.compilerState.scope, ctx.compilerState)
  return fn
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
    const entryParam = getOrCreateParamBinding(value.type)
    return new SetAst(VoidType, location, entryParam, value)
  }
  return {
    createEntryLabel, createElseLabel, continueEntry, continueOther, waitForEntryBinding, 
    getOrCreateParamBinding, createSetter, buildAst
  }
}


const expandZipIterator = (state: ExpansionZipState): Task<Ast, CompilerError> => {

  const location = state.location

  if (state.iteratorList.length === 0 && state.setterSelector) {
    compilerAssert(!state.expansion.filterNode, "Filter not implemented in setter yet")

    const setter = createArraySetterIterator(createAnonymousToken(''), state.compilerState, state.setterSelector)

    const fnctx1: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const finalProducer = closureHelper(fnctx1, 'finalproducer', [], () => {

      const fnctx2: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
      return (
        createCallAstFromValue(fnctx2, state.resultClosure, [], state.bindingValues)
        .chainFn((task, value) => {
          return Task.of(createStatements(location, [...state.lets, value]))
        })
      )
    })

    const fnctx2: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const callFsm = createCallAstFromValue(fnctx2, setter.closure, [], [new CompTimeObjAst(VoidType, location, finalProducer)])
    return callFsm
  }

  else if (state.iteratorList.length === 0) {
    // Generates the following loop using all the expansion zip state lets and bindingValues
    // while true:
    //   elem_a := yield_a()
    //   elem_b := yield_b()
    //   ...
    //   resultClosure(elem_a, elem_b, ...)

    const fnctx2: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
    return (
      createCallAstFromValue(fnctx2, state.resultClosure, [], state.bindingValues)
      .chainFn((task, result) => {
        const stmts = createStatements(location, [...state.lets, result])
        const whileAst = new WhileAst(VoidType, location, new BoolAst(BoolType, location, true), stmts)
        return Task.of(whileAst)
      })
    )
  }

  const iterator1 = state.iteratorList.shift()

  const consume1 = (yieldedValue: Ast): Task<Ast, CompilerError> => {

    const binding = new Binding("", yieldedValue.type)
    const bindingAst = new BindingAst(binding.type, location, binding)

    const letAst = new LetAst(VoidType, location, binding, yieldedValue)
    state.bindingValues.push(bindingAst)
    state.lets.push(letAst)

    return expandZipIterator(state)
  }

  const produce1 = (yieldFn: (value: Ast) => Task<Ast, CompilerError>): Task<Ast, CompilerError> => {
    const fnctx1: CompilerFunctionCallContext = { location: location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }

    const fn2 = closureHelper(fnctx1, 'fn2', ['x'], (bindingAst) => {
      compilerAssert(isAst(bindingAst))
      return yieldFn(bindingAst)
    })
    const fnctx2: CompilerFunctionCallContext = { location: location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
    compilerAssert(iterator1)
    return createCallAstFromValue(fnctx2, iterator1.closure, [], [new CompTimeObjAst(VoidType, location, fn2)])
  }

  const fnctx2: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }

  return generatorInternal(fnctx2, consume1, produce1)

}

const iteratableToClosure = (ctx: CompilerFunctionCallContext, token: Token, iteratable: Ast, selector: ExpansionSelector) => {
  if (iteratable instanceof CompTimeObjAst && iteratable.value instanceof Closure) {
    let closure = iteratable.value
    if (selector.start || selector.end || selector.step) {
      closure = createIteratorSliceIterator(token, ctx.compilerState, selector, closure)
    }
    return closure
  } else if (isAst(iteratable) && iteratable.type.typeInfo.metaobject['iterate']) {
    const iterateFn = iteratable.type.typeInfo.metaobject['iterate']
    compilerAssert(iterateFn instanceof Closure, "Expected closure")
    const fnctx: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    let closure = closureHelper(fnctx, 'iteratetoclosure', ['x'], (iteratorArg) => {
      compilerAssert(iteratorArg instanceof CompTimeObjAst, "Expected comp time obj", { iteratorArg })
      return createCallAstFromValue(fnctx, iterateFn, [iteratorArg.value], [iteratable])
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

  const list = new ParseList(node.token, expansion.selectors.map(x => new ParseQuote(node.token, x.node)))
  const letIteratorList = new ParseLet(node.token, expansion.iteratorListIdentifier, null, list)

  const metaList = new ParseMeta(node.token, letIteratorList)

  const finalAst = new ParseEvalFunc(createAnonymousToken(''), (vm: Vm) => {

    const iteratorObjList = vm.stack.pop() as unknown
    compilerAssert(isArray(iteratorObjList))

    // Set up result closure. Can we put this in global scope so we don't generate it every time?
    
    const fnParams: ParserFunctionParameter[] = expansion.selectors.map(selector => {
      return { name: selector.elemIdentifier, storage: null, type: null }
    })
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

    const fnBody = new ParseStatements(createAnonymousToken(''), [expansion.loopBodyNode])
    const decl = createAnonymousParserFunctionDecl(`reduced${expansion.debugName}`, createAnonymousToken(''), fnParams, fnBody)
    const funcDef = insertFunctionDefinition(vm.context.subCompilerState.globalCompiler, decl)
    let resultClosure = new Closure(funcDef, vm.context.subCompilerState.scope, vm.context.subCompilerState)
    setOptimiseSimpleIterator(resultClosure)

    const iteratorList: IteratorState[] = iteratorObjList.map((obj, i) => {
      const selector = expansion.selectors[i]
      compilerAssert(!selector.setterIdentifier, "Shouldn't happen") // setterIdentifier is only used in state.setterSelector
      const lets: ParseNode[] = []
      compilerAssert(isAst(obj), "Expected ast", { obj })
      const fnctx: CompilerFunctionCallContext = { location: vm.location, compilerState: vm.context.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
      const closure = iteratableToClosure(fnctx, node.token, obj, selector)
      return { closure, selector, lets }
    })

    if (expansion.optimiseSimple) {
      compilerAssert(iteratorList.length === 1, "optimiseSimple is set to true but expected one iterator")
      compilerAssert(!expansion.setterSelector, "Shouldn't happen?")
      return createCallAstFromValueAndPushValue(vm, iteratorList[0].closure, [], [new CompTimeObjAst(VoidType, vm.location, resultClosure)])
    }

    const initialState: ExpansionZipState = { expansion, setterSelector: expansion.setterSelector, iteratorList, bindingValues: [], 
      lets: [], resultClosure, totalIterators: iteratorList.length, location: vm.location, compilerState: vm.context.subCompilerState }

    return expandZipIterator(initialState).chainFn((task, ast) => {
      vm.stack.push(ast); return Task.success()
    })
  }, [], [expansion.iteratorListIdentifier])

  const lets: ParseNode[] = [metaList]
  const final: ParseNode[] = [finalAst]
  if (expansion.fold) {
    lets.push(new ParseLet(node.token, expansion.fold.iden, null, expansion.fold.initial))
    final.push(expansion.fold.iden)
  }

  const stmts = new ParseStatements(node.token, [...lets, ...final])
  return new ParseBlock(createAnonymousToken(''), 'break', expansion.breakIden, stmts)
}


export const expandDotsSugar = (out: BytecodeWriter, node: ParseExpand) => {
  const expansion = createExpansionState('loop', node.token.location)
  let result = visitExpansion(out, expansion, node.expr)
  if (expansion.fold) expansion.loopBodyNode = new ParseSet(node.token, expansion.fold.iden, result)
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
export const expandFuncLastSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('last', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const iden = new ParseFreshIden(node.token, new FreshBindingToken('last'))
  expansion.loopBodyNode = new ParseSet(node.token, iden, result)
  
  // TODO: Only supports numbers at the moment
  const reduce = compileExpansionToParseNode(out, expansion, node)
  const let_ = new ParseLet(node.token, iden, null, new ParseNumber(createAnonymousToken('0')))
  const value = new ParseStatements(node.token, [let_, reduce, iden])
  visitParseNode(out, value)
}
export const expandFuncFirstSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('first', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const objIden = new ParseFreshIden(node.token, new FreshBindingToken('obj'))
  // const iden = new ParseFreshIden(node.token, new FreshBindingToken('first'))
  const tcResult = new ParseEvalFunc(createAnonymousToken(''), (vm: Vm) => {
    const value = vm.stack.pop()
    const obj = vm.stack.pop() as any
    compilerAssert(isAst(value), "Expected ast", { value, obj })
    obj.type = value.type
    obj.binding = new Binding("", value.type)
    vm.stack.push(new SetAst(VoidType, node.token.location, obj.binding, value))
    // compilerAssert(false, "Not implemented", { obj, value })
    // return Task.of(null)
  }, [new ParseQuote(node.token, result)], [objIden]);
  const set = new ParseStatements(node.token, [
    new ParseMeta(node.token, tcResult)
  ])
  const break_ = new ParseBreak(node.token, expansion.breakIden, null)
  expansion.loopBodyNode = new ParseStatements(node.token, [set, break_])

  // TODO: Only supports numbers at the moment
  const reduce = compileExpansionToParseNode(out, expansion, node)
  // const let_ = new ParseLet(node.token, iden, null, new ParseNumber(createAnonymousToken('0')))
  const letobj = new ParseLetConst(node.token, objIden, new ParseEvalFunc(createAnonymousToken(''), (vm: Vm) => {
    // compilerAssert(false, "Not implemented", { stack: vm.stack })
    vm.stack.push({ thing: 1 })
  }, [], []))
  const stmts = new ParseStatements(node.token, [letobj, reduce])
  // const thing = new ParseMeta(node.token, new ParseCall(createAnonymousToken(''), new ParseIdentifier(createAnonymousToken('thing')), [], []
  visitParseNode(out, new ParseEvalFunc(node.token, (vm) => {
    const obj = vm.stack.pop()
    const stmts = vm.stack.pop()
    compilerAssert(isAst(stmts), "Expected ast", { stmts })
    const binding = obj.binding
    compilerAssert(binding instanceof Binding, "Expected binding", { obj })

    const newStmts = createStatements(vm.location, [
      new LetAst(binding.type, vm.location, binding, new DefaultConsAst(binding.type, vm.location)),
      stmts,
      new BindingAst(binding.type, vm.location, binding)
    ])

    vm.stack.push(newStmts)
    // compilerAssert(false, "Not implemented", { stmts, obj })

  }, [stmts, objIden], []))
}
export const expandFuncMinSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('min', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const iden = new ParseFreshIden(node.token, new FreshBindingToken('min'))
  const comp = new ParseOperator(createAnonymousToken('<'), [result, iden])
  const set_ = new ParseSet(node.token, iden, result)
  expansion.loopBodyNode = new ParseIf(node.token, false, comp, set_, null)

  // TODO: Only supports numbers at the moment
  // TODO: Use optional types later
  const reduce = compileExpansionToParseNode(out, expansion, node)
  const let_ = new ParseLet(node.token, iden, null, new ParseNumber(createAnonymousToken('2147483647')))
  const value = new ParseStatements(node.token, [let_, reduce, iden])
  visitParseNode(out, value)
}
export const expandFuncMaxSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const expansion = createExpansionState('max', node.token.location)
  const result = visitExpansion(out, expansion, node)
  expansion.optimiseSimple = expansion.selectors.length === 1 && !expansion.filterNode

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const iden = new ParseFreshIden(node.token, new FreshBindingToken('max'))
  const comp = new ParseOperator(createAnonymousToken('>'), [result, iden])
  const set_ = new ParseSet(node.token, iden, result)
  expansion.loopBodyNode = new ParseIf(node.token, false, comp, set_, null)

  // TODO: Only supports numbers at the moment
  const reduce = compileExpansionToParseNode(out, expansion, node)
  const let_ = new ParseLet(node.token, iden, null, new ParseNumber(createAnonymousToken('0')))
  const value = new ParseStatements(node.token, [let_, reduce, iden])
  visitParseNode(out, value)
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
    compilerAssert(valueBinding.value instanceof Closure, "Expected function")

    if (canOptimiseSimpleIterator(valueBinding.value)) {
      const forwardingClosure = valueBinding.value
      const iteratorCalls = values_.map((closure, i) => {
        const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }

        if (!(closure instanceof Closure)) { // Support for constant values
          const value = unknownToAst(ctx.location, closure)
          return createCallAstFromValue(fnctx1, forwardingClosure, [], [propagatedLiteralAst(value)])
        }
        return createCallAstFromValue(fnctx1, closure, [], [new CompTimeObjAst(VoidType, ctx.location, forwardingClosure)])
      })
      return Task.all(iteratorCalls).chainFn((task, callAsts) => {
        callAsts.forEach(x => compilerAssert(isAst(x)))
        return Task.of(new StatementsAst(VoidType, ctx.location, callAsts))
      })
      
    }
    const outFn = valueBinding.value

    const interleave = interleaveHelper()

    const iteratorCalls = values_.map((closure, i) => {
      if (!(closure instanceof Closure)) { // Support for constant values
        const value = unknownToAst(ctx.location, closure)
        return Task.of(createStatements(ctx.location, [
          interleave.createSetter(ctx.location, propagatedLiteralAst(value)),
          interleave.continueEntry(ctx.location),
        ]))
      }

      const yieldA = closureHelper(ctx, 'yieldA', ['x'], (valueAst) => {
        compilerAssert(isAst(valueAst), "Expected AST", { valueAst })
        return createStatements(ctx.location, [
          interleave.createSetter(ctx.location, valueAst),
          interleave.continueEntry(ctx.location),
        ])
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
  const fn = closureHelper(ctx, 'concat', ['x'], closureBody)

  return fn
  
})

export const generator = new CompilerFunction("generator", (ctx, typeArgs, args) => {
  const consumeFn = args[0] instanceof CompTimeObjAst ? args[0].value : undefined
  const produceFn = args[1] instanceof CompTimeObjAst ? args[1].value : undefined
  compilerAssert(consumeFn instanceof Closure, "Expected function", { consumeFn })
  compilerAssert(produceFn instanceof Closure, "Expected function", { produceFn })

  return generatorInternal(ctx, (ast) => {
    const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const fn1 = closureHelper(ctx, 'consume', [], () => Task.of(ast))
    return createCallAstFromValue(fnctx1, consumeFn, [], [new CompTimeObjAst(VoidType, ctx.location, fn1)])
  }, (yieldFn) => {
    const fn2 = closureHelper(ctx, 'produce', ['x'], (bindingAst) => {
      compilerAssert(isAst(bindingAst))
      return yieldFn(bindingAst)
    })
    const fnctx2: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    return createCallAstFromValue(fnctx2, produceFn, [], [new CompTimeObjAst(VoidType, ctx.location, fn2)])
  })

})