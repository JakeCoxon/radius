import { BytecodeSecondOrder, compileFunctionPrototype, getOperatorTable, loadModule, propagateLiteralType, propagatedLiteralAst, pushBytecode, resolveScope, unknownToAst, visitParseNode } from "./compiler"
import { compileExportedFunctionTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall, insertFunctionDefinition } from "./compiler_functions"
import { Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd, ParseFold, ParseForExpr, ParseWhileExpr, Module, pushSubCompilerState, createScope, TaskContext, CompilerError, AstType, OperatorAst, CompilerFunction, CallAst, RawPointerType, SubscriptAst, IntType, expectType, SetSubscriptAst, ParserFunctionParameter, FunctionType, Binding, StringType, ValueFieldAst, LetAst, BindingAst, createStatements, StringAst, FloatType, DoubleType, CompilerFunctionCallContext, Vm, expectAst, NumberAst, Type, UserCallAst, hashValues, NeverType, IfAst, BoolType, VoidAst, LoopObject, CompileTimeObjectType, u64Type, FunctionDefinition, ParserFunctionDecl, StatementsAst, isTypeScalar, IntLiteralType, FloatLiteralType, isAst, isType, isTypeCheckError, InterleaveAst, ContinueInterAst, CompTimeObjAst, ParseEvalFunc, SetAst, DefaultConsAst, WhileAst, BoolAst, isArray, ExpansionSelector, ParseNote, ExpansionCompilerState, ParseBoolean, ParseOr, ParseBreak, filterNotNull, ParseTuple } from "./defs"
import { Event, Task, TaskDef, isTask } from "./tasks"


type ExpansionZipState = {
  expansion: ExpansionCompilerState,
  setterSelector: ExpansionSelector | null,
  iteratorList: IteratorState[],
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
const canOptimiseSimpleIterator = (closure: Closure) => {
  return (closure as any)._canOptimiseSimpleIterator
}
const setOptimiseSimpleIterator = (closure: Closure) => {
  (closure as any)._canOptimiseSimpleIterator = true
}

const getLength = (token: Token, expr: ParseNode) => new ParseCall(token, new ParseCompilerIden(createAnonymousToken(''), 'lenfn'), [expr], [])

export const forLoopSugar = (out: BytecodeWriter, node: ParseFor) => {
  const fnIden = node.left instanceof ParseIdentifier ? node.left : new ParseFreshIden(node.token, new FreshBindingToken('it'))
  const fnParams: ParserFunctionParameter[] = [{ name: fnIden, storage: null, type: null }]
  const extract = node.left instanceof ParseTuple ? new ParseLet(node.token, node.left, null, fnIden) : null
  const continueBlock = new ParseBlock(node.token, 'continue', null, new ParseStatements(node.token, filterNotNull([extract, node.body])))
  const decl = createAnonymousParserFunctionDecl("for", node.token, fnParams, continueBlock)
  const fn = new ParseFunction(node.token, decl)
  const iterateFn = new ParseCompilerIden(createAnonymousToken(''), 'iteratefn');
  const call = new ParseCall(node.token, iterateFn, [node.expr], [fn])
  const breakBlock = new ParseBlock(node.token, 'break', null, new ParseStatements(node.token, [call]))
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


const createArrayIterator = (token: Token, subCompilerState: SubCompilerState, selector: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, setterIdentifier: ParseNode | null, indexIdentifier: ParseFreshIden | null }) => {

  const yieldParam = new ParseFreshIden(token, new FreshBindingToken('yield'))
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
  const loopBody = new ParseCall(createAnonymousToken(''), yieldParam, [subscriptIterator], [])
  const whileStmts = new ParseStatements(token, [loopBody, incNode])
  const loop = new ParseWhile(token, condNode, whileStmts)

  const fnBody = new ParseStatements(createAnonymousToken(''), [letIndexNode, letLengthNode, loop])
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
  const consumedValue = new ParseFreshIden(token, new FreshBindingToken('foo'))
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
        // compilerAssert(false, "Not implemented yet", { args })
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

  if (state.expansion.optimiseSimple && state.iteratorList.length === 1) {
    compilerAssert(!state.setterSelector, "Shouldn't happen?")
    const iterator1 = state.iteratorList.shift()!
    const fnctx2: CompilerFunctionCallContext = { location: location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
    return createCallAstFromValue(fnctx2, iterator1.closure, [], [new CompTimeObjAst(VoidType, location, state.resultClosure)])
  }

  if (state.iteratorList.length === 0 && state.setterSelector) {

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

    const fnBody = new ParseStatements(createAnonymousToken(''), [expansion.loopBodyNode])
    const decl = createAnonymousParserFunctionDecl(`reduced${expansion.debugName}`, createAnonymousToken(''), fnParams, fnBody)
    const funcDef = insertFunctionDefinition(vm.context.subCompilerState.globalCompiler, decl)
    let resultClosure = new Closure(funcDef, vm.context.subCompilerState.scope, vm.context.subCompilerState)
    setOptimiseSimpleIterator(resultClosure)

    const iteratorList: IteratorState[] = iteratorObjList.map((obj, i) => {
      const selector = expansion.selectors[i]
      compilerAssert(!selector.setterIdentifier, "Shouldn't happen") // setterIdentifier is only used in state.setterSelector
      const lets: ParseNode[] = []
      if (obj instanceof CompTimeObjAst && obj.value instanceof Closure) {
        let closure = obj.value
        if (selector.start || selector.end || selector.step) {
          closure = createIteratorSliceIterator(node.token, vm.context.subCompilerState, selector, closure)
        }
        return { closure, selector, lets }
      }
      compilerAssert(selector.indexIdentifier)
      const iterator = createArrayIterator(node.token, vm.context.subCompilerState, selector)
      return { closure: iterator.closure, selector, lets }
    })

    const initialState: ExpansionZipState = { expansion, setterSelector: expansion.setterSelector, iteratorList, bindingValues: [], 
      lets: [], resultClosure, location: vm.location, compilerState: vm.context.subCompilerState }

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

  return new ParseStatements(node.token, [...lets, ...final])
}


export const expandLoopSugar = (out: BytecodeWriter, node: ParseExpand) => {

  // Because we want to use the expansion state to insert a loop construct
  // around the expansion body we need to evaluate the body first.
  // Visit the body of the expansion node but output to a fresh bytecode
  // array which we will append later using a ParseBytecode.
  // This is in combination with sliceSugar below

  compilerAssert(!out.state.expansion, "Already in expansion state")

  const bytecode = { code: [], locations: [] }
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: ExpansionCompilerState = out.state.expansion = { debugName: '', loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.expr)
  out.state.expansion = null

  expansion.loopBodyNode = new ParseBytecode(node.token, bytecode)
  if (expansion.fold) {
    expansion.loopBodyNode = new ParseSet(node.token, expansion.fold.iden, expansion.loopBodyNode)
  }
  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}

export const expandFuncAllSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const bytecode = { code: [], locations: [] }
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: ExpansionCompilerState = out.state.expansion = { debugName: 'all', optimiseSimple: true,
    loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
  
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node)
  out.state.expansion = null

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  expansion.fold = { iden: new ParseFreshIden(node.token, new FreshBindingToken('fold_iden')), initial: new ParseBoolean((createAnonymousToken('true'))) }
  expansion.loopBodyNode = new ParseBytecode(node.token, bytecode)
  expansion.loopBodyNode = new ParseAnd(createAnonymousToken('and'), [expansion.loopBodyNode, expansion.fold.iden])
  expansion.loopBodyNode = new ParseSet(node.token, expansion.fold.iden, expansion.loopBodyNode)

  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}

export const expandFuncAnySugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const bytecode = { code: [], locations: [] }
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: ExpansionCompilerState = out.state.expansion = { debugName: 'any', optimiseSimple: true,
    loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
  
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node)
  out.state.expansion = null

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  expansion.fold = { iden: new ParseFreshIden(node.token, new FreshBindingToken('fold_iden')), initial: new ParseBoolean((createAnonymousToken('false'))) }
  expansion.loopBodyNode = new ParseBytecode(node.token, bytecode)
  expansion.loopBodyNode = new ParseOr(createAnonymousToken('or'), [expansion.loopBodyNode, expansion.fold.iden])
  expansion.loopBodyNode = new ParseSet(node.token, expansion.fold.iden, expansion.loopBodyNode)

  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}
export const expandFuncSumSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const bytecode = { code: [], locations: [] }
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: ExpansionCompilerState = out.state.expansion = { debugName: 'sum', optimiseSimple: true,
    loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
  
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node)
  out.state.expansion = null

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  expansion.fold = { iden: new ParseFreshIden(node.token, new FreshBindingToken('fold_iden')), initial: new ParseNumber((createAnonymousToken('0'))) }
  expansion.loopBodyNode = new ParseBytecode(node.token, bytecode)
  expansion.loopBodyNode = new ParseOperator(createAnonymousToken('+'), [expansion.loopBodyNode, expansion.fold.iden])
  expansion.loopBodyNode = new ParseSet(node.token, expansion.fold.iden, expansion.loopBodyNode)

  visitParseNode(out, compileExpansionToParseNode(out, expansion, node))
}
export const expandFuncLastSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const bytecode = { code: [], locations: [] }
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: ExpansionCompilerState = out.state.expansion = { debugName: 'last', optimiseSimple: true,
    loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
  
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node)
  out.state.expansion = null

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const iden = new ParseFreshIden(node.token, new FreshBindingToken('last'))
  expansion.loopBodyNode = new ParseSet(node.token, iden, new ParseBytecode(node.token, bytecode))
  
  // TODO: Only supports numbers at the moment
  const reduce = compileExpansionToParseNode(out, expansion, node)
  const let_ = new ParseLet(node.token, iden, null, new ParseNumber(createAnonymousToken('0')))
  const value = new ParseStatements(node.token, [let_, reduce, iden])
  visitParseNode(out, value)
}
export const expandFuncFirstSugar = (out: BytecodeWriter, noteNode: ParseNote, args: ParseNode[]) => {
  compilerAssert(!out.state.expansion, "Already in expansion state")
  const node = args[0]
  const bytecode = { code: [], locations: [] }
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: ExpansionCompilerState = out.state.expansion = { debugName: 'first', optimiseSimple: true,
    loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
  
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node)
  out.state.expansion = null

  compilerAssert(!expansion.fold, "Fold not supported in this context")

  const iden = new ParseFreshIden(node.token, new FreshBindingToken('first'))
  const set = new ParseSet(node.token, iden, new ParseBytecode(node.token, bytecode))
  const blockName = new ParseFreshIden(node.token, new FreshBindingToken('block'))
  
  expansion.loopBodyNode = new ParseStatements(node.token, [set, new ParseBreak(node.token, blockName, null)])

  // TODO: Only supports numbers at the moment
  const reduce = compileExpansionToParseNode(out, expansion, node)
  const namedBlock = new ParseBlock(node.token, 'break', blockName, reduce)
  const let_ = new ParseLet(node.token, iden, null, new ParseNumber(createAnonymousToken('0')))
  const value = new ParseStatements(node.token, [let_, namedBlock, iden])
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
    const bytecode = { code: [], locations: [] }
    const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

    const expansion: ExpansionCompilerState = out.state.expansion = { debugName: "concat", loopBodyNode: null, selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
    
    visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node)
    out.state.expansion = null

    if (expansion.selectors.length === 0) {
      nodes.push(node)
      continue
    }

    compilerAssert(!expansion.fold, "Fold not supported in this context")

    const param = new ParseFreshIden(node.token, new FreshBindingToken('yieldFn'))
    expansion.loopBodyNode = new ParseBytecode(node.token, bytecode)
    expansion.loopBodyNode = new ParseCall(createAnonymousToken(''), param, [expansion.loopBodyNode], [])
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