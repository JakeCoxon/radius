import { BytecodeSecondOrder, getOperatorTable, loadModule, propagateLiteralType, propagatedLiteralAst, pushBytecode, resolveScope, visitParseNode } from "./compiler"
import { compileExportedFunctionTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall, insertFunctionDefinition } from "./compiler_functions"
import { Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd, ParseFold, ParseForExpr, ParseWhileExpr, Module, pushSubCompilerState, createScope, TaskContext, CompilerError, AstType, OperatorAst, CompilerFunction, CallAst, RawPointerType, SubscriptAst, IntType, expectType, SetSubscriptAst, ParserFunctionParameter, FunctionType, Binding, StringType, ValueFieldAst, LetAst, BindingAst, createStatements, StringAst, FloatType, DoubleType, CompilerFunctionCallContext, Vm, expectAst, NumberAst, Type, UserCallAst, hashValues, NeverType, IfAst, BoolType, VoidAst, LoopObject, CompileTimeObjectType, u64Type, FunctionDefinition, ParserFunctionDecl, StatementsAst, isTypeScalar, IntLiteralType, FloatLiteralType, isAst, isType, isTypeCheckError, InterleaveAst, ContinueInterAst, CompTimeObjAst, ParseEvalFunc, SetAst, DefaultConsAst, WhileAst, BoolAst, isArray, ExpansionSelector, IteratorType } from "./defs"
import { Event, Task, TaskDef, isTask } from "./tasks"

export const forLoopSugar = (out: BytecodeWriter, node: ParseFor) => {
  const fnParams: ParserFunctionParameter[] = [{ name: node.identifier, storage: null, type: null }]
  const continueBlock = new ParseBlock(node.token, 'continue', null, new ParseStatements(node.token, [node.body]))
  const decl = createAnonymousParserFunctionDecl("for", node.token, fnParams, continueBlock)
  const fn = new ParseFunction(node.token, decl)
  const iterateFn = new ParseCompilerIden(createAnonymousToken(''), 'iteratefn');
  const call = new ParseCall(node.token, iterateFn, [node.expr], [fn])
  const breakBlock = new ParseBlock(node.token, 'break', null, new ParseStatements(node.token, [call]))
  visitParseNode(out, breakBlock)
}

const rangeLoop = (token: Token, iden: ParseIdentifier | ParseFreshIden, start: ParseNode, end: ParseNode, body: ParseNode) => {
  const letNode = new ParseLet(token, iden, null, start)
  const loopBody = new ParseStatements(token, [body, new ParseOpEq(createAnonymousToken("+="), iden, new ParseNumber(createAnonymousToken('1')))])
  const loop = new ParseWhile(token, new ParseOperator(createAnonymousToken('<'), [iden, end]), loopBody)
  return [letNode, loop]
}
const getLength = (token: Token, expr: ParseNode) => new ParseCall(token, new ParseCompilerIden(createAnonymousToken(''), 'lenfn'), [expr], [])

const minAll = (token: Token, letIden: ParseIdentifier | ParseFreshIden, exprs: ParseNode[]) => {
  // Iteratively compute the minimum of a list of expressions
  const letMin = new ParseLet(token, letIden, null, exprs[0])
  const mins = exprs.flatMap((expr, i) => {
    const tmpIden = new ParseFreshIden(token, new FreshBindingToken("min"))
    const letExpr = new ParseLet(token, tmpIden, null, expr)
    const less = new ParseOperator(createAnonymousToken('<'), [letIden, tmpIden])
    const min = new ParseIf(token, true, less, letIden, new ParseElse(token, tmpIden))
    const set = new ParseSet(token, letIden, min)
    return [letExpr, set]
  })
  return [letMin, ...mins]
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
  const incNode = new ParseOpEq(createAnonymousToken("+="), letIndexNode.name, selector.step ?? new ParseNumber(createAnonymousToken('1')))
  const condNode = new ParseOperator(createAnonymousToken("<"), [letIndexNode.name, letLengthNode.name])
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
  const incNode = new ParseOpEq(createAnonymousToken("+="), letIndexNode.name, selector.step ?? new ParseNumber(createAnonymousToken('1')))
  const condNode = new ParseOperator(createAnonymousToken("<"), [letIndexNode.name, letLengthNode.name])
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

type ExpansionZipState = {
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

const expandZipIterator = (state: ExpansionZipState): Task<Ast, CompilerError> => {

  if (state.iteratorList.length === 0 && state.setterSelector) {
    const location = state.location

    const setter = createArraySetterIterator(createAnonymousToken(''), state.compilerState, state.setterSelector)

    const fnctx1: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const finalProducer = closureHelper(fnctx1, 'finalproducer', [], () => {

      const fnctx2: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
      return (
        createCallAstFromValue(fnctx2, state.resultClosure, [], state.bindingValues)
        .chainFn((task, value) => {
          const stmts = createStatements(location, [...state.lets, value])
          return Task.of(stmts)
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
    //   resultClosure(elem_a, elem_b)

    const location = state.location
    const fnctx2: CompilerFunctionCallContext = { location, compilerState: state.compilerState, resultAst: undefined, typeCheckResult: undefined }
    return (
      createCallAstFromValue(fnctx2, state.resultClosure, [], state.bindingValues)
      .chainFn((task, result) => {
        const stmts = createStatements(location, [...state.lets, result])
        const whileAst = new WhileAst(VoidType, location,  new BoolAst(BoolType, location, true), stmts)
        return Task.of(whileAst)
      })
    )
  }

  const iterator1 = state.iteratorList.shift()
  const location = state.location

  const consume1 = (yieldedValue: Ast): Task<Ast, CompilerError> => {

    const binding = new Binding("", yieldedValue.type)
    const bindingAst = new BindingAst(binding.type, location, binding)

    const letAst = new LetAst(VoidType, location, binding, yieldedValue)
    state.bindingValues.push(bindingAst)
    state.lets.push(letAst)

    return expandZipIterator(state)

  }
  const given1 = (yieldFn: (value: Ast) => Task<Ast, CompilerError>): Task<Ast, CompilerError> => {
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

  return generatorInternal(fnctx2, consume1, given1)

}


export const expandLoopSugar = (out: BytecodeWriter, node: ParseExpand) => {

  // Because we want to use the expansion state to insert a loop construct
  // around the expansion body we need to evaluate the body first.
  // Visit the body of the expansion node but output to a fresh bytecode
  // array which we will append later using a ParseBytecode.
  // This is in combination with sliceSugar below

  const bytecode = { code: [], locations: [] }
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: typeof out.state.expansion = out.state.expansion = { selectors: [], iteratorListIdentifier, fold: null, setterSelector: null }
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.expr)

  const list = new ParseList(node.token, expansion.selectors.map(x => new ParseQuote(node.token, x.node)))
  const letIteratorList = new ParseLet(node.token, iteratorListIdentifier, null, list)

  const metaList = new ParseMeta(node.token, letIteratorList)
  let calledOnce = false

  const finalAst = new ParseEvalFunc(createAnonymousToken(''), (vm) => {
    compilerAssert(!calledOnce, "Error called multiple times")
    calledOnce = true
    
    const iteratorObjList = vm.stack.pop() as unknown
    compilerAssert(isArray(iteratorObjList))

    // Set up result closure. Can we put this in global scope so we don't generate it every time?
    let loopBody: ParseNode = new ParseBytecode(node.token, bytecode)
    if (expansion.fold) {
      loopBody = new ParseSet(node.token, expansion.fold.iden, loopBody)
    }
    
    const fnParams: ParserFunctionParameter[] = expansion.selectors.map(selector => {
      return { name: selector.elemIdentifier, storage: null, type: null }
    })

    const fnBody = new ParseStatements(createAnonymousToken(''), [loopBody])
    const decl = createAnonymousParserFunctionDecl(`reduced`, createAnonymousToken(''), fnParams, fnBody)
    const funcDef = insertFunctionDefinition(vm.context.subCompilerState.globalCompiler, decl)
    const resultClosure = new Closure(funcDef, vm.context.subCompilerState.scope, vm.context.subCompilerState)

    const iteratorList: IteratorState[] = iteratorObjList.map((obj, i) => {
      const selector = expansion.selectors[i]
      const lets: ParseNode[] = []
      if (obj instanceof CompTimeObjAst && obj.value instanceof Closure) {
        compilerAssert(!selector.end, "Not implemented yet")
        compilerAssert(!selector.start, "Not implemented yet")
        compilerAssert(!selector.step, "Not implemented yet")
        compilerAssert(!selector.setterIdentifier, "Not possible to assign to an iterator")
        return { closure: obj.value, selector, lets }
      }

      compilerAssert(selector.indexIdentifier)
      const iterator = createArrayIterator(node.token, vm.context.subCompilerState, selector)
      if (selector.setterIdentifier) {
        lets.push(new ParseMeta(createAnonymousToken(''), 
          new ParseLet(createAnonymousToken(''), selector.setterIdentifier, null,
            new ParseQuote(createAnonymousToken(''), iterator.subscriptIterator))))
      }
      
      return { closure: iterator.closure, selector, lets }
    })

    const initialState: ExpansionZipState = { setterSelector: expansion.setterSelector, iteratorList, bindingValues: [], 
      lets: [], resultClosure, location: vm.location, compilerState: vm.context.subCompilerState }

    return expandZipIterator(initialState).chainFn((task, ast) => {
      vm.stack.push(ast); return Task.success()
    })

  }, [], [iteratorListIdentifier, ])

  const lets: ParseNode[] = [metaList]
  const final: ParseNode[] = [finalAst]
  if (expansion.fold) {
    lets.push(new ParseLet(node.token, expansion.fold.iden, null, expansion.fold.initial))
    final.push(expansion.fold.iden)
  }

  visitParseNode(out, new ParseStatements(node.token, [...lets, ...final]))

  out.state.expansion = null
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

const insertMetaObjectPairwiseOperator = (compiledClass: CompiledClass, operatorName: string, operatorSymbol: string) => {
  const operatorFunc = new CompilerFunction(operatorName, (ctx, typeArgs, args) => {
    const [a, b] = args
    // TODO: Try and move this to be more automatic
    if (a.type === IntLiteralType || a.type === FloatLiteralType) propagateLiteralType(compiledClass.fields[0].fieldType, a)
    if (b.type === IntLiteralType || b.type === FloatLiteralType) propagateLiteralType(compiledClass.fields[0].fieldType, b)
    const bindingAstA = new BindingAst(a.type, ctx.location, new Binding("", a.type))
    const bindingAstB = new BindingAst(b.type, ctx.location, new Binding("", b.type))
    const stmts: Ast[] = [
      new LetAst(VoidType, ctx.location, bindingAstA.binding, a),
      new LetAst(VoidType, ctx.location, bindingAstB.binding, b)]

    const length = compiledClass.fields.length // TODO: Static length

    const getFieldOrScalar = (value: BindingAst, index: number): Ast => {
      const expectedFieldType = compiledClass.fields[index].fieldType

      if (value.type instanceof ParameterizedType && value.type.typeConstructor === TupleTypeConstructor) {
        compilerAssert(value.type.args.length === compiledClass.fields.length, `Expected tuple of size ${length}, got ${value.type.args.length}`, { type: value.type })
        const field = value.type.typeInfo.fields.find(x => x.name === `_${index+1}`)
        compilerAssert(field?.fieldType === expectedFieldType, `Expected type of tuple field ${index+1} to be $fieldType got $otherFieldType`, { fieldType: field?.fieldType, expectedFieldType })
        return new ValueFieldAst(field.fieldType, ctx.location, bindingAstB, [field])
      }
      if (isTypeScalar(value.type)) {
        compilerAssert(value.type === expectedFieldType, "Expected $expectedFieldType got $type", { expectedFieldType, type: value.type })
        return value
      }
      compilerAssert(value.type === compiledClass.type, "Expected vec or tuple. got $type", { type: b.type })
      const field = value.type.typeInfo.fields[index]
      compilerAssert(field.fieldType === expectedFieldType, "Expected $expectedFieldType got $type", { type: field.fieldType, expectedFieldType })
      return new ValueFieldAst(field.fieldType, ctx.location, value, [field])
    }

    const constructorArgs = Array(length).fill(0).map((_, i) => 
      getOperatorTable()[operatorSymbol].func(ctx, 
        getFieldOrScalar(bindingAstA, i), getFieldOrScalar(bindingAstB, i)))
    
    return (
      Task.all(constructorArgs).chainFn((task, constructorArgs) => {
        stmts.push(new ConstructorAst(compiledClass.type, ctx.location, constructorArgs))
        return Task.of(createStatements(ctx.location, stmts))
      }) as Task<Ast, CompilerError>
    )
  })
  compiledClass.metaobject[operatorName] = operatorFunc
}

export const VecTypeMetaClass = new ExternalFunction('VecType', VoidType, (ctx, args) => {
  const compiledClass = args[0]
  compilerAssert(compiledClass instanceof CompiledClass)
  insertMetaObjectPairwiseOperator(compiledClass, "add", "+")
  insertMetaObjectPairwiseOperator(compiledClass, "sub", "-")
  insertMetaObjectPairwiseOperator(compiledClass, "mul", "*")
  insertMetaObjectPairwiseOperator(compiledClass, "div", "/")

  const operatorFunc = new ExternalFunction("static_subscript", VoidType, (ctx, args) => {
    const [index, value] = args
    compilerAssert(isAst(value), "Expected AST", { value })
    compilerAssert(typeof index === 'number', "Expected number")
    compilerAssert(index >= 0 && index < compiledClass.fields.length, "Index out of bounds $index", { index })
    const field = compiledClass.fields[index]
    const bindingAst = new BindingAst(value.type, ctx.location, new Binding("", value.type))
    return createStatements(ctx.location, [
      new LetAst(VoidType, ctx.location, bindingAst.binding, value),
      new ValueFieldAst(field.fieldType, ctx.location, bindingAst, [field]),
    ])
  })
  compiledClass.metaobject["static_subscript"] = operatorFunc
  compiledClass.metaobject["static_length"] = compiledClass.fields.length

})

export const defaultMetaFunction = (subCompilerState: SubCompilerState, compiledClass: CompiledClass, definitionScope: Scope, templateScope: Scope) => {
  const iterate = templateScope['__iterate']
  compilerAssert(!iterate || iterate instanceof Closure)
  const subscript = templateScope['__subscript']
  compilerAssert(!subscript || subscript instanceof Closure)
  const set_subscript = templateScope['__set_subscript']
  compilerAssert(!set_subscript || set_subscript instanceof Closure)

  if (compiledClass.classDefinition.keywords.includes('struct'))
    compiledClass.type.typeInfo.isReferenceType = false

  const fnParams: ParserFunctionParameter[] = compiledClass.fields.map(x => 
    ({ name: new ParseIdentifier(createAnonymousToken(x.name)), storage: null,
    type: new ParseValue(createAnonymousToken(''), x.fieldType)}) as ParserFunctionParameter)
  const constructorBody = new ParseConstructor(
    createAnonymousToken(''), 
    new ParseValue(createAnonymousToken(''), compiledClass.type), 
    compiledClass.fields.map(x => new ParseIdentifier(createAnonymousToken(x.name))))
  const decl = createAnonymousParserFunctionDecl(`${compiledClass.debugName} constructor`, createAnonymousToken(''), fnParams, constructorBody)
  const funcDef = insertFunctionDefinition(subCompilerState.globalCompiler, decl)
  const constructor = new Closure(funcDef, definitionScope, subCompilerState.lexicalParent!)

  Object.assign(compiledClass.metaobject, { iterate, subscript, set_subscript, constructor })
}

export const createListConstructor = (vm: Vm, elementType: Type, values: Ast[]) => {

  // TODO: Prefer to do some bytecode manipulations here instead?

  let module: Module
  let array: Ast
  let binding: Binding
  const callArray: Ast[] = []
  return (
    TaskDef(loadModule, vm.location, 'array')
    .chainFn((task, module_) => {
      module = module_
      return TaskDef(resolveScope, module.compilerState.scope, 'array_create')
    })
    .chainFn((task, func: Closure) => {
      return createCallAstFromValueAndPushValue(vm, func, [elementType], [new NumberAst(IntType, vm.location, 0)])
    })
    .chainFn((task, _) => {
      array = expectAst(vm.stack.pop())
      binding = new Binding("", array.type)
      
      const calls = values.map(ast => {
        return (
          createMethodCall(vm, new BindingAst(binding.type, vm.location, binding), 'append', [elementType], [ast])
          .chainFn((task, _) => { const ast = expectAst(vm.stack.pop()); callArray.push(ast); return Task.success() })
        )
      })
      const reducedTasks = calls.reduce((acc, nextTask) => acc.chainFn((task, _) => nextTask))
      return reducedTasks
    })
    .chainFn((task, _) => {
      const stmts = createStatements(vm.location, [
        new LetAst(VoidType, vm.location, binding, array),
        ...callArray,
        new BindingAst(binding.type, vm.location, binding)
      ])
      vm.stack.push(stmts)
      return Task.success()
    })
  )
}

export const externalBuiltinBindings: {[key:string]: Binding} = {
  print: new Binding('print', FunctionType),
  printf: new Binding('printf', FunctionType),
  malloc: new Binding('malloc', FunctionType),
  realloc: new Binding('realloc', FunctionType),
  free: new Binding('free', FunctionType),
  sizeof: new Binding('sizeof', FunctionType),
  exit: new Binding('exit', FunctionType),
}

export const assert = new CompilerFunction('assert', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const op = args[0]
  compilerAssert(op.type == BoolType, "Expected bool")
  compilerAssert(op instanceof OperatorAst, "Expected operator")
  const location = ctx.location
  const globalCompiler = ctx.compilerState.globalCompiler

  // TODO: Make this better
  const name = 'exit'
  const existing = globalCompiler.externalDefinitions.find(x => x.name === name)
  const concreteTypes = [IntType]
  const paramHash = hashValues(concreteTypes)
  compilerAssert(!existing || existing.paramHash === paramHash, "Function exists with different param hash", { existing })
  const binding = externalBuiltinBindings.exit
  if (!existing) globalCompiler.externalDefinitions.push({ name: name, binding, paramHash, paramTypes: concreteTypes, returnType: NeverType })

  // Gotta be a nicer way to do this automatically
  const left = new LetAst(VoidType, location, new Binding("", op.args[0].type), op.args[0])
  const right = new LetAst(VoidType, location, new Binding("", op.args[1].type), op.args[1])
  const leftBinding = new BindingAst(left.binding.type, location, left.binding)
  const rightBinding = new BindingAst(right.binding.type, location, right.binding)
  const newOp = new OperatorAst(op.type, location, op.operator, [leftBinding, rightBinding])

  return (
    print.func(ctx, [], [new StringAst(StringType, location, 'Expected'), leftBinding, new StringAst(StringType, location, op.operator), rightBinding])
    .chainFn((task, printResult) => {
      return Task.of(createStatements(location, [
        left, right,
        new IfAst(VoidType, location, newOp, new VoidAst(VoidType, location), 
          createStatements(location, [
            printResult,
            new UserCallAst(VoidType, location, externalBuiltinBindings.exit, [new NumberAst(IntType, location, 1)])
          ])
        )
      ]))
    })
  )
})

export const print = new CompilerFunction('print', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const location = ctx.location
  // compilerAssert(args.length === 1 && args[0].type !== VoidType , "Expected non void argument", { args })
  const stmts: Ast[] = []
  let formatStr = ''
  const printfArgs: Ast[] = []

  const fieldHelper = (binding: Binding, name: string) => {
    const field = binding.type.typeInfo.fields.find(x => x.name === name)!
    return new ValueFieldAst(field.fieldType, location, new BindingAst(binding.type, location, binding), [field])
  }
  const formats = new Map()
  formats.set(IntType, '%i')
  formats.set(u64Type, '%i')
  formats.set(RawPointerType, '%p')
  formats.set(FloatType, '%f')
  formats.set(DoubleType, '%f')

  args.forEach((arg, i) => {
    if (i !== 0) formatStr += ' '
    if (arg.type === StringType && arg instanceof StringAst) {
      // Constant strings
      formatStr += arg.value
    } else if (arg.type === StringType) {
      const binding = new Binding("", StringType)
      stmts.push(new LetAst(VoidType, location, binding, arg))
      const lengthGetter = fieldHelper(binding, 'length')
      const dataGetter = fieldHelper(binding, 'data')
      formatStr += '%.*s'
      printfArgs.push(lengthGetter, dataGetter)
    } else if (arg.type === BoolType) {
      printfArgs.push(arg)
      formatStr += '%i'
    } else if (formats.has(arg.type)) {
      printfArgs.push(arg)
      formatStr += formats.get(arg.type)
    } else if (arg.type instanceof ParameterizedType && arg.type.typeConstructor === TupleTypeConstructor) {
      const binding = new Binding("", arg.type)
      stmts.push(new LetAst(VoidType, location, binding, arg))
      formatStr += `(`
      const fieldsToPrint = binding.type.typeInfo.fields.filter(x => formats.has(x.fieldType))
      fieldsToPrint.forEach((field, j) => {
        if (j !== 0) formatStr += ', '
        const getter = fieldHelper(binding, field.name)
        formatStr += formats.get(field.fieldType)
        printfArgs.push(getter)
      })
      formatStr += ')'
    } else if (arg.type.typeInfo.fields.length && !arg.type.typeInfo.isReferenceType) {
      const binding = new Binding("", arg.type)
      stmts.push(new LetAst(VoidType, location, binding, arg))
      formatStr += `${arg.type.shortName}(`
      const fieldsToPrint = binding.type.typeInfo.fields.filter(x => formats.has(x.fieldType))
      fieldsToPrint.forEach((field, j) => {
        if (j !== 0) formatStr += ', '
        const getter = fieldHelper(binding, field.name)
        formatStr += `${field.name}=`
        formatStr += formats.get(field.fieldType)
        printfArgs.push(getter)
      })
      formatStr += ')'
    }
  })
  formatStr += '\n'
  const formatBinding = new Binding("", StringType)
  stmts.unshift(new LetAst(VoidType, location, formatBinding, new StringAst(StringType, location, formatStr)))
  printfArgs.unshift(fieldHelper(formatBinding, 'data'))
  stmts.push(new CallAst(VoidType, location, externalBuiltinBindings.printf, printfArgs, []))
  return Task.of(createStatements(location, stmts))
})

export const unsafe_subscript = new CompilerFunction('unsafe_subscript', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [left, right] = args
  propagatedLiteralAst(right)
  compilerAssert(right && right.type === IntType, "Expected int type", { right })
  compilerAssert(left && left.type === RawPointerType, "Expected rawptr", { left })
  const type = expectType(typeArgs[0])
  return Task.of(new SubscriptAst(type, ctx.location, left, propagatedLiteralAst(right)))
})
export const unsafe_set_subscript = new CompilerFunction('unsafe_set_subscript', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [left, right, value] = args
  propagatedLiteralAst(right)
  compilerAssert(right && right.type === IntType, "Expected int type", { right })
  compilerAssert(left && left.type === RawPointerType, "Expected rawptr", { left })
  compilerAssert(value, "Expected value", { value })
  return Task.of(new SetSubscriptAst(VoidType, ctx.location, left, propagatedLiteralAst(right), propagatedLiteralAst(value)))
})
export const operator_bitshift_left = new CompilerFunction('operator_bitshift_left', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return Task.of(new OperatorAst(IntType, ctx.location, "<<", [a, b]))
})
export const operator_bitshift_right = new CompilerFunction('operator_bitshift_right', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return Task.of(new OperatorAst(IntType, ctx.location, ">>", [a, b]))
})
export const operator_bitwise_and = new CompilerFunction('operator_bitwise_and', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return Task.of(new OperatorAst(IntType, ctx.location, "&", [a, b]))
})
export const operator_bitwise_or = new CompilerFunction('operator_bitwise_or', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return Task.of(new OperatorAst(IntType, ctx.location, "|", [a, b]))
})
export const operator_mod = new CompilerFunction('operator_mod', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === u64Type, "Expected int type", { a })
  compilerAssert(b && b.type === u64Type, "Expected int type", { b })
  return Task.of(new OperatorAst(u64Type, ctx.location, "mod", [a, b]))
})
export const static_length = new ExternalFunction('static_length', VoidType, (ctx, args: Ast[]) => {
  let type: unknown = args[0]
  if (type instanceof Binding) type = type.type
  compilerAssert(isType(type), "Expected type or binding got $type", { type })
  const static_length = type.typeInfo.metaobject['static_length']
  compilerAssert(static_length, "Expected 'static_length' metafield for $type", { type })
  return static_length
})

const add_external_library = new ExternalFunction("add_external_library", VoidType, (ctx: CompilerFunctionCallContext, args) => {
  compilerAssert(typeof args[0] == 'string', "Expected string")
  ctx.compilerState.globalCompiler.externalCompilerOptions.libraries.push(args[0])
})
const add_macos_framework = new ExternalFunction("add_macos_framework", VoidType, (ctx: CompilerFunctionCallContext, args) => {
  compilerAssert(typeof args[0] == 'string', "Expected string")
  ctx.compilerState.globalCompiler.externalCompilerOptions.macosFrameworks.push(args[0])
})

const get_current_loop = new ExternalFunction("get_current_loop", VoidType, (ctx: CompilerFunctionCallContext, args) => {
  let b = ctx.compilerState.labelBlock
  while (b) { if (b.breakType === 'continue') break; b = b.parent }
  // I'm making the assumption that are loops have a continue and a direct parent of break. Must make sure this is always the case
  compilerAssert(b && b.parent?.breakType === 'break', "Couldn't find current loop")
  const loop = new LoopObject(b, b.parent)
  return loop
})

const overloaded = new ExternalFunction("overloaded", VoidType, (ctx: CompilerFunctionCallContext, args) => {
  const funcs = args[0] as Closure[]
  compilerAssert(Array.isArray(funcs) && funcs.every(x => x instanceof Closure), "Expected functions", { funcs })

  return new CompilerFunction('overload', (fnctx, typeArgs: unknown[], args: Ast[]) => {
    const contexts: CompilerFunctionCallContext[] = funcs.map((func, i) => ({...fnctx}))

    return (
      (funcs.map((func, i) => createCallAstFromValue(contexts[i], func, [], args)) as Task<Ast, CompilerError> [])
      .reduce((acc, funcCall, i) => acc.chainRejected((err) => {
        if (contexts[i - 1].typeCheckResult!.checkFailed) return funcCall
        return Task.rejected(err)
      }))
      .chainRejected(err => {
        // Check if the error was something other than type check, in which case propagate it
        if (!contexts.some(x => x.typeCheckResult!.checkFailed)) return Task.rejected(err)
        const names = funcs.map(x => x.func.debugName).join(', ')
        compilerAssert(false, `Could not find overload that matches for ${names}`, { args, err })
      })
    )
  })
})

export const assert_compile_error = new CompilerFunction("assert_compile_error", (ctx, typeArgs, args): Task<Ast, CompilerError> => {
  const func = typeArgs[0]
  const errorMsg = typeArgs[1]
  compilerAssert(typeof errorMsg == 'string', "Expected string")
  compilerAssert(func instanceof Closure, "Expected function")
  const fnctx: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }

  return (
    createCallAstFromValue(fnctx, func, [], [])
    .chainFn((task, v) => {
      compilerAssert(false, "Expected compile to fail but it didn't", { assertCompileError: true })
    })
    .chainRejected((err) => {
      if ((err.info as any).assertCompileError) return Task.rejected(err)
      if (err.message.includes(errorMsg)) return Task.of(new VoidAst(VoidType, ctx.location))
      return Task.rejected(err)
    })
  )
  
})

export const initializer_function = new CompilerFunction("initializer_function", (ctx, typeArgs, args) => {
  return Task.of(new UserCallAst(VoidType, ctx.location, ctx.compilerState.globalCompiler.initializerFunctionBinding, []))
})

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
  const interleaveBinding = new Binding("interleave", VoidType)
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
      compilerAssert(entryParamEvent._success.type === type, "Mismatch types", { type, type2: entryParamEvent._success.type, entry: entryParamEvent._success })
      return entryParamEvent._success
    }
    const entryParam = new Binding('entryParam', type)
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

export const concat = new ExternalFunction("concat", VoidType, (ctx, values) => {
  
  const [a, b] = values
  compilerAssert(a instanceof Closure, "Expected function")
  compilerAssert(b instanceof Closure, "Expected function")

  const closureBody = (valueBinding: unknown): Task<Ast, CompilerError> => {
    compilerAssert(valueBinding instanceof CompTimeObjAst)
    compilerAssert(valueBinding.value instanceof Closure, "Expected function")
    const outFn = valueBinding.value

    const interleave = interleaveHelper()

    const yieldA = closureHelper(ctx, 'yieldA', ['x'], (bindingAst) => {
      compilerAssert(bindingAst instanceof BindingAst)
      return createStatements(ctx.location, [
        interleave.createSetter(ctx.location, bindingAst),
        interleave.continueEntry(ctx.location),
      ])
    })
    const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const callA = createCallAstFromValue(fnctx1, a, [], [new CompTimeObjAst(VoidType, ctx.location, yieldA)])

    const yieldB = closureHelper(ctx, 'yieldB',['x'], (bindingAst) => {
      compilerAssert(bindingAst instanceof BindingAst)
      return createStatements(ctx.location, [
        interleave.createSetter(ctx.location, bindingAst),
        interleave.continueEntry(ctx.location)
      ])
    })
    const fnctx2: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
    const callB = createCallAstFromValue(fnctx2, b, [], [new CompTimeObjAst(VoidType, ctx.location, yieldB)])

    const callC = (
      interleave.waitForEntryBinding()
      .chainFn((task, entryParam) => {
        const fnctx: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
        return (
          createCallAstFromValue(fnctx, outFn, [], [new BindingAst(entryParam.type, ctx.location, entryParam)])
          .chainFn((task, arg) => {
            const alt = interleave.continueOther(ctx.location)
            const stmts = createStatements(ctx.location, [arg, alt])
            const loop = new WhileAst(VoidType, ctx.location, new BoolAst(BoolType, ctx.location, true), stmts)
            return Task.of(loop)
          })
        )
      })
    )

    return (
      Task.concurrency<unknown, CompilerError>([callA, callB, callC, interleave.waitForEntryBinding()])
      .chainFn((task, args) => {
        const [a, b, c, entryParam] = args

        compilerAssert(isAst(a))
        compilerAssert(isAst(b))
        compilerAssert(isAst(c))
        compilerAssert(entryParam instanceof Binding)
        compilerAssert(ctx.location)

        const entryStmts = createStatements(ctx.location, [a, b])
        const stmts = createStatements(ctx.location, [
          new LetAst(VoidType, ctx.location, entryParam, new DefaultConsAst(entryParam.type, ctx.location)),
          interleave.buildAst(ctx.location, entryStmts, c)
        ])

        return Task.of(stmts)
        
      })
    )
    
  }
  const fn = closureHelper(ctx, 'concat',['x'], closureBody)

  return fn
  
})


export const generatorInternal = (
  ctx: CompilerFunctionCallContext, 
  consumeFunc: (yieldedValue: Ast) => Task<Ast, CompilerError>, 
  givenFunc: (yieldFn: (value: Ast) => Task<Ast, CompilerError>) => Task<Ast, CompilerError>
): Task<Ast, CompilerError> => {

  const interleave = interleaveHelper()

  const yieldA = (
    interleave.waitForEntryBinding()
    .chainFn((task, entryParam) => {
      const stmts = createStatements(ctx.location, [
        interleave.continueEntry(ctx.location),
        new BindingAst(entryParam.type, ctx.location, entryParam)
      ])
      return consumeFunc(stmts)
    })
  ); 
  const yieldFn = (value: Ast): Task<Ast, CompilerError> => {
    compilerAssert(isAst(value))
    const stmts = createStatements(ctx.location, [
      interleave.createSetter(ctx.location, value),
      interleave.continueOther(ctx.location)])
    return Task.of(stmts)
  }
  const yieldB = givenFunc(yieldFn)

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

export const generator = new CompilerFunction("generator", (ctx, typeArgs, args) => {
  const consumeFunc = args[0] instanceof CompTimeObjAst ? args[0].value : undefined
  const givenFunc = args[1] instanceof CompTimeObjAst ? args[1].value : undefined
  compilerAssert(consumeFunc instanceof Closure, "Expected function", { consumeFunc })
  compilerAssert(givenFunc instanceof Closure, "Expected function", { givenFunc })

  const interleave = interleaveHelper()
  

  const fn1 = closureHelper(ctx, 'fn1',[], () => {
    return (
      interleave.waitForEntryBinding()
      .chainFn((task, entryParam) => {
        const stmts = createStatements(ctx.location, [
          interleave.continueEntry(ctx.location),
          new BindingAst(entryParam.type, ctx.location, entryParam)
        ])
        return Task.of(stmts)
      })
    )
  })
  const fnctx1: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
  const yieldA = createCallAstFromValue(fnctx1, consumeFunc, [], [new CompTimeObjAst(VoidType, ctx.location, fn1)])

  const fn2 = closureHelper(ctx, 'fn2',['x'], (bindingAst) => {
    compilerAssert(isAst(bindingAst))

    const stmts = createStatements(ctx.location, [
      interleave.createSetter(ctx.location, bindingAst),
      interleave.continueOther(ctx.location),
    ])
    return Task.of(stmts)
  })
  const fnctx2: CompilerFunctionCallContext = { location: ctx.location, compilerState: ctx.compilerState, resultAst: undefined, typeCheckResult: undefined }
  const yieldB = createCallAstFromValue(fnctx2, givenFunc, [], [new CompTimeObjAst(VoidType, ctx.location, fn2)])

  return (
    Task.concurrency<unknown, CompilerError>([
      yieldA, yieldB, 
      interleave.waitForEntryBinding()
    ])
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

export const add_export = new ExternalFunction("add_export", VoidType, (ctx, values) => {
  const [name, closure] = values
  compilerAssert(typeof name === 'string', "Expected string", { name })
  compilerAssert(closure instanceof Closure, "Expected function", { closure })
  return (
    TaskDef(compileExportedFunctionTask, { exportName: name, closure })
    .chainFn((task, value) => { return Task.of(null) })
  )
})

export const createCompilerModuleTask = (ctx: TaskContext): Task<Module, CompilerError> => {
  const moduleScope = createScope({}, undefined)
  Object.assign(moduleScope, { 
    unsafe_subscript, unsafe_set_subscript, operator_bitshift_left, operator_bitshift_right,
    operator_bitwise_and, operator_bitwise_or, rawptr: RawPointerType, add_external_library, add_macos_framework, assert, never: NeverType,
    get_current_loop, ctobj: CompileTimeObjectType, operator_mod, overloaded, static_length, assert_compile_error, initializer_function, add_export,
    generator, concat })
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `compiler module`, lexicalParent: undefined, scope: moduleScope })
  const module = new Module('compiler', subCompilerState, null!)
  return Task.of(module)
}

export const preloadModuleText = () => {
  return `
import compiler for rawptr, overloaded, never

fn iterate!(f, T)(list: List!T) @inline @method:
  i := 0
  while i < list.length:
    f(list[i])
    i += 1

# This is needed for expansion operator
fn length!(T)(list: List!T) @inline @method:
  list.length

fn malloc(size: int) -> rawptr @external
fn realloc(ptr: rawptr, new_size: int) -> rawptr @external
fn free(ptr: rawptr) @external
fn sizeof!(T)() @external

@@external("fmod")
fn fmod_double(t: double, b: double) -> double 
@@external("fmodf")
fn fmod_float(t: float, b: float) -> float
fmod :: overloaded([fmod_double, fmod_float])

fn abs_int(v: int) -> int:
  ifx v < 0: -1 * v else: v
fn abs_float(v: float) -> float:
  ifx v < 0.0: -1.0 * v else: v
abs :: overloaded([abs_int, abs_float])

@@external("sin")
fn sin_double(t: double) -> double
fn sin_float(t: float) -> float:
  float(sin_double(double(t)))
sin :: overloaded([sin_double, sin_float])

@@external("cos")
fn cos_double(t: double) -> double
fn cos_float(t: float) -> float:
  float(cos_double(double(t)))
cos :: overloaded([cos_double, cos_float])

@@external("tan")
fn tan_double(t: double) -> double
fn tan_float(t: float) -> float:
  float(tan_double(double(t)))
tan :: overloaded([tan_double, tan_float])

@@external("sqrt")
fn sqrt_double(t: double) -> double 
fn sqrt_float(t: float) -> float:
  float(sqrt_double(double(t)))
sqrt :: overloaded([sqrt_double, sqrt_float])

fn min!(T)(a: T, b: T) -> T @inline:
  ifx a <= b: a else: b
fn max!(T)(a: T, b: T) -> T @inline:
  ifx a >= b: a else: b

fn exit(status: int) -> never @external
fn unreachable() -> never @inline:
  print("Unreachable code")
  exit(1)

PI :: 3.14159265359

`
}