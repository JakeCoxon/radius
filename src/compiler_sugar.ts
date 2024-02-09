import { BytecodeSecondOrder, getOperatorTable, loadModule, propagatedLiteralAst, pushBytecode, resolveScope, visitParseNode } from "./compiler"
import { createCallAstFromValueAndPushValue, createMethodCall, insertFunctionDefinition } from "./compiler_functions"
import { Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd, ParseFold, ParseForExpr, ParseWhileExpr, Module, pushSubCompilerState, createScope, TaskContext, CompilerError, AstType, OperatorAst, CompilerFunction, CallAst, RawPointerType, SubscriptAst, IntType, expectType, SetSubscriptAst, ParserFunctionParameter, FunctionType, Binding, StringType, ValueFieldAst, LetAst, BindingAst, createStatements, StringAst, FloatType, DoubleType, FunctionCallContext, Vm, expectAst, NumberAst, Type } from "./defs"
import { Task, TaskDef } from "./tasks"

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

export const expandLoopSugar = (out: BytecodeWriter, node: ParseExpand) => {

  // Because we want to use the expansion state to insert a loop construct
  // around the expansion body we need to evaluate the body first.
  // Visit the body of the expansion node but output to a fresh bytecode
  // array which we will append later using a ParseBytecode.
  // This is in combination with sliceSugar below
  // TODO: use fresh variables

  const bytecode = { code: [], locations: [] }
  // const indexIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('i'))
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  const expansion: typeof out.state.expansion = out.state.expansion = { selectors: [], iteratorListIdentifier, fold: null }
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.expr)

  const getIterator = (i: number) => new ParseMeta(node.token, new ParseSubscript(node.token, 
    iteratorListIdentifier, new ParseNumber(createAnonymousToken(i)), false))

  const iteratorNodes = expansion.selectors.map((s, i) => {
    let lengthNode: ParseNode = getLength(node.token, getIterator(i))
    // TODO: Handle positive end positions!
    if (s.end) lengthNode = new ParseOperator(createAnonymousToken("+"), [lengthNode, s.end])
    let letLengthNode: ParseNode = new ParseLet(node.token, new ParseFreshIden(node.token, new FreshBindingToken('length')), null, lengthNode)
    const letItNode = new ParseLet(node.token, s.indexIdentifier, null, s.start ?? new ParseNumber(createAnonymousToken('0')))
    const incNode = new ParseOpEq(createAnonymousToken("+="), letItNode.name, s.step ?? new ParseNumber(createAnonymousToken('1')))
    const condNode = new ParseOperator(createAnonymousToken("<"), [letItNode.name, letLengthNode.name])
    return { letLengthNode, condNode, letItNode, incNode }
  })

  const list = new ParseList(node.token, expansion.selectors.map(x => new ParseQuote(node.token, x.node)))
  const letIteratorList = new ParseLet(node.token, iteratorListIdentifier, null, list)
  const lets = iteratorNodes.flatMap(x => [x.letLengthNode, x.letItNode])
  const cond = iteratorNodes.slice(1).reduce((acc: ParseNode, x) => new ParseAnd(node.token, [acc, x.condNode]), iteratorNodes[0].condNode)

  let loopBody: ParseNode = new ParseBytecode(node.token, bytecode)
  const result: ParseNode[] = []
  
  if (expansion.fold) {
    lets.push(new ParseLet(node.token, expansion.fold.iden, null, expansion.fold.initial))
    loopBody = new ParseSet(node.token, expansion.fold.iden, loopBody)
    result.push(expansion.fold.iden)
  }
  const whileStmts = new ParseStatements(node.token, [loopBody, ...iteratorNodes.map(x => x.incNode)])
  const loop = new ParseWhile(node.token, cond, whileStmts)
  const metaList = new ParseMeta(node.token, letIteratorList)
  visitParseNode(out, new ParseStatements(node.token, [metaList, ...lets, loop, ...result]))

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
  const index = out.state.expansion.selectors.length
  const indexIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('i'))
  out.state.expansion.selectors.push({ node: node.expr, start: node.start, end: node.end, step: node.step, indexIdentifier })
  const indexNode = new ParseNumber(createAnonymousToken(index))
  const iteratorList = new ParseMeta(node.token, new ParseSubscript(node.token, out.state.expansion.iteratorListIdentifier, indexNode, false))
  const subscriptIterator = new ParseSubscript(node.token, iteratorList, indexIdentifier, false);
  const finalNode = assignValue ? new ParseSet(node.token, subscriptIterator, assignValue) : subscriptIterator
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
  const operatorFunc = new CompilerFunction(operatorName, (location: SourceLocation, a: Ast, b: Ast) => {
    if (b.type instanceof ParameterizedType && b.type.typeConstructor === TupleTypeConstructor) {
      compilerAssert(b.type.args.length === compiledClass.fields.length, `Expected tuple of size ${compiledClass.fields.length}, got ${b.type.args.length}`, { type: b.type })
      const constructorArgs = compiledClass.fields.map((field, i) => {
        const otherField = b.type.typeInfo.fields.find(x => x.name === `_${i+1}`)
        compilerAssert(otherField?.fieldType === field.fieldType, `Expected type of tuple field ${i+1} to be $fieldType got $otherFieldType`, { fieldType: field.fieldType, otherFieldType: otherField?.fieldType })
        return getOperatorTable()[operatorSymbol].func(location, 
          new FieldAst(field.fieldType, location, a, field),
          new FieldAst(otherField.fieldType, location, b, otherField))
      })
      return new ConstructorAst(compiledClass.type, location, constructorArgs)
    }
    compilerAssert(b.type === a.type, "Expected vec or tuple. got $type", { type: b.type })
    const constructorArgs = compiledClass.fields.map(field => 
      getOperatorTable()[operatorSymbol].func(location, 
        new FieldAst(field.fieldType, location, a, field),
        new FieldAst(field.fieldType, location, b, field))
    )
    return new ConstructorAst(compiledClass.type, location, constructorArgs)
  })
  compiledClass.metaobject[operatorName] = operatorFunc
}

export const VecTypeMetaClass = new ExternalFunction('VecType', new Binding("VecType", VoidType), VoidType, (compiledClass: CompiledClass) => {
  insertMetaObjectPairwiseOperator(compiledClass, "add", "+")
  insertMetaObjectPairwiseOperator(compiledClass, "sub", "-")
  insertMetaObjectPairwiseOperator(compiledClass, "mul", "*")
  insertMetaObjectPairwiseOperator(compiledClass, "div", "/")
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
  const decl = createAnonymousParserFunctionDecl("constructor", createAnonymousToken(''), fnParams, constructorBody)
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
}

// Order index is external index in VM 
// TODO: Fix all this
export const externals: {[key:string]: ExternalFunction} = {
  // print:       new ExternalFunction('print',       externalBuiltinBindings.print, VoidType, (...args) => { compilerAssert(false, "Implemented elsewhere") }),
  printf:      new ExternalFunction('printf',      externalBuiltinBindings.printf, VoidType, (...args: unknown[]) => { compilerAssert(false, "Implemented elsewhere") }),
  // // malloc:      new ExternalFunction('malloc',      externalBuiltinBindings.malloc, VoidType, (ast: Ast) => { compilerAssert(false, "Implemented elsewhere") }),
  sizeof:      new ExternalFunction('sizeof',      externalBuiltinBindings.sizeof, VoidType, (ast: Ast) => { compilerAssert(false, "Implemented elsewhere") }),
  // // realloc:     new ExternalFunction('realloc',     externalBuiltinBindings.realloc, VoidType, (ast: Ast) => { compilerAssert(false, "Implemented elsewhere") }),
  // // free:        new ExternalFunction('free',        externalBuiltinBindings.free, VoidType, (ast: Ast) => { compilerAssert(false, "Implemented elsewhere") }),
}

export const print = new CompilerFunction('print', (location: SourceLocation, typeArgs: unknown[], args: Ast[]) => {
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
  formats.set(RawPointerType, '%p')
  formats.set(FloatType, '%f')
  formats.set(DoubleType, '%f')

  args.forEach((arg, i) => {
    if (i !== 0) formatStr += ' '
    if (arg.type === StringType) {
      const binding = new Binding("", StringType)
      stmts.push(new LetAst(VoidType, location, binding, arg))
      const lengthGetter = fieldHelper(binding, 'length')
      const dataGetter = fieldHelper(binding, 'data')
      formatStr += '%.*s'
      printfArgs.push(lengthGetter, dataGetter)
    } else if (formats.has(arg.type)) {
      printfArgs.push(arg)
      formatStr += formats.get(arg.type)
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
  stmts.push(new CallAst(VoidType, location, externals.printf, printfArgs, []))
  return createStatements(location, stmts)
})



export const unsafe_subscript = new CompilerFunction('unsafe_subscript', (location: SourceLocation, typeArgs: unknown[], args: Ast[]) => {
  const [left, right] = args
  propagatedLiteralAst(right)
  compilerAssert(right && right.type === IntType, "Expected int type", { right })
  compilerAssert(left && left.type === RawPointerType, "Expected rawptr", { left })
  const type = expectType(typeArgs[0])
  return new SubscriptAst(type, location, left, propagatedLiteralAst(right))
})
export const unsafe_set_subscript = new CompilerFunction('unsafe_set_subscript', (location: SourceLocation, typeArgs: unknown[], args: Ast[]) => {
  const [left, right, value] = args
  propagatedLiteralAst(right)
  compilerAssert(right && right.type === IntType, "Expected int type", { right })
  compilerAssert(left && left.type === RawPointerType, "Expected rawptr", { left })
  compilerAssert(value, "Expected value", { value })
  return new SetSubscriptAst(VoidType, location, left, propagatedLiteralAst(right), propagatedLiteralAst(value))
})
export const operator_bitshift_left = new CompilerFunction('operator_bitshift_left', (location: SourceLocation, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return new OperatorAst(IntType, location, "<<", [a, b])
})
export const operator_bitshift_right = new CompilerFunction('operator_bitshift_right', (location: SourceLocation, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return new OperatorAst(IntType, location, ">>", [a, b])
})
export const operator_bitwise_and = new CompilerFunction('operator_bitwise_and', (location: SourceLocation, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return new OperatorAst(IntType, location, "&", [a, b])
})
export const operator_bitwise_or = new CompilerFunction('operator_bitwise_or', (location: SourceLocation, typeArgs: unknown[], args: Ast[]) => {
  const [a, b] = args
  propagatedLiteralAst(a)
  propagatedLiteralAst(b)
  compilerAssert(a && a.type === IntType, "Expected int type", { a })
  compilerAssert(b && b.type === IntType, "Expected int type", { b })
  return new OperatorAst(IntType, location, "|", [a, b])
})

const add_external_library = new ExternalFunction("add_external_library", new Binding("", FunctionType), VoidType, (ctx: FunctionCallContext, library: unknown) => {
  compilerAssert(typeof library == 'string', "Expected string")
  ctx.compilerState.globalCompiler.externalCompilerOptions.libraries.push(library)
})

export const createCompilerModuleTask = (ctx: TaskContext): Task<Module, CompilerError> => {
  const moduleScope = createScope({}, undefined)
  Object.assign(moduleScope, { 
    unsafe_subscript, unsafe_set_subscript, operator_bitshift_left, operator_bitshift_right,
    operator_bitwise_and, operator_bitwise_or, rawptr: RawPointerType, add_external_library })
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `compiler module`, lexicalParent: undefined, scope: moduleScope })
  const module = new Module('compiler', subCompilerState, null!)
  return Task.of(module)
}

export const preloadModuleText = () => {
  return `
import compiler

fn iterate!(f, T)(list: List!T) @inline @method:
  i := 0
  while i < list.length:
    f(list[i])
    i += 1

# This is needed for expansion operator
fn length!(T)(list: List!T) @inline @method:
  list.length

fn malloc(size: int) -> compiler.rawptr @external
fn realloc(ptr: compiler.rawptr, new_size: int) -> compiler.rawptr @external
fn free(ptr: compiler.rawptr) @external
fn sizeof!(T)() @external

fn sin(t: double) -> double @external
fn cos(t: double) -> double @external
fn sinf(t: float) -> float:
  float(sin(double(t)))
fn cosf(t: float) -> float:
  float(cos(double(t)))

fn min!(T)(a: T, b: T) -> T:
  ifx a <= b: a else: b
fn max!(T)(a: T, b: T) -> T:
  ifx a >= b: a else: b

`
}