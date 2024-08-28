import { BytecodeSecondOrder, callFunctionFromValueTask, compileFunctionPrototype, createParameterizedExternalType, getOperatorTable, loadModule, propagateLiteralType, propagatedLiteralAst, pushBytecode, resolveScope, unknownToAst, visitParseNode } from "./compiler"
import { compileExportedFunctionTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall, insertFunctionDefinition } from "./compiler_functions"
import { concat } from "./compiler_iterator"
import { Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd, ParseFold, ParseForExpr, ParseWhileExpr, Module, pushSubCompilerState, createScope, TaskContext, CompilerError, AstType, OperatorAst, CompilerFunction, CallAst, RawPointerType, SubscriptAst, IntType, expectType, SetSubscriptAst, ParserFunctionParameter, FunctionType, Binding, StringType, ValueFieldAst, LetAst, BindingAst, createStatements, StringAst, FloatType, DoubleType, CompilerFunctionCallContext, Vm, expectAst, NumberAst, Type, UserCallAst, hashValues, NeverType, IfAst, BoolType, VoidAst, LoopObject, CompileTimeObjectType, u64Type, FunctionDefinition, ParserFunctionDecl, StatementsAst, isTypeScalar, IntLiteralType, FloatLiteralType, isAst, isType, isTypeCheckError, InterleaveAst, ContinueInterAst, CompTimeObjAst, ParseEvalFunc, SetAst, DefaultConsAst, WhileAst, BoolAst, isArray, ExpansionSelector, ParseNote, ExpansionCompilerState, ParseBoolean, ParseOr, ParseBreak, isTypeInteger, OptionType, OptionTypeConstructor, EnumValueAst, ParseIs, filterNotNull, ParseLetConst, ParseCast, VariantCastAst, ExternalTypeConstructor, NoneTypeConstructor, SomeTypeConstructor, typesEqual, GlobalCompilerState } from "./defs"
import { Event, Task, TaskDef, isTask } from "./tasks"

const rangeLoop = (token: Token, iden: ParseIdentifier | ParseFreshIden, start: ParseNode, end: ParseNode, body: ParseNode) => {
  const letNode = new ParseLet(token, iden, null, start)
  const loopBody = new ParseStatements(token, [body, new ParseOpEq(createAnonymousToken("+="), iden, new ParseNumber(createAnonymousToken('1')))])
  const loop = new ParseWhile(token, new ParseOperator(createAnonymousToken('<'), [iden, end]), loopBody)
  return [letNode, loop]
}

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
    propagatedLiteralAst(arg)
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
      const binding = new Binding("", StringType)
      const str = new IfAst(StringType, location, arg, new StringAst(StringType, location, 'true'), new StringAst(StringType, location, 'false'))
      stmts.push(new LetAst(VoidType, location, binding, str))
      const lengthGetter = fieldHelper(binding, 'length')
      const dataGetter = fieldHelper(binding, 'data')
      formatStr += '%.*s'
      printfArgs.push(lengthGetter, dataGetter)
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
    } else {
      compilerAssert(false, "Cannot print value of type $type", { type: arg.type })
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
  compilerAssert(a && isTypeInteger(a.type), "Expected int type", { a })
  compilerAssert(b && isTypeInteger(b.type), "Expected int type", { b })
  compilerAssert(a.type === b.type, "Expected same type", { a, b })
  return Task.of(new OperatorAst(a.type, ctx.location, "mod", [a, b]))
})
export const static_length = new ExternalFunction('static_length', VoidType, (ctx, args: Ast[]) => {
  let type: unknown = args[0]
  if (type instanceof Binding) type = type.type
  compilerAssert(isType(type), "Expected type or binding got $type", { type })
  const static_length = type.typeInfo.metaobject['static_length']
  compilerAssert(static_length, "Expected 'static_length' metafield for $type", { type })
  return static_length
})

export const createDefaultFromType = new ExternalFunction('createDefaultFromType', VoidType, (ctx, values) => {
  let [type] = values
  compilerAssert(isType(type), "Expected type", { type })
  return new DefaultConsAst(type, ctx.location)
})
export const maxOfType = new ExternalFunction('maxOfType', VoidType, (ctx, values) => {
  let [type] = values
  compilerAssert(isType(type), "Expected type", { type })
  compilerAssert(type.typeInfo.metaobject['max'], "Type does not have a max value", { type })
  return new NumberAst(type, ctx.location, type.typeInfo.metaobject['max'] as number)
})
export const minOfType = new ExternalFunction('maxOfType', VoidType, (ctx, values) => {
  let [type] = values
  compilerAssert(isType(type), "Expected type", { type })
  compilerAssert(type.typeInfo.metaobject['min'], "Type does not have a min value", { type })
  return new NumberAst(type, ctx.location, type.typeInfo.metaobject['min'] as number)
})

export const typeOf = new ExternalFunction('typeOf', VoidType, (ctx, values) => {
  let [value] = values
  compilerAssert(isAst(value), "Expected ast", { value })
  return value.type
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
    .chainFn<Ast, CompilerError>((task, v) => {
      compilerAssert(false, "Expected compile to fail but it didn't", { assertCompileError: true })
    })
    .chainRejected<CompilerError>((err) => {
      if ((err.info as any).assertCompileError) return Task.rejected(err)
      if (err.message.includes(errorMsg)) return Task.of(new VoidAst(VoidType, ctx.location))
      return Task.rejected(err)
    })
  )
  
})

export const initializer_function = new CompilerFunction("initializer_function", (ctx, typeArgs, args) => {
  return Task.of(new UserCallAst(VoidType, ctx.location, ctx.compilerState.globalCompiler.initializerFunctionBinding, []))
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

export const some = new CompilerFunction("Some", (ctx, typeArgs, args) => {
  const t = typeArgs[0]
  propagatedLiteralAst(args[0])
  compilerAssert(isType(t), "Expected type", { t })
  return (
    TaskDef(callFunctionFromValueTask, ctx.compilerState.vm, OptionTypeConstructor, [t], [])
    .chainFn((task, value) => { 
      const type = ctx.compilerState.vm.stack.pop()
      compilerAssert(isType(type), "Expected type", { type })
      const someType = type.typeInfo.metaobject.Some
      compilerAssert(isType(someType), "Expected type", { type })
      compilerAssert(args.length === 1, "Expected one argument", { args })
      compilerAssert(args[0].type === t, "Expected type to match", { type: args[0].type, t })
      return Task.of(new EnumValueAst(type, ctx.location, someType, [args[0]]))
    })
  )
})
export const none = new CompilerFunction("None", (ctx, typeArgs, args) => {
  const t = typeArgs[0]
  compilerAssert(isType(t), "Expected type", { t })
  return (
    TaskDef(callFunctionFromValueTask, ctx.compilerState.vm, OptionTypeConstructor, [t], [])
    .chainFn((task, value) => { 
      const type = ctx.compilerState.vm.stack.pop()
      compilerAssert(isType(type), "Expected type", { type })
      const noneType = type.typeInfo.metaobject.None
      compilerAssert(isType(noneType), "Expected type", { noneType, metaobject: type.typeInfo.metaobject })
      return Task.of(new EnumValueAst(type, ctx.location, noneType, []))
    })
  )
})

const getEnumOf = (compiler: GlobalCompilerState, type: Type) => {
  compilerAssert(type.typeInfo.metaobject.isEnumVariant, "Expected enum variant", { type })
  let args: unknown[] = []
  if (type instanceof ParameterizedType) args = type.args
  return createParameterizedExternalType(compiler, type.typeInfo.metaobject.enumConstructorVariantOf as ExternalTypeConstructor, args)
}

const getTypeIndex = new CompilerFunction("getTypeIndex", (ctx, typeArgs, args) => {
  let [variantType] = typeArgs
  const [value] = args
  const type = value.type
  compilerAssert(isAst(value), "Expected ast", { value })
  compilerAssert(value.type.typeInfo.metaobject.isEnum, "Expected value of type enum", { value })
  if (typeof variantType === 'string') variantType = (value.type.typeInfo.metaobject.variants as ExternalTypeConstructor[]).find(x => x.typeName === variantType)
  if (variantType instanceof ExternalTypeConstructor) {
    variantType = (type.typeInfo.metaobject.variants as ParameterizedType[]).find(x => x.typeConstructor === variantType)
  }
  compilerAssert(isType(variantType), "Expected type", { variantType })
  compilerAssert(variantType.typeInfo.metaobject.isEnumVariant, "Expected enum variant", { type })
  return (
    getEnumOf(ctx.compilerState.globalCompiler, variantType)
    .chainFn((task, enumVariantOf) => {
      compilerAssert(enumVariantOf === type, "Expected type to match $type $enumVariantOf", { type, enumVariantOf })
      compilerAssert(typeof variantType.typeInfo.metaobject.enumVariantIndex === 'number', "Expected number", { variantType })
      return Task.of(new NumberAst(IntType, ctx.location, variantType.typeInfo.metaobject.enumVariantIndex))
    })
  )
})

const unsafeEnumCast = new CompilerFunction("unsafeEnumCast", (ctx, typeArgs, args) => {
  const [value] = args
  let [variantType] = typeArgs
  const type = value.type
  compilerAssert(isType(type), "Expected type", { type })
  if (variantType instanceof ExternalTypeConstructor) {
    variantType = (type.typeInfo.metaobject.variants as ParameterizedType[]).find(x => x.typeConstructor === variantType)
  }
  compilerAssert(isType(variantType), "Expected type", { variantType })

  return (
    getEnumOf(ctx.compilerState.globalCompiler, variantType)
    .chainFn((task, enumVariantOf) => {
      compilerAssert(type === enumVariantOf, "Expected type to match", { type, enumVariantOf })
      compilerAssert(typeof variantType.typeInfo.metaobject.enumVariantIndex === 'number', "Expected number", { variantType })
      return Task.of(new VariantCastAst(variantType, ctx.location, type, value))
    })
  )
})

export const smartCastSugar = (out: BytecodeWriter, node: ParseIf) => {
  compilerAssert(node.condition instanceof ParseIs, "Expected is", { node })
  const testType = node.condition.type
  compilerAssert(node.condition.expr instanceof ParseIdentifier, "Expected identifier", { node })
  const name = node.condition.expr.token
  const condExpr = new ParseQuote(node.token, node.condition.expr)

  const testTypeIden = new ParseFreshIden(node.token, new FreshBindingToken('testtype'))
  const condExprIden = new ParseFreshIden(node.token, new FreshBindingToken('condexpr'))
  const indexIden = new ParseFreshIden(node.token, new FreshBindingToken('index'))

  const letTestType = new ParseLetConst(node.token, testTypeIden, testType)
  const letCondExpr = new ParseLetConst(node.token, condExprIden, condExpr)
  
  const call = new ParseCall(node.token, new ParseValue(node.token, getTypeIndex), [condExprIden], [testTypeIden])
  const letIndex = new ParseLetConst(node.token, indexIden, new ParseQuote(node.token, call))
  
  const cond = new ParseOperator(createAnonymousToken('=='), [indexIden, new ParseNumber(createAnonymousToken('1'))])
  const cast = new ParseCall(node.token, new ParseValue(node.token, unsafeEnumCast), [new ParseIdentifier(name)], [testType])
  const letCast = new ParseLet(node.token, new ParseIdentifier(name), null, cast)
  const trueBody = new ParseBlock(node.token, null, null, new ParseStatements(node.token, [letCast, node.trueBody]))

  visitParseNode(out, letTestType)
  pushBytecode(out, node.token, { type: 'pop' })
  visitParseNode(out, letCondExpr)
  pushBytecode(out, node.token, { type: 'pop' })
  visitParseNode(out, letIndex)
  pushBytecode(out, node.token, { type: 'pop' })
  if (node.falseBody) visitParseNode(out, node.falseBody)
  visitParseNode(out, trueBody)
  visitParseNode(out, cond)
  pushBytecode(out, node.token, { type: "ifast", f: !!node.falseBody, e: node.isExpr });
}

export const metaLetIn = (token: Token, node: ParseNode, f: (iden: ParseFreshIden) => ParseNode[]) => {
  const iden = new ParseFreshIden(token, new FreshBindingToken('let_in'))
  const let_ = new ParseLetConst(token, iden, node)
  return new ParseStatements(token, [let_, ...f(iden)])
}

export const createCompilerModuleTask = (ctx: TaskContext): Task<Module, CompilerError> => {
  const moduleScope = createScope({}, undefined)
  Object.assign(moduleScope, { 
    unsafe_subscript, unsafe_set_subscript, operator_bitshift_left, operator_bitshift_right,
    operator_bitwise_and, operator_bitwise_or, rawptr: RawPointerType, add_external_library, add_macos_framework, assert, never: NeverType,
    get_current_loop, ctobj: CompileTimeObjectType, operator_mod, overloaded, static_length, assert_compile_error, initializer_function, add_export,
    concat, Option: OptionTypeConstructor, Some: SomeTypeConstructor, None: NoneTypeConstructor, some, none })
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