import { createDefaultConstructorAst, createParameter, generateConstructor, generateDestructor, generateMoveFunction } from "../borrow/codegen_ast"
import { BytecodeSecondOrder, callFunctionFromValueTask, compileFunctionPrototype, getOperatorTable, pushBytecode, unknownToAst, visitParseNode } from "./compiler_vm"
import { compileAndExecuteFunctionHeaderTask, compileExportedFunctionTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall, FunctionCallArg, functionTemplateTypeCheckAndCompileTask, insertFunctionDefinition } from "./compiler_functions"
import { concat, generator } from "./compiler_iterator"
import { OptionTypeConstructor, createParameterizedExternalType, hashValues, isTypeInteger, isTypeScalar, propagateLiteralType, propagatedLiteralAst } from "./compiler_types"
import { Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd, ParseFold, ParseForExpr, ParseWhileExpr, Module, pushSubCompilerState, createScope, TaskContext, CompilerError, AstType, OperatorAst, CompilerFunction, CallAst, RawPointerType, SubscriptAst, IntType, expectType, SetSubscriptAst, ParserFunctionParameter, FunctionType, Binding, StringType, ValueFieldAst, LetAst, BindingAst, createStatements, StringAst, FloatType, DoubleType, CompilerFunctionCallContext, Vm, expectAst, NumberAst, Type, UserCallAst, NeverType, IfAst, BoolType, VoidAst, LoopObject, CompileTimeObjectType, u64Type, FunctionDefinition, ParserFunctionDecl, StatementsAst, IntLiteralType, FloatLiteralType, isAst, isType, isTypeCheckError, InterleaveAst, ContinueInterAst, CompTimeObjAst, ParseEvalFunc, SetAst, DefaultConsAst, WhileAst, BoolAst, isArray, ExpansionSelector, ParseNote, ExpansionCompilerState, ParseBoolean, ParseOr, ParseBreak, ParseIs, filterNotNull, ParseLetConst, ParseCast, VariantCastAst, ExternalTypeConstructor, GlobalCompilerState, ParseOrElse, ParseField, ParseQuestion, ParseBreakOpt, LabelBlock, BlockAst, ParseMatch, ParseExtract, ParseMatchCase, ParseTuple, ParseString, Tuple, ParseNot, EnumVariantAst, ParseIfMulti, ParseGuard, ParseBlockNoScope, Capability, TypeCheckResult, CompiledFunction, LetType, YieldAst, textColors, MutSigilAst, TypeField, ParseMutSigil, ParseSymbol, FunctionParameter } from "./defs"
import { Task, TaskDef } from "./tasks"

const insertMetaObjectPairwiseOperator = (compiledClass: CompiledClass, operatorName: string, operatorSymbol: string) => {
  const operatorFunc = new CompilerFunction(operatorName, (ctx, typeArgs, args) => {
    const [a, b] = args
    // TODO: Try and move this to be more automatic
    if (a.type === IntLiteralType || a.type === FloatLiteralType) propagateLiteralType(compiledClass.fields[0].fieldType, a)
    if (b.type === IntLiteralType || b.type === FloatLiteralType) propagateLiteralType(compiledClass.fields[0].fieldType, b)
    const bindingAstA = new BindingAst(a.type, ctx.location, new Binding("", a.type))
    const bindingAstB = new BindingAst(b.type, ctx.location, new Binding("", b.type))
    const stmts: Ast[] = [
      new LetAst(VoidType, ctx.location, bindingAstA.binding, a, LetType.Let),
      new LetAst(VoidType, ctx.location, bindingAstB.binding, b, LetType.Let)]

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
      new LetAst(VoidType, ctx.location, bindingAst.binding, value, LetType.Let),
      new ValueFieldAst(field.fieldType, ctx.location, bindingAst, [field]),
    ])
  })
  compiledClass.metaobject["static_subscript"] = operatorFunc
  compiledClass.metaobject["static_length"] = compiledClass.fields.length

})

export const defaultMetaFunction = (subCompilerState: SubCompilerState, compiledClass: CompiledClass, definitionScope: Scope, templateScope: Scope): Task<unknown, CompilerError> => {
  const iterate = templateScope['__iterate']
  compilerAssert(!iterate || iterate instanceof Closure)
  const subscript = templateScope['__subscript']
  compilerAssert(!subscript || subscript instanceof Closure)
  const subscript_inout = templateScope['__subscript_inout']
  compilerAssert(!subscript_inout || subscript_inout instanceof Closure)
  const subscript_sink = templateScope['__subscript_sink']
  compilerAssert(!subscript_sink || subscript_sink instanceof Closure)
  const subscript_set = templateScope['__subscript_set']
  compilerAssert(!subscript_set || subscript_set instanceof Closure)
  const destructor = templateScope['__destructor']
  compilerAssert(!destructor || destructor instanceof Closure)
  const moveInit = templateScope['__move_init']
  compilerAssert(!moveInit || moveInit instanceof Closure)
  const moveAssign = templateScope['__move_assign']
  compilerAssert(!moveAssign || moveAssign instanceof Closure)
  const copy = templateScope['__copy']
  compilerAssert(!copy || copy instanceof Closure)
  const length = templateScope['__length']
  compilerAssert(!length || length instanceof Closure)
  const print = templateScope['__print']
  compilerAssert(!print || print instanceof Closure)

  // if (compiledClass.classDefinition.keywords.includes('struct'))
  compiledClass.type.typeInfo.isReferenceType = false // Always false for now

  const fnParams: ParserFunctionParameter[] = compiledClass.fields.map(x => 
    ({ name: new ParseIdentifier(createAnonymousToken(x.name)), storage: null,
    type: new ParseValue(createAnonymousToken(''), x.fieldType), capability: Capability.Sink}) satisfies ParserFunctionParameter)
  const constructorBody = new ParseConstructor(
    createAnonymousToken(''), 
    new ParseValue(createAnonymousToken(''), compiledClass.type), 
    compiledClass.fields.map(x => new ParseMutSigil(createAnonymousToken(''), new ParseIdentifier(createAnonymousToken(x.name)))))
  const decl = createAnonymousParserFunctionDecl(`${compiledClass.debugName} constructor`, createAnonymousToken(''), fnParams, constructorBody)
  const funcDef = insertFunctionDefinition(subCompilerState.globalCompiler, decl)
  const constructor = new Closure(funcDef, definitionScope, subCompilerState.lexicalParent!)

  Object.assign(compiledClass.metaobject, { 
    iterate, subscript, subscript_inout, subscript_sink, subscript_set, constructor, destructor, moveInit,
    moveAssign, copy, length, print
  })

  return (
    compileCustomDestructor(subCompilerState, compiledClass.debugName, destructor as Closure | undefined, compiledClass)
    .chainFn(() => compileCustomMove(subCompilerState, compiledClass.debugName, moveInit as Closure | undefined, Capability.Set, compiledClass).chainFn((task, compiledFn) => {
      if (compiledFn) compiledClass.metaobject.moveInitBinding = compiledFn.binding
      return Task.success()
    }))
    .chainFn(() => compileCustomMove(subCompilerState, compiledClass.debugName, moveAssign as Closure | undefined, Capability.Inout, compiledClass).chainFn((task, compiledFn) => {
      if (compiledFn) compiledClass.metaobject.moveAssignBinding = compiledFn.binding
      return Task.success()
    }))
    .chainFn(() => compileCustomCopy(subCompilerState, compiledClass.debugName, copy as Closure | undefined, compiledClass).chainFn((task, compiledFn) => {
      if (compiledFn) compiledClass.metaobject.copyConstructorBinding = compiledFn.binding
      return Task.success()
    }))
    .chainFn(() => compileCustomPrint(subCompilerState, compiledClass.debugName, print as Closure | undefined, compiledClass).chainFn((task, compiledFn) => {
      if (compiledFn) compiledClass.metaobject.printBinding = compiledFn.binding
      return Task.success()
    }))
    .chainFn((task, constructor) => {
      // Order is important, so destructor is added to compiled functions first.
      generateTypeMethods(subCompilerState.globalCompiler, compiledClass.type)
      return Task.success()
    })
  )
  
}

const compileCustomDestructor = (subCompilerState: SubCompilerState, structName: string, destructor: Closure | undefined, compiledClass: CompiledClass) => {
  if (!destructor) return Task.success()

  // We have to compile the destructor here to get the binding, and set
  // it in the metaobject because the destructor is may be invoked during the
  // post processing passes of the compiler, which does not have access
  // to the compilation state

  const ctx: CompilerFunctionCallContext = { location: SourceLocation.anon, compilerState: subCompilerState, resultAst: undefined, typeCheckResult: undefined }
  const value = new BindingAst(compiledClass.type, ctx.location, new Binding("", compiledClass.type)) // Fake it

  const typeCheckResult: TypeCheckResult = { func: destructor.func, concreteTypes: [], substitutions: {}, returnType: undefined!, sortedArgs: [], checkFailed: false }
  const call: FunctionCallArg = { location: SourceLocation.anon, func: destructor.func, typeArgs: [], args: [value], parentScope: destructor.scope, lexicalParent: destructor.lexicalParent, result: typeCheckResult }

  return (
    TaskDef(compileAndExecuteFunctionHeaderTask, call)
    .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
    .chainFn((task, compiledFunction) => {
      compiledFunction.isDestructor = true
      compilerAssert(compiledFunction.parameters.length === 1, "Expected 1 parameter in destructor", { c: compiledFunction })
      compilerAssert(compiledFunction.parameters[0].type === compiledClass.type, "Expected type of destructor's first argument to be $type got $otherType", { type: compiledClass.type, otherType: compiledFunction.parameters[0].type })
      compilerAssert(compiledFunction.parameters[0].capability === Capability.Sink, "Expected sink capability", { c: compiledFunction })
      compilerAssert(compiledFunction.returnType === VoidType, "Expected void return type", { c: compiledFunction })
      compiledClass.metaobject.destructorBinding = compiledFunction.binding
      return Task.success()
    })
  )
}

const compileCustomMove = (subCompilerState: SubCompilerState, structName: string, movefn: Closure | undefined, sourceCapability: Capability, compiledClass: CompiledClass): Task<CompiledFunction | undefined, CompilerError> => {
  if (!movefn) return Task.of(undefined)

  const ctx: CompilerFunctionCallContext = { location: SourceLocation.anon, compilerState: subCompilerState, resultAst: undefined, typeCheckResult: undefined }
  const destValue = new BindingAst(compiledClass.type, ctx.location, new Binding("", compiledClass.type)) // Fake it
  const sourceValue = new BindingAst(compiledClass.type, ctx.location, new Binding("", compiledClass.type)) // Fake it

  const typeCheckResult: TypeCheckResult = { func: movefn.func, concreteTypes: [], substitutions: {}, returnType: undefined!, sortedArgs: [], checkFailed: false }
  const call: FunctionCallArg = { location: SourceLocation.anon, func: movefn.func, typeArgs: [], args: [destValue, sourceValue], parentScope: movefn.scope, lexicalParent: movefn.lexicalParent, result: typeCheckResult }

  return (
    TaskDef(compileAndExecuteFunctionHeaderTask, call)
    .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
    .chainFn((task, compiledFunction) => {
      compilerAssert(compiledFunction.parameters.length === 2, "Expected 2 parameters in move function", { c: compiledFunction })
      compilerAssert(compiledFunction.parameters[0].type === compiledClass.type, "Expected type of move function's first argument to be $type got $otherType", { type: compiledClass.type, otherType: compiledFunction.parameters[0].type })
      compilerAssert(compiledFunction.parameters[0].capability === sourceCapability, `Expected ${sourceCapability} capability`, { c: compiledFunction })
      compilerAssert(compiledFunction.parameters[1].type === compiledClass.type, "Expected type of move function's second argument to be $type got $otherType", { type: compiledClass.type, otherType: compiledFunction.parameters[1].type })
      compilerAssert(compiledFunction.parameters[1].capability === Capability.Sink, "Expected sink capability", { c: compiledFunction })
      compilerAssert(compiledFunction.returnType === VoidType, "Expected void return type", { c: compiledFunction })
      return Task.of(compiledFunction)
    })
  )
}

const compileCustomCopy = (subCompilerState: SubCompilerState, structName: string, copyfn: Closure | undefined, compiledClass: CompiledClass): Task<CompiledFunction | undefined, CompilerError> => {
  if (!copyfn) return Task.of(undefined)

  const ctx: CompilerFunctionCallContext = { location: SourceLocation.anon, compilerState: subCompilerState, resultAst: undefined, typeCheckResult: undefined }
  const destValue = new BindingAst(compiledClass.type, ctx.location, new Binding("", compiledClass.type)) // Fake it
  const sourceValue = new BindingAst(compiledClass.type, ctx.location, new Binding("", compiledClass.type)) // Fake it

  const typeCheckResult: TypeCheckResult = { func: copyfn.func, concreteTypes: [], substitutions: {}, returnType: undefined!, sortedArgs: [], checkFailed: false }
  const call: FunctionCallArg = { location: SourceLocation.anon, func: copyfn.func, typeArgs: [], args: [destValue, sourceValue], parentScope: copyfn.scope, lexicalParent: copyfn.lexicalParent, result: typeCheckResult }

  return (
    TaskDef(compileAndExecuteFunctionHeaderTask, call)
    .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
    .chainFn((task, compiledFunction) => {
      compilerAssert(compiledFunction.parameters.length === 2, "Expected 2 parameters in copy function", { c: compiledFunction })
      compilerAssert(compiledFunction.parameters[0].type === compiledClass.type, "Expected type of copy function's first argument to be $type got $otherType", { type: compiledClass.type, otherType: compiledFunction.parameters[0].type })
      compilerAssert(compiledFunction.parameters[0].capability === Capability.Set, "Expected sink capability", { c: compiledFunction })
      compilerAssert(compiledFunction.parameters[1].type === compiledClass.type, "Expected type of copy function's first argument to be $type got $otherType", { type: compiledClass.type, otherType: compiledFunction.parameters[0].type })
      compilerAssert(compiledFunction.parameters[1].capability === Capability.Let, "Expected let capability", { c: compiledFunction })
      compilerAssert(compiledFunction.returnType === VoidType, "Expected return type", { c: compiledFunction })
      return Task.of(compiledFunction)
    })
  )
}

const compileCustomPrint = (subCompilerState: SubCompilerState, structName: string, printfn: Closure | undefined, compiledClass: CompiledClass): Task<CompiledFunction | undefined, CompilerError> => {
  if (!printfn) return Task.of(undefined)

  const ctx: CompilerFunctionCallContext = { location: SourceLocation.anon, compilerState: subCompilerState, resultAst: undefined, typeCheckResult: undefined }
  const value = new BindingAst(compiledClass.type, ctx.location, new Binding("", compiledClass.type)) // Fake it

  const typeCheckResult: TypeCheckResult = { func: printfn.func, concreteTypes: [], substitutions: {}, returnType: undefined!, sortedArgs: [], checkFailed: false }
  const call: FunctionCallArg = { location: SourceLocation.anon, func: printfn.func, typeArgs: [], args: [value], parentScope: printfn.scope, lexicalParent: printfn.lexicalParent, result: typeCheckResult }

  return (
    TaskDef(compileAndExecuteFunctionHeaderTask, call)
    .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
    .chainFn((task, compiledFunction) => {
      compilerAssert(compiledFunction.parameters.length === 1, "Expected 1 parameter in print function", { c: compiledFunction })
      compilerAssert(compiledFunction.parameters[0].type === compiledClass.type, "Expected type of print function's first argument to be $type got $otherType", { type: compiledClass.type, otherType: compiledFunction.parameters[0].type })
      compilerAssert(compiledFunction.parameters[0].capability === Capability.Let, "Expected let capability", { c: compiledFunction })
      compilerAssert(compiledFunction.returnType === VoidType, "Expected return type", { c: compiledFunction })
      return Task.of(compiledFunction)
    })
  )
}

export const generateTypeMethods = (globalCompiler: GlobalCompilerState, type: Type) => {

  const name = type.shortName
  const typeInfo = type.typeInfo

  if (!typeInfo.metaobject.constructorBinding) {
    const binding = typeInfo.metaobject.constructorBinding = new Binding(`constructor${name}`, VoidType)
    const constructor = generateConstructor(name, type, binding)
    globalCompiler.compiledFunctions.set(constructor.binding, constructor)
  }

  if (!typeInfo.metaobject.destructorBinding) {
    const binding = typeInfo.metaobject.destructorBinding = new Binding(`destructor${name}`, VoidType)
    const destructor = generateDestructor(name, type, binding)
    globalCompiler.compiledFunctions.set(destructor.binding, destructor)
  }

  if (!typeInfo.metaobject.copyConstructorBinding) {
    const binding = typeInfo.metaobject.copyConstructorBinding = new Binding(`copyConstructor${name}`, VoidType)
    const copyConstructor = generateMoveFunction(type, `copy${name}`, Capability.Set, Capability.Let, binding as any);
    globalCompiler.compiledFunctions.set(copyConstructor.binding, copyConstructor)
  }

  if (!typeInfo.metaobject.moveInitBinding) {
    const binding = typeInfo.metaobject.moveInitBinding = new Binding(`moveInit${name}`, VoidType)
    const moveInit = generateMoveFunction(type, `moveInit${name}`, Capability.Set, Capability.Sink, binding);
    globalCompiler.compiledFunctions.set(moveInit.binding, moveInit)
  }

  if (!typeInfo.metaobject.moveAssignBinding) {
    const binding = typeInfo.metaobject.moveAssignBinding = new Binding(`moveAssign${name}`, VoidType)
    const moveAssign = generateMoveFunction(type, `moveAssign${name}`, Capability.Inout, Capability.Sink, binding);
    globalCompiler.compiledFunctions.set(moveAssign.binding, moveAssign)
  }

  if (!typeInfo.metaobject.printBinding) {
    const binding = typeInfo.metaobject.printBinding = new Binding(`print${name}`, VoidType)
    const print = generatePrintFunction(type, binding);
    globalCompiler.compiledFunctions.set(print.binding, print)
  }
}

export const generatePrintFunction = (type: Type, binding: Binding) => {
  const funcParams: FunctionParameter[] = [];
  const argBindings: Binding[] = [];
  const concreteTypes: Type[] = [];

  const paramBinding = new Binding('param', type);
  argBindings.push(paramBinding);
  funcParams.push(createParameter(paramBinding, Capability.Let));
  concreteTypes.push(type);

  const location = SourceLocation.anon

  const rawstr = (str: string) => new StringAst(RawPointerType, location, str)
  const printf = (...args: any) => new UserCallAst(VoidType, location, externalBuiltinBindings.printf, args)

  const fieldHelper = (binding: Binding, name: string) => {
    const field = binding.type.typeInfo.fields.find(x => x.name === name)!
    return new ValueFieldAst(field.fieldType, location, new BindingAst(binding.type, location, binding), [field])
  }

  const printStringAst = (format: string, ast: Ast) => {
    const let_ = new LetAst(VoidType, location, new Binding("", StringType), ast, LetType.Let)
    const lengthGetter = fieldHelper(let_.binding, 'length')
    const dataGetter = fieldHelper(let_.binding, 'data')
    return createStatements(location, [let_, printf(rawstr(format), lengthGetter, dataGetter)])
  }

  const bindingAst = new BindingAst(type, SourceLocation.anon, paramBinding)

  const constructorBody = (() => {
    if (type === BoolType) {
      const bool = new IfAst(StringType, location, bindingAst, new StringAst(StringType, location, "true"), new StringAst(StringType, location, "false"))
      return printStringAst(textColors.yellow("%.*s"), bool)
    } else if (type === StringType) {
      return printStringAst(textColors.yellow("\"%.*s\""), bindingAst)
    } else if (type.typeInfo.metaobject.isTuple) {
      const stmts = []
      stmts.push(printf(rawstr(`(`)))
      type.typeInfo.fields.forEach((field, j) => {
        if (j !== 0) stmts.push(printf(rawstr(", ")))
        const fieldAst = new ValueFieldAst(field.fieldType, location, bindingAst, [field])
        stmts.push(generateInlinePrintArg(fieldAst))
      })
      stmts.push(printf(rawstr(`)`)))
      return createStatements(location, stmts)
    }
    return generateStructPrintStatements(bindingAst)
  })()
  const compiledFunc = new CompiledFunction(binding, { debugName: binding.name } as any, VoidType, concreteTypes, constructorBody, argBindings, funcParams, [], 0);
  return compiledFunc
}

export const externalBuiltinBindings: {[key:string]: Binding} = {
  print: new Binding('print', FunctionType),
  printInt: new Binding('printInt', FunctionType),
  printFloat: new Binding('printFloat', FunctionType),
  printf: new Binding('printf', FunctionType),
  malloc: new Binding('malloc', FunctionType),
  realloc: new Binding('realloc', FunctionType),
  free: new Binding('free', FunctionType),
  sizeof: new Binding('sizeof', FunctionType),
  exit: new Binding('exit', FunctionType),
  copy: new Binding('copy', FunctionType),
  initializer: new Binding('initializer', VoidType),
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
  const left = new LetAst(VoidType, location, new Binding("", op.args[0].type), op.args[0], LetType.Let)
  const right = new LetAst(VoidType, location, new Binding("", op.args[1].type), op.args[1], LetType.Let)
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

const generateStructPrintStatements = (bindingArg: BindingAst) => {
  const location = bindingArg.location

  const rawstr = (str: string) => new StringAst(RawPointerType, location, str)
  const printf = (...args: any) => new UserCallAst(VoidType, location, externalBuiltinBindings.printf, args)

  const stmts = []
  stmts.push(printf(rawstr(`${textColors.green(bindingArg.type.shortName)}(`)))
  bindingArg.type.typeInfo.fields.forEach((field, j) => {
    
    if (j !== 0) stmts.push(printf(rawstr(", ")))
    stmts.push(printf(rawstr(`${field.name}=`)))
    const fieldAst = new ValueFieldAst(field.fieldType, location, bindingArg, [field])
    stmts.push(generateInlinePrintArg(fieldAst))
  })
  stmts.push(printf(rawstr(")")))
  return createStatements(location, stmts)
}

const generateInlinePrintArg = (arg: Ast) => {
  const location = arg.location
  const fieldHelper = (binding: Binding, name: string) => {
    const field = binding.type.typeInfo.fields.find(x => x.name === name)!
    return new ValueFieldAst(field.fieldType, location, new BindingAst(binding.type, location, binding), [field])
  }
  const formats = new Map()
  formats.set(IntType,        textColors.yellow("%i"))
  formats.set(u64Type,        textColors.yellow("%i"))
  formats.set(RawPointerType, textColors.blue("%p"))
  formats.set(FloatType,      textColors.yellow("%f"))
  formats.set(DoubleType,     textColors.yellow("%f"))

  const rawstr = (str: string) => new StringAst(RawPointerType, location, str)
  const printf = (...args: any) => new UserCallAst(VoidType, location, externalBuiltinBindings.printf, args)

  const printStringAst = (format: string, ast: Ast) => {
    const let_ = new LetAst(VoidType, location, new Binding("", StringType), ast, LetType.Let)
    const lengthGetter = fieldHelper(let_.binding, 'length')
    const dataGetter = fieldHelper(let_.binding, 'data')
    return createStatements(location, [let_, printf(rawstr(format), lengthGetter, dataGetter)])
  }

  if (arg.type.typeInfo.metaobject.printBinding) {
    const binding = arg.type.typeInfo.metaobject.printBinding as Binding
    const call_ = new UserCallAst(VoidType, location, binding, [arg])
    return call_
  }
  
  if (arg.type === StringType) {
    const binding = new Binding("", StringType)
    const let_ = new LetAst(VoidType, location, binding, arg, LetType.Let)
    const lengthGetter = fieldHelper(binding, 'length')
    const dataGetter = fieldHelper(binding, 'data')
    const call_ = printf(rawstr("%.*s"), lengthGetter, dataGetter)
    return createStatements(location, [let_, call_])
  }
  if (arg.type === BoolType) {
    const bool = new IfAst(StringType, location, arg, new StringAst(StringType, location, "true"), new StringAst(StringType, location, "false"))
    return printStringAst(textColors.yellow("%.*s"), bool)
  }
  if (formats.has(arg.type)) {
    return printf(rawstr(formats.get(arg.type)), arg)
  }
}

export const print = new CompilerFunction('print', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const location = ctx.location
  const stmts: Ast[] = []

  const fieldHelper = (binding: Binding, name: string) => {
    const field = binding.type.typeInfo.fields.find(x => x.name === name)!
    return new ValueFieldAst(field.fieldType, location, new BindingAst(binding.type, location, binding), [field])
  }
  const formats = new Map()
  formats.set(IntType,        textColors.yellow("%i"))
  formats.set(u64Type,        textColors.yellow("%i"))
  formats.set(RawPointerType, textColors.blue("%p"))
  formats.set(FloatType,      textColors.yellow("%f"))
  formats.set(DoubleType,     textColors.yellow("%f"))

  const rawstr = (str: string) => new StringAst(RawPointerType, location, str)
  const printf = (...args: any) => new UserCallAst(VoidType, location, externalBuiltinBindings.printf, args)

  args.forEach((arg, i) => {
    propagatedLiteralAst(arg)
    if (i !== 0) {
      stmts.push(printf(rawstr(" ")))
    }
    const ast = (() => {
      if (arg.type === StringType) {
        const binding = new Binding("", StringType)
        const let_ = new LetAst(VoidType, location, binding, arg, LetType.Let)
        const lengthGetter = fieldHelper(binding, 'length')
        const dataGetter = fieldHelper(binding, 'data')
        const call_ = printf(rawstr("%.*s"), lengthGetter, dataGetter)
        return createStatements(location, [let_, call_])
      }

      if (arg.type.typeInfo.metaobject.printBinding) {
        const binding = arg.type.typeInfo.metaobject.printBinding as Binding
        const call_ = new UserCallAst(VoidType, location, binding, [arg])
        return createStatements(location, [call_])
      } 
      
      if (formats.has(arg.type)) {
        return printf(rawstr(formats.get(arg.type)), arg)
      }
      
      compilerAssert(false, "Cannot print value of type $type. not implemented", { type: arg.type })
    })()


    stmts.push(ast)
  })
  stmts.push(printf(rawstr("\n")))
  return Task.of(createStatements(location, stmts))
})

export const unsafe_subscript = new CompilerFunction('unsafe_subscript', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [left, right] = args
  propagatedLiteralAst(right)
  compilerAssert(right && right.type === IntType, "Expected int type", { right })
  compilerAssert(left && left.type === RawPointerType, "Expected rawptr", { left })
  const type = expectType(typeArgs[0])
  return Task.of(new SubscriptAst(type, ctx.location, left, propagatedLiteralAst(right), {}))
})
export const unsafe_set_subscript = new CompilerFunction('unsafe_set_subscript', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [left, right, value] = args
  compilerAssert(typeArgs.length === 0, "Not implemented. cast value instead for now", { typeArgs })
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
  return createDefaultConstructorAst(type, ctx.location)
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

export const copyFunction = new CompilerFunction('copy', (ctx, typeArgs: unknown[], args: Ast[]) => {
  const [value] = args
  compilerAssert(isAst(value), "Expected ast", { value })
  return Task.of(new CallAst(value.type, ctx.location, externalBuiltinBindings.copy, [value], []))
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
      const msg = err.message.replace(/\x1b\[[0-9;]*m/g, '') // Ansi colors
      if (msg.includes(errorMsg)) return Task.of(new VoidAst(VoidType, ctx.location))
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

export const getEnumOf = (compiler: GlobalCompilerState, type: Type) => {
  compilerAssert(type.typeInfo.metaobject.isEnumVariant, "Expected enum variant", { type })
  const enumType = type.typeInfo.metaobject.enumType
  compilerAssert(enumType, "Expected enum type", { type })
  compilerAssert(isType(enumType), "Expected type", { enumType })
  return enumType
}

const findVariant = (type: Type, givenVariantType: unknown) => {
  if (typeof givenVariantType === 'string') {
    const variants = type.typeInfo.metaobject.variants as Type[]
    const found = variants.find(x => x.typeInfo.metaobject.isEnumVariant && x.typeInfo.metaobject.variantName === givenVariantType)
    compilerAssert(found, "Expected variant type", { givenVariantType, variants, shortNames: variants.map(x => x.shortName) })
    compilerAssert(isType(found), "Expected type", { found })
    return found
  }
  compilerAssert(false, "Not implemented yet", { givenVariantType })
}

const getTypeIndex = new CompilerFunction("getTypeIndex", (ctx, typeArgs, args) => {
  let [givenVariantType] = typeArgs
  const [value] = args
  const type = value.type
  compilerAssert(isAst(value), "Expected ast", { value })
  compilerAssert(value.type.typeInfo.metaobject.isEnum, "Expected enum value but got $type", { value, type: value.type })
  const variantType = findVariant(type, givenVariantType)
  compilerAssert(variantType.typeInfo.metaobject.isEnumVariant, "Expected enum variant", { type })
  const enumType = getEnumOf(ctx.compilerState.globalCompiler, variantType)
  compilerAssert(enumType === type, "Expected type to match $type $enumVariantOf", { type, enumType })
  compilerAssert(typeof variantType.typeInfo.metaobject.enumVariantIndex === 'number', "Expected number", { variantType })
  return Task.of(new NumberAst(IntType, ctx.location, variantType.typeInfo.metaobject.enumVariantIndex))
})

const isEnumVariant = new CompilerFunction("isEnumVariant", (ctx, typeArgs, args) => {
  const [value] = args
  const [variantType] = typeArgs
  compilerAssert(isAst(value), "Expected ast", { value })
  compilerAssert(value.type.typeInfo.metaobject.isEnum, "Expected enum value but got $type", { value, type: value.type })

  const fnDef = insertFunctionDefinition(ctx.compilerState.globalCompiler, isEnumVariantFn)
  const closure = new Closure(fnDef, ctx.compilerState.scope, ctx.compilerState)
  const v = new CompTimeObjAst(CompileTimeObjectType, ctx.location, variantType)
  return createCallAstFromValue(ctx, closure, [], [value, v])
})

const isTypeOf = new CompilerFunction("isType", (ctx, typeArgs, args) => {
  const [value] = args
  const [type] = typeArgs
  compilerAssert(isAst(value), "Expected ast", { value })
  if (type instanceof ExternalTypeConstructor) {
    if (value.type.typeInfo.metaobject.isEnum) {
      return isEnumVariant.func(ctx, typeArgs, args)
    }
    compilerAssert(false, "Not implemented yet", { type })
  } else if (isType(type)) {    
    return Task.of(new BoolAst(BoolType, ctx.location, value.type === type))
  }
  compilerAssert(false, "Expected type or type constructor", { type })
})

const unsafeEnumCast = new CompilerFunction("unsafeEnumCast", (ctx, typeArgs, args) => {
  const [value] = args
  let [givenVariantType] = typeArgs
  const type = value.type
  compilerAssert(isType(type), "Expected type", { type })
  compilerAssert(type.typeInfo.metaobject.isEnum, "Expected enum value but got $type", { type, args, typeArgs })
  const variantType = findVariant(value.type, givenVariantType)
  const enumType = getEnumOf(ctx.compilerState.globalCompiler, variantType)
  compilerAssert(type === enumType, "Expected type to match", { type, enumType })
  compilerAssert(typeof variantType.typeInfo.metaobject.enumVariantIndex === 'number', "Expected number", { variantType })
  return Task.of(new VariantCastAst(variantType, ctx.location, type, value))
})

export const guardSugar = (out: BytecodeWriter, node: ParseGuard) => {
  const token = node.token
  const blockIden = new ParseFreshIden(token, new FreshBindingToken('block'))
  const break_ = node.conditions.length === 1 ? node.elseBody : new ParseBreak(token, blockIden, null)
  const stmts = node.conditions.map(cond => {
    if (cond instanceof ParseLet) {
      if (cond.left instanceof ParseExtract) {
        compilerAssert(cond.value, "Expected value", { cond })
        return extractOrElse(cond.left, cond.value, break_)
      }
      compilerAssert(false, "Not implemented", { node })
    } else {
      return new ParseIf(token, false, new ParseNot(token, cond), break_, null)
    }
  })
  let block: ParseNode = new ParseStatements(token, stmts)
  // BlockNoScope means new variables will escape the scope. So we
  // really need to make sure that the elseBlock exits the scope
  // TODO: Do that
  if (node.conditions.length > 1) block = new ParseBlockNoScope(token, null, blockIden, block)
  visitParseNode(out, block)
}

export const ifMultiSugar = (out: BytecodeWriter, node: ParseIfMulti) => {
  // This is the same as option block, and match case
  // where we need to continue executing the block unless we 
  // hit a condition that causes us to break or continue,
  // and continue should switch to the next branch

  // {
  //   if (!... ) continue
  //   ...
  //   if (!... ) continue
  // } else {
  //   ...
  // }
  const token = node.token

  const outerIden = new ParseFreshIden(token, new FreshBindingToken('outer'))
  const innerIden = new ParseFreshIden(token, new FreshBindingToken('inner'))
  const resultIden = new ParseFreshIden(token, new FreshBindingToken('res'))

  const breakInner = new ParseBreak(token, innerIden, null)
  const conds = node.conditions.map(cond => {
    if (cond instanceof ParseLet) return new ParseGuard(token, [cond], breakInner)
    return new ParseIf(token, false, new ParseNot(token, cond), breakInner, null)
  })
  const let_ = new ParseLet(token, LetType.Alias, resultIden, null, node.trueBody)
  const breakOuter = new ParseBreak(token, outerIden, resultIden)
  const stmts = new ParseStatements(token, [...conds, let_, breakOuter])
  const innerBlock = new ParseBlock(token, null, innerIden, stmts)
  const else_ = new ParseStatements(token, filterNotNull([innerBlock, node.falseBody]))
  const outerBlock = new ParseBlock(token, null, outerIden, else_)
  visitParseNode(out, outerBlock)
}

const asExprTuple = new ExternalFunction('asExprTuple', VoidType, (ctx, args) => {
  const [subject, type, numFields, idenName] = args
  compilerAssert(typeof numFields === 'number', "Expected number", { numFields })
  compilerAssert(typeof idenName === 'string', "Expected string", { idenName })
  compilerAssert(isAst(subject), "Expected ast", { subject })

  const vm = ctx.compilerState.vm
  if (subject.type instanceof ParameterizedType && subject.type.typeConstructor === TupleTypeConstructor) {
    const tuple = subject.type
    compilerAssert(tuple instanceof ParameterizedType, "Expected parameterized type", { tuple })
    compilerAssert(tuple.typeConstructor === TupleTypeConstructor, "Expected tuple type", { tuple })
    compilerAssert(tuple.args.length === numFields, "Expected tuple with $expected fields got $got", { tuple, expected: tuple.args.length, got: numFields })
    compilerAssert(subject.type.typeConstructor === type, "Expected $subjectType to be a type or a variant of $type", { subjectType: subject.type, type })
    const location = ctx.location
    const bool_ = new BoolAst(BoolType, location, true)
    return new Tuple([bool_, subject])
  }
  if (subject.type instanceof ParameterizedType && subject.type.typeInfo.metaobject.isEnum) {
    compilerAssert(subject.type.typeInfo.metaobject.variants, "Expected variants", { subject })
    compilerAssert(typeof type === 'string', "Expected type constructor", { type })
    const variant = findVariant(subject.type, type)
    compilerAssert(variant !== undefined, "Expected $subjectType to be a type or have a variant of $type", { subjectType: subject.type, type, variant, variants: subject.type.typeInfo.metaobject.variants })
    const fnDef = insertFunctionDefinition(ctx.compilerState.globalCompiler, asEnumVariantFn)
    const closure = new Closure(fnDef, ctx.compilerState.scope, ctx.compilerState)

    return (
      TaskDef(callFunctionFromValueTask, vm, closure, [], [subject, type, idenName])
      .chainFn((task, _) => { return Task.of(vm.stack.pop()) })
    )
  }
  compilerAssert(false, "asExprTuple Not implemented", { subject, type, numFields, idenName })
})

const isEnumVariantFn = (() => {

  const token = createAnonymousToken('')
  const subjectIden = new ParseFreshIden(token, new FreshBindingToken('subject'))
  const testTypeIden = new ParseFreshIden(token, new FreshBindingToken('type'))

  const fnParams: ParserFunctionParameter[] = [
    { name: subjectIden, storage: null, capability: Capability.Let, type: null },
    { name: testTypeIden, storage: null, capability: Capability.Let, type: null },
  ]

  const indexIden = new ParseFreshIden(token, new FreshBindingToken('index'))
  const getTypeIndexCall = new ParseCall(token, new ParseValue(token, getTypeIndex), [subjectIden], [testTypeIden])
  const letIndex = new ParseLetConst(token, indexIden, new ParseQuote(token, getTypeIndexCall))

  const tag = new ParseField(token, subjectIden, new ParseIdentifier(createAnonymousToken('tag')))
  const cond = new ParseOperator(createAnonymousToken('=='), [tag, indexIden])
  const stmts = new ParseStatements(token, [letIndex, cond])

  return createAnonymousParserFunctionDecl('isEnumVariant', token, fnParams, stmts)
})()

const asEnumVariantFn = (() => {

  const token = createAnonymousToken('')
  const subjectIden = new ParseFreshIden(token, new FreshBindingToken('subject'))
  const testTypeIden = new ParseFreshIden(token, new FreshBindingToken('type'))
  const newNameIden = new ParseFreshIden(token, new FreshBindingToken('iden'))

  const fnParams: ParserFunctionParameter[] = [
    { name: subjectIden, storage: null, capability: Capability.Let, type: null },
    { name: testTypeIden, storage: null, capability: Capability.Let, type: null },
    { name: newNameIden, storage: null, capability: Capability.Let, type: null },
  ]

  const indexIden = new ParseFreshIden(token, new FreshBindingToken('index'))
  const getTypeIndexCall = new ParseCall(token, new ParseValue(token, getTypeIndex), [subjectIden], [testTypeIden])
  const letIndex = new ParseLetConst(token, indexIden, new ParseQuote(token, getTypeIndexCall))

  const tag = new ParseField(token, subjectIden, new ParseIdentifier(createAnonymousToken('tag')))
  const cond = new ParseOperator(createAnonymousToken('=='), [tag, indexIden])

  const cast = new ParseCall(token, new ParseValue(token, unsafeEnumCast), [subjectIden], [testTypeIden])

  const condExpr = new ParseStatements(token, [letIndex, cond])
  const condQuote = new ParseQuote(token, condExpr)

  const extractQuote = new ParseQuote(token, cast)
  const body = new ParseTuple(token, [condQuote, extractQuote])

  return createAnonymousParserFunctionDecl('asEnumVariant', token, fnParams, body)
})()

const guardAsExprSugar = (subject: ParseNode, asType: ParseNode, numFields: number, iden: ParseFreshIden | ParseIdentifier, elseExpr: ParseNode) => {
  const token = subject.token
  const asTupleIden = new ParseFreshIden(token, new FreshBindingToken('astuple'))
  const idenName = iden instanceof ParseFreshIden ? iden.freshBindingToken.identifier : iden.token.value
  const subjectQuote = new ParseQuote(token, subject)
  const asExprCall = new ParseCall(token, new ParseValue(token, asExprTuple), [subjectQuote, asType, new ParseNumber(createAnonymousToken(numFields)), new ParseString(token, idenName)], [])
  const letAsTuple = new ParseLetConst(token, asTupleIden, asExprCall)
  const cond = new ParseEvalFunc(token, (vm) => {
    const value = vm.stack.pop()
    compilerAssert(value instanceof Tuple, "Expected tuple", { value })
    vm.stack.push(value.values[0])
  }, [], [asTupleIden])
  
  const extractValue = new ParseEvalFunc(token, (vm) => {
    const value = vm.stack.pop()
    compilerAssert(value instanceof Tuple, "Expected tuple", { value })
    vm.stack.push(value.values[1])
  }, [], [asTupleIden])

  const letExtract = new ParseLet(token, LetType.Let, iden, null, extractValue)
  const if_ = new ParseIf(token, true, new ParseNot(token, cond), elseExpr, null)
  return new ParseStatements(token, [letAsTuple, if_, letExtract])
}


const extractOrElse = (node: ParseNode, subject: ParseNode, elseBlock: ParseNode): ParseNode => {
  // compilerAssert(false, "Not implemented", { node })
  const token = node.token

  if (node instanceof ParseFreshIden) return new ParseLet(token, LetType.Alias, node, null, subject)
  if (node instanceof ParseIdentifier) return new ParseLet(token, LetType.Alias, node, null, subject)
  if (node instanceof ParseNumber || node instanceof ParseString || node instanceof ParseBoolean) {
    return new ParseIf(token, false, new ParseOperator(createAnonymousToken('!='), [subject, node]), elseBlock, null)
  }

  const extractIden = new ParseFreshIden(token, new FreshBindingToken('extract'))

  if (node instanceof ParseSymbol) {
    return guardAsExprSugar(subject, node, 0, extractIden, elseBlock)
  }

  if (node instanceof ParseExtract) {
    const numArgs = node.args.length
    if (numArgs === 0)
      return guardAsExprSugar(subject, node.name, 0, extractIden, elseBlock)

    const guard = guardAsExprSugar(subject, node.name, numArgs, extractIden, elseBlock)
    const extractedSubject = new ParseField(token, extractIden, new ParseIdentifier(createAnonymousToken('value')))
    const bind = node.args.map(x => extractOrElse(x, extractedSubject, elseBlock))
    return new ParseStatements(token, [guard, ...bind])
  }

  if (node instanceof ParseTuple) {
    const tupleType = new ParseValue(token, TupleTypeConstructor)
    const guard = guardAsExprSugar(subject, tupleType, node.exprs.length, extractIden, elseBlock)
    const bind = node.exprs.map((x, i) => {
      const field = new ParseField(token, extractIden, new ParseIdentifier(createAnonymousToken(`_${i+1}`)))
      return extractOrElse(x, field, elseBlock)
    })
    return new ParseStatements(token, [guard, ...bind])
  }
  
  compilerAssert(false, "Not implemented", { node })
}

const caseOrElse = (node: ParseMatchCase, subject: ParseNode, elseBlock: ParseNode): ParseNode => {
  const token = node.token
  const smts = [extractOrElse(node.extract, subject, elseBlock)]
  if (node.condition) smts.push(new ParseIf(token, false, new ParseNot(token, node.condition), elseBlock, null))

  const resIden = new ParseFreshIden(token, new FreshBindingToken('res'))
  const letRes = new ParseLet(token, LetType.Alias, resIden, null, node.body)
  return new ParseStatements(token, [...smts, letRes, resIden])
}

export const matchSugar = (out: BytecodeWriter, node: ParseMatch) => {
  const token = node.token
  const subjectIden = new ParseFreshIden(token, new FreshBindingToken('subject'))
  const letSubject = new ParseLet(token, LetType.Let, subjectIden, null, node.subject)
  const outerIden = new ParseFreshIden(token, new FreshBindingToken('match'))
  const options = node.cases.map(case_ => {
    // The case block is named `node.name` so that a continue `name` statement will continue to the next case
    // TODO: This is a bit of a hack so we should make a match block a specific type of block
    // and then also allow break `name` statements to break out of the whole thing. Or seperate
    // the block types and wrap a breakable block around the whole thing
    const caseIden = node.name ?? new ParseFreshIden(token, new FreshBindingToken('case'))
    const caseBlock = caseOrElse(case_, subjectIden, new ParseBreak(token, caseIden, null))
    const res = new ParseBreak(token, outerIden, caseBlock)
    return new ParseBlock(token, 'continue', caseIden, new ParseStatements(token, [res]))
  })
  const defaultElse = new ParseCall(token, new ParseIdentifier(createAnonymousToken('unreachable')), [], [])

  const stmts = new ParseBlock(token, null, outerIden, new ParseStatements(token, [letSubject, ...options, defaultElse]))
  visitParseNode(out, stmts)
}

export const isSugar = (out: BytecodeWriter, node: ParseIs) => {
  // compilerAssert(false, "isSugar not implemented", { node })
  const call = new ParseCall(node.token, new ParseValue(node.token, isTypeOf), [node.expr], [node.type])
  visitParseNode(out, call)
}

export const orElseSugar = (out: BytecodeWriter, node: ParseOrElse) => {
  const token = node.token
  const letIden = new ParseFreshIden(token, new FreshBindingToken('orelse'))
  const letNode = new ParseLet(token, LetType.Let, letIden, null, node.expr)
  const valueIden = new ParseFreshIden(token, new FreshBindingToken('value'))

  const extract = new ParseExtract(token, new ParseSymbol(createAnonymousToken("Some")), [valueIden])
  const ifLet = new ParseLet(token, LetType.Let, extract, null, letIden)
  const ifNode = new ParseIfMulti(token, true, [ifLet], valueIden, new ParseElse(token, node.orElse))
  const stmts = new ParseStatements(token, [letNode, ifNode])
  visitParseNode(out, stmts)
}

const someOrVoid = new CompilerFunction('someOrVoid', (ctx, typeArgs, args): Task<Ast, CompilerError> => {
  if (args[0].type === VoidType) return Task.of(args[0])
  const typeArg = typeArgs[0] ?? args[0].type
  return (
    createParameterizedExternalType(ctx.compilerState.globalCompiler, OptionTypeConstructor, [typeArg])
    .chainFn((task, optionType) => {
      return createEnumVariant.func(ctx, [optionType, 1], args)
    })
  )
})
const noneOrVoid = new CompilerFunction('noneOrVoid', (ctx, typeArgs, args): Task<Ast, CompilerError> => {
  if (typeArgs[0] === VoidType) return Task.of(new VoidAst(VoidType, ctx.location))
  return (
    createParameterizedExternalType(ctx.compilerState.globalCompiler, OptionTypeConstructor, typeArgs)
    .chainFn((task, optionType) => {
      return createEnumVariant.func(ctx, [optionType, 0], args)
    })
  )
})

const createEnumVariant = new CompilerFunction('createEnumVariant', (ctx, typeArgs, args): Task<Ast, CompilerError> => {
  const [enumType, variantIndex] = typeArgs
  compilerAssert(isType(enumType), "Expected type", { enumType })
  const metaobject = (enumType as ParameterizedType).typeInfo.metaobject as any
  compilerAssert(typeof variantIndex === 'number', "Expected variantIndex as number", { variantIndex })
  const variantType = metaobject.variants[variantIndex]
  compilerAssert(variantType, "Expected variant type", { variantIndex, variants: metaobject.variants })
  compilerAssert(isType(variantType), "Expected type", { variantType })
  const newArgs = args.map(x => propagatedLiteralAst(x))
  while (newArgs.length < enumType.typeInfo.fields.length - 1) {
    const argType = enumType.typeInfo.fields[newArgs.length].fieldType
    newArgs.push(createDefaultConstructorAst(argType, ctx.location))
  }
  newArgs.unshift(new NumberAst(IntType, ctx.location, variantIndex))
  return Task.of(new EnumVariantAst(enumType, ctx.location, variantType, enumType, newArgs))
})

const SomeConstructor = someOrVoid
const NoneConstructor = noneOrVoid

export const optionBlockSugar = (out: BytecodeWriter, node: ParseBlock) => {
  const name = (node.name instanceof ParseFreshIden ? node.name.freshBindingToken.identifier : node.name?.token.value) ?? null
  const outerIden = new ParseFreshIden(node.token, new FreshBindingToken('optionouter'))
  const stmtsIden = new ParseFreshIden(node.token, new FreshBindingToken('stmts'))

  // Do a bit of a hack here to figure out if we are in an optional block and if so, what the
  // inferred type should be of the optional. Would be nice to have a more general way of doing this

  const bytecode = { code: [], locations: [] }
  const prevOptionalBlock = out.state.optionalBlock
  const optionalBlock = { didBreak: false }
  out.state.optionalBlock = optionalBlock
  const writer = { location: node.token.location, bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }
  visitParseNode(writer, new ParseLetConst(node.token, stmtsIden, new ParseValue(node.token, null)))
  pushBytecode(writer, node.token, { type: 'pop' })
  pushBytecode(writer, node.token, { type: 'beginblockast', breakType: node.breakType, name: outerIden.freshBindingToken.identifier, scope: true })
  pushBytecode(writer, node.token, { type: 'beginblockast', breakType: node.breakType, name, scope: true })
  visitParseNode(writer, new ParseMeta(node.token, new ParseSet(node.token, stmtsIden, new ParseQuote(node. token, node.statements))))
  pushBytecode(writer, node.token, { type: 'pop' })
  if (optionalBlock.didBreak) {
    const some_ = new ParseCall(node.token, new ParseValue(node.token, someOrVoid), [stmtsIden], [])
    visitParseNode(writer, new ParseBreak(node.token, outerIden, some_))
  } else {
    visitParseNode(writer, stmtsIden)
  }
  pushBytecode(writer, node.token, { type: 'endblockast', scope: true })
  if (optionalBlock.didBreak) {
    const top = new ParseEvalFunc(node.token, (vm) => { }, [], [])
    const inferType = new ParseCall(node.token, new ParseValue(node.token, typeOf), [stmtsIden], [])
    const none_ = new ParseCall(node.token, new ParseValue(node.token, noneOrVoid), [], [inferType])
    const stmts = new ParseStatements(node.token, [top, none_])
    visitParseNode(writer, stmts)
  }
  pushBytecode(writer, node.token, { type: 'endblockast', scope: true })
  out.state.optionalBlock = prevOptionalBlock
  
  const bc = new ParseBytecode(node.token, bytecode)
  visitParseNode(out, bc)
}

export const questionSugar = (out: BytecodeWriter, node: ParseQuestion) => {
  compilerAssert(out.state.optionalBlock, "Expected optional block")
  out.state.optionalBlock.didBreak = true
  const break_ = new ParseBreakOpt(node.token, null)
  orElseSugar(out, new ParseOrElse(node.token, node.expr, break_))
}

export const metaLetIn = (token: Token, node: ParseNode, f: (iden: ParseFreshIden) => ParseNode[]) => {
  const iden = new ParseFreshIden(token, new FreshBindingToken('let_in'))
  const let_ = new ParseLetConst(token, iden, node)
  return new ParseStatements(token, [let_, ...f(iden)])
}

export const subscriptCompiler = (ctx: CompilerFunctionCallContext, subject: Ast, args: Ast[]): Task<Ast, CompilerError> => {
  compilerAssert(args.length === 1, "Expected one argument", { args })

  const alreadyCompiling = subject.type.typeInfo.metaobject['subscriptCompiled'] === 'compiling'
  compilerAssert(!alreadyCompiling, "Already compiling subscript. This could mean an infinite loop", { subject })
  
  const compiled = subject.type.typeInfo.metaobject['subscriptCompiled'] as any
  if (compiled) {
    return Task.of(new SubscriptAst(compiled.type, ctx.location, subject, args[0], compiled.capabilities))
  }

  subject.type.typeInfo.metaobject['subscriptCompiled'] = 'compiling'

  const subscriptInout = subject.type.typeInfo.metaobject['subscript_inout']
  compilerAssert(subscriptInout, "No 'subscript_inout' operator found for $type", { type: subject.type })
  compilerAssert(subscriptInout instanceof Closure, "Expected closure", { subscriptInout })

  type SubscriptResult = { type: Type, binding: Binding }
  const compileSubscript = (name: string): Task<SubscriptResult | null, CompilerError> => {

    const closure = subject.type.typeInfo.metaobject[name]
    if (!closure) return Task.of(null)

    compilerAssert(closure, "No '$name' operator found for $type", { name, type: subject.type })
    compilerAssert(closure instanceof Closure, "Expected closure", { closure })
    
    let type: Type
    const lambda2 = new CompilerFunction(name, (ctx, typeArgs, args) => {
      const ast = propagatedLiteralAst(args[0])
      type = ast.type
      if (type === IntLiteralType) compilerAssert(false, "Int literals not supported", { type, ast: args[0] })
      compilerAssert(type, "Expected type", { ast })
      compilerAssert(type !== VoidType, "Expected non-void type", { type })
      return Task.of(new YieldAst(VoidType, ctx.location, ast))
    })
    const lambdaAst = new CompTimeObjAst(CompileTimeObjectType, ctx.location, lambda2)

    return (
      createCallAstFromValue(ctx, closure, [], [subject, args[0], lambdaAst])
      .chainFn((task, res) => {
        compilerAssert(type, "Expected result for inout type", { type, res })
        compilerAssert(res instanceof UserCallAst, "Expected call ast", { res })
        return Task.of({ type, binding: res.binding })
      })
    )
  }

  return compileSubscript("subscript").chainFn((task, letResult) => {
    return compileSubscript("subscript_inout").chainFn((task, inoutResult) => {
      return compileSubscript("subscript_sink").chainFn((task, sinkResult) => {
        return compileSubscript("subscript_set").chainFn((task, setResult) => {
          const results = [letResult, inoutResult, sinkResult, setResult].filter(x => x) as SubscriptResult[]
          if (results.length === 0) compilerAssert(false, "No subscript operator found", { letResult, inoutResult, sinkResult, setResult })
          const type = results[0].type
          compilerAssert(results.every(x => x.type === type), "Expected types to match when compiling subscript operators", { inoutResult, letResult })

          const compiled = { type: type, capabilities: {
            [Capability.Let]: letResult?.binding,
            [Capability.Inout]: inoutResult?.binding,
            [Capability.Set]: setResult?.binding,
            [Capability.Sink]: sinkResult?.binding
          }}
          subject.type.typeInfo.metaobject['subscriptCompiled'] = compiled
          return Task.of(new SubscriptAst(type, ctx.location, subject, args[0], compiled.capabilities))
        })
      })
    })
  })
}

export const createCompilerModuleTask = (ctx: TaskContext): Task<Module, CompilerError> => {
  const moduleScope = createScope({}, undefined)
  Object.assign(moduleScope, { 
    unsafe_subscript, unsafe_set_subscript, operator_bitshift_left, operator_bitshift_right,
    operator_bitwise_and, operator_bitwise_or, rawptr: RawPointerType, add_external_library, add_macos_framework, assert, never: NeverType,
    get_current_loop, ctobj: CompileTimeObjectType, operator_mod, overloaded, static_length, assert_compile_error, initializer_function, add_export,
    concat, Option: OptionTypeConstructor, Some: SomeConstructor, None: NoneConstructor, unsafe_enum_cast: unsafeEnumCast, is_enum_variant: isEnumVariant, generator,
    copy: copyFunction })
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `compiler module`, lexicalParent: undefined, scope: moduleScope })
  const module = new Module('compiler', subCompilerState, null!)
  return Task.of(module)
}
