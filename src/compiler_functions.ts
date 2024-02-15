import { BytecodeDefault, BytecodeSecondOrder, compileClassTask, compileFunctionPrototype, createBytecodeVmAndExecuteTask, propagateLiteralType, propagatedLiteralAst, pushBytecode, pushGeneratedBytecode, visitParseNode } from "./compiler";
import { externalBuiltinBindings } from "./compiler_sugar";
import { BytecodeWriter, FunctionDefinition, Type, Binding, LetAst, Ast, StatementsAst, Scope, createScope, compilerAssert, VoidType, Vm, bytecodeToString, ParseIdentifier, ParseNode, CompiledFunction, AstRoot, isAst, pushSubCompilerState, ParseNil, createToken, ParseStatements, FunctionType, ParserFunctionDecl, Tuple, hashValues, TaskContext, GlobalCompilerState, isType, ParseNote, createAnonymousToken, textColors, CompilerError, PrimitiveType, CastAst, CallAst, IntType, Closure, UserCallAst, ParameterizedType, expectMap, ConcreteClassType, ClassDefinition, ParseCall, TypeVariable, TypeMatcher, typeMatcherEquals, SourceLocation, ExternalTypeConstructor, ScopeParentSymbol, SubCompilerState, CompilerFunction, IntLiteralType, FloatLiteralType, FloatType, RawPointerType, AddressAst, BindingAst, UnknownObject, NeverType, CompilerFunctionCallContext, CompileTimeObjectType, CompTimeObjAst } from "./defs";
import { Task, TaskDef, Unit } from "./tasks";


export const insertFunctionDefinition = (compilerState: GlobalCompilerState, decl: ParserFunctionDecl) => {
  if (decl.id !== undefined) return compilerState.functionDefinitions[decl.id];

  decl.id = compilerState.functionDefinitions.length;
  const keywords = decl.keywords.map(x => x instanceof ParseNote ? x.expr.token.value : x.token.value)
  const inline = !!decl.anonymous || keywords.includes('inline')
  const funcDef = new FunctionDefinition(
    decl.id, decl.debugName,
    decl.name, decl.typeParams, decl.params,
    decl.returnType, decl.body,
    inline)
  funcDef.keywords.push(...keywords)

  compilerState.functionDefinitions.push(funcDef);
  return funcDef;
}

export type TypeCheckHeaderArg = {
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  result: TypeCheckResult
}
export type TypeCheckResult = {
  concreteTypes: Type[]
  substitutions: UnknownObject
  returnType: Type
}

function compileAndExecuteFunctionHeaderTask(ctx: TaskContext, { func, args, typeArgs, parentScope, result }: TypeCheckHeaderArg): Task<Unit, CompilerError> {

  compilerAssert(typeArgs.length <= func.typeParams.length, "Expected $expected type parameters, got $got", { expected: func.typeParams.length, got: typeArgs.length, func })
  compilerAssert(args.length === func.params.length, 'Expected $expected args got $got', { expected: func.params.length, got: args.length, func })

  // if (func.params.length === 0) return Task.success()
  
  if (!func.headerPrototype) {

    func.headerPrototype = { name: `${func.debugName} header`, body: null!, initialInstructionTable: BytecodeDefault };
    func.headerPrototype.bytecode = { code: [], locations: [] }
    const out: BytecodeWriter = {
      bytecode: func.headerPrototype.bytecode,
      instructionTable: func.headerPrototype.initialInstructionTable,
      globalCompilerState: ctx.globalCompiler,
      state: { labelBlock: null, expansion: null }
    }
    // visitParseNode(out, func.headerPrototype.body);
    func.params.forEach(({ name, type }, i) => {
      if (type === null) return visitParseNode(out, new ParseNil(createAnonymousToken('')));
      visitParseNode(out, type)
      pushBytecode(out, type.token, { type: 'totype' })
    })

    pushGeneratedBytecode(out, { type: "tuple", count: func.params.length })
    if (func.returnType === null) visitParseNode(out, new ParseNil(createAnonymousToken('')))
    else {
      visitParseNode(out, func.returnType)
      pushBytecode(out, func.returnType.token, { type: 'totype' })
    }
    pushGeneratedBytecode(out, { type: "tuple", count: 2 })
    pushGeneratedBytecode(out, { type: "halt" })

    ctx.globalCompiler.logger.log(textColors.cyan(`Compiled ${func.headerPrototype.name}`))
    ctx.globalCompiler.logger.log(bytecodeToString(func.headerPrototype.bytecode))
    ctx.globalCompiler.logger.log("")
  }

  const scope = createScope({}, parentScope)
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} header`, scope, lexicalParent: ctx.subCompilerState });
  ;(subCompilerState as any).location = func.name?.token.location
  subCompilerState.functionCompiler = subCompilerState

  func.params.forEach((param, i) => {
    scope[param.name.token.value] = args[i]
  })
  func.typeParams.forEach((typeParam, i) => {
    compilerAssert(typeParam instanceof ParseIdentifier)
    if (!typeArgs[i]) scope[typeParam.token.value] = new TypeVariable(typeParam.token.value)
    else scope[typeParam.token.value] = typeArgs[i];
  })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.headerPrototype!.bytecode!, scope)
    .chainFn((task, resultTuple) => {
      compilerAssert(resultTuple instanceof Tuple, "Expected tuple")
      let [compiledArgTypes, returnType] = resultTuple.values
      
      returnType = returnType || VoidType
      // compilerAssert(isType(returnType), "Expected type got $returnType", { returnType })
      result.returnType = returnType as any // TODO: Fix this

      compilerAssert(compiledArgTypes instanceof Tuple, "Expected tuple")
      compiledArgTypes.values.forEach((type, i) => {
        let givenType = args[i].type
        if (givenType === IntLiteralType && type === FloatType) givenType = FloatType
        else if (givenType === IntLiteralType) givenType = IntType
        else if (givenType === FloatLiteralType) givenType = FloatType
        
        if (type === null) {
        } else if (type instanceof TypeMatcher) {
          const matches = typeMatcherEquals(type, args[i].type, result.substitutions)
          compilerAssert(matches, "Type check failed. Expected $expected got $got", { expected: type, got: args[i].type })
        } else if (type instanceof TypeVariable) {
          result.substitutions[type.name] = givenType
        } else {
          compilerAssert(isType(type), "Expected type got $type", { type });
          compilerAssert(givenType === type, "Argument $name of type $value does not match $expected", { name: func.params[i].name.token, value: args[i].type, expected: type })
        }

        compilerAssert(givenType !== IntLiteralType && givenType !== FloatLiteralType, "Unexpected literal type", { type: result.concreteTypes.at(-1) })

        result.concreteTypes.push(givenType)

      })
      return Task.success()
    })
  )
}

export type TypeCheckAndCompileArg = {
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  result: TypeCheckResult
}

export function functionTemplateTypeCheckAndCompileTask(ctx: TaskContext, { func, typeArgs, args, parentScope, result }: TypeCheckAndCompileArg): Task<CompiledFunction, CompilerError> {

  const argBindings: Binding[] = [];

  compilerAssert(func.body, "Expected function body")

  if (!func.templatePrototype)  {
    func.templatePrototype = { name: `${func.debugName} template bytecode`, body: func.body, initialInstructionTable: BytecodeSecondOrder };
    compileFunctionPrototype(ctx, func.templatePrototype);
  }
  compilerAssert(func.templatePrototype);

  const inferedTypeParams: unknown[] = []
  func.typeParams.forEach((typeParam, i) => {
    compilerAssert(typeParam instanceof ParseIdentifier, "Not implemented")
    const name = typeParam.token.value
    if (result.substitutions[name] === undefined) {
      compilerAssert(typeArgs[i] !== undefined, "Expected type arg", { substitutions: result.substitutions })
      inferedTypeParams.push(typeArgs[i])
    } else inferedTypeParams.push(result.substitutions[name])
  })

  const typeParamHash = hashValues(inferedTypeParams)
  const existing = func.compiledFunctions.find(compiledFunc => {
    if (compiledFunc.typeParamHash === typeParamHash) {
      if (compiledFunc.typeParameters.every((x, i) => x === inferedTypeParams[i])) return true
    }
  })
  if (existing) return Task.of(existing)

  const templateScope = createScope({}, parentScope); // TODO: parent scope
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} template`, scope: templateScope, lexicalParent: ctx.subCompilerState });
  ;(subCompilerState as any).location = func.name?.token.location
  subCompilerState.functionCompiler = subCompilerState

  func.typeParams.forEach((typeParam, i) => {
    compilerAssert(typeParam instanceof ParseIdentifier, "Not implemented")
    const name = typeParam.token.value
    templateScope[name] = inferedTypeParams[i]
  })
  
  func.params.forEach(({ name, type, storage }, i) => {
    const binding = new Binding(name.token.value, result.concreteTypes[i]);
    binding.storage = storage
    binding.definitionCompiler = subCompilerState
    templateScope[name.token.value] = binding
    argBindings.push(binding)
  });

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.templatePrototype.bytecode!, templateScope)
    .chainFn((task, ast) => {

      ctx.globalCompiler.logger.log(textColors.cyan(`Compiled template ${func.debugName}`))
      
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });
      propagatedLiteralAst(ast)

      const id = func.compiledFunctions.length;
      const binding = new Binding(`${func.debugName} compiled ${id}`, FunctionType);
      let returnType = result.returnType
      // TODO: Proper checking here
      if (returnType === VoidType) {
        // compilerAssert(ast instanceof StatementsAst)
        // ast.type = VoidType
      } else if (ast.type !== NeverType && ast.type !== result.returnType) {
        compilerAssert(false, "Invalid return type got $got expected $expected", { got: ast.type, expected: result.returnType })
      }
      
      const compiledFunction = new CompiledFunction(
          binding, func, returnType, result.concreteTypes, ast, argBindings, typeArgs, typeParamHash);
      ctx.globalCompiler.compiledFunctions.set(binding, compiledFunction);
      func.compiledFunctions.push(compiledFunction)
      
      return Task.of(compiledFunction)
    })
  )

}

export type FunctionCallArg = {
  location: SourceLocation
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  lexicalParent: SubCompilerState
  result: TypeCheckResult
}

function functionInlineTask(ctx: TaskContext, { location, func, typeArgs, args, parentScope, lexicalParent, result }: FunctionCallArg): Task<Ast, CompilerError> {

  const argBindings: Binding[] = [];
  const statements: Ast[] = []

  compilerAssert(func.body)

  if (!func.templatePrototype)  {
    func.templatePrototype = { name: `${func.debugName} inline bytecode`, body: func.body, initialInstructionTable: BytecodeSecondOrder };
    compileFunctionPrototype(ctx, func.templatePrototype);
  }
  compilerAssert(func.templatePrototype);
  
  const inlineInto = ctx.subCompilerState
  const templateScope = createScope({}, parentScope)
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} inline`, lexicalParent, scope: templateScope })
  subCompilerState.inlineIntoCompiler = inlineInto
  subCompilerState.labelBlock = lexicalParent.labelBlock
  subCompilerState.nextLabelBlockDepth = inlineInto.nextLabelBlockDepth
  const { concreteTypes } = result
  
  func.params.forEach(({ name, type, storage }, i) => {
    compilerAssert(concreteTypes[i], `Expected type`, { args, concreteTypes })
    compilerAssert(concreteTypes[i] !== IntLiteralType)
    if (args[i] instanceof CompTimeObjAst) { // Special case compile time objects
      templateScope[name.token.value] = args[i]
      return
    }
    const binding = new Binding(name.token.value, concreteTypes[i])
    binding.storage = storage // is this right for inline?
    binding.definitionCompiler = inlineInto
    templateScope[name.token.value] = binding
    propagateLiteralType(concreteTypes[i], args[i])
    statements.push(new LetAst(VoidType, location, binding, args[i]))
    argBindings.push(binding)
  });
  
  func.typeParams.forEach((typeParam, i) => {
    compilerAssert(typeParam instanceof ParseIdentifier, "Not implemented")
    const typeArg = typeArgs[i] || result.substitutions[typeParam.token.value]
    compilerAssert(typeArg, "Type arg not found $name", { name: typeParam.token.value, typeArgs })
    templateScope[typeParam.token.value] = typeArg
  });

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.templatePrototype.bytecode!, templateScope)
    .chainFn((task, ast) => {

      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      ctx.globalCompiler.logger.log(textColors.cyan(`Compiled inline ${func.debugName}`))
      return Task.of(new StatementsAst(ast.type, location, [...statements, ast]))
    })
  )

}

export type CompileTimeFunctionCallArg = {
  vm: Vm
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: unknown[],
  parentScope: Scope
}

export function functionCompileTimeCompileTask(ctx: TaskContext, { vm, func, typeArgs, args, parentScope }: CompileTimeFunctionCallArg): Task<Unit, CompilerError> {
  compilerAssert(func.body, "Expected body");
  compilerAssert(args.length === func.params.length, "Expected $expected arguments got $got", { expected: func.params.length, got: args.length, func })

  if (!func.compileTimePrototype) 
    func.compileTimePrototype = { name: `${func.debugName} comptime bytecode`, body: func.body, initialInstructionTable: BytecodeDefault };
  compilerAssert(func.compileTimePrototype)

  const scope = createScope({}, parentScope)
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} comptime`, lexicalParent: ctx.subCompilerState, scope })
  subCompilerState.functionCompiler = subCompilerState

  args.forEach((arg, i) => {
    scope[func.params[i].name.token.value] = arg;
  });
  func.typeParams.forEach((typeParam, i) => {
    compilerAssert(typeParam instanceof ParseIdentifier, "Not implemented")
    scope[typeParam.token.value] = typeArgs[i];
  });
  compileFunctionPrototype(ctx, func.compileTimePrototype);

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.compileTimePrototype.bytecode!, scope)
    .chainFn((task, res) => { vm.stack.push(res); return Task.success() })
  );
};

export function createCallAstFromValueAndPushValue(vm: Vm, value: unknown, typeArgs: unknown[], args: Ast[]): Task<Unit, CompilerError> {
  const ctx: CompilerFunctionCallContext = { location: vm.location, compilerState: vm.context.subCompilerState }
  return (
    createCallAstFromValue(ctx, value, typeArgs, args)
    .chainFn((task, ast) => {vm.stack.push(ast); return Task.success() })
  )
}

export function createCallAstFromValue(ctx: CompilerFunctionCallContext, value: unknown, typeArgs: unknown[], args: Ast[]): Task<Ast, CompilerError> {
  const location = ctx.location 
  if (value instanceof PrimitiveType) {
    compilerAssert(args.length === 1 && typeArgs.length === 0, "Expected 1 arg got $count", { count: args.length })
    const ast = propagatedLiteralAst(args[0])
    if (ast.type === value) return Task.of(ast)
    return Task.of(new CastAst(value, location, propagatedLiteralAst(args[0])))
  }

  if (value instanceof ClassDefinition) {
    return (
      TaskDef(compileClassTask, { classDef: value, typeArgs }).
      chainFn((task, clssType) => {
        const constructor = expectMap(clssType.typeInfo.metaobject, 'constructor', "Expected constructor in metaobject for object $obj", { obj: value })
        return createCallAstFromValue(ctx, constructor, [], args)
      })
    )
  }

  if (value instanceof CompilerFunction) {
    return Task.of(value.func(ctx, typeArgs, args))
  }

  if (value instanceof Closure) {

    if (typeArgs.some(x => x instanceof ClassDefinition && !x.concreteType)) {

      const compileTypeArgs = Task.concurrency(typeArgs.map(typeArg => {
        if (typeArg instanceof ClassDefinition && !typeArg.concreteType) {
          compilerAssert(typeArg.typeArgs.length === 0, "Cannot compile class $classDef to type without specifing type arguments", { classDef: typeArg })
          return TaskDef(compileClassTask, { classDef: typeArg, typeArgs: [] })
        }
        return Task.of(typeArg)
      }))

      // Call self
      return (
        compileTypeArgs.chainFn((task, compiledTypeArgs) => 
          TaskDef(createCallAstFromValue, value, compiledTypeArgs, args)
        )
      )
    }
    
    const { func, scope: parentScope, lexicalParent } = value
    const call: FunctionCallArg = { location, func, typeArgs, args, parentScope, lexicalParent, result: { concreteTypes: [], substitutions: {}, returnType: undefined! } }
    
    if (func.keywords.includes('external')) {
      const name = func.name
      compilerAssert(name, "Expected name for external function");
      return (
        TaskDef(compileAndExecuteFunctionHeaderTask, call)
        .chainFn((task, arg) => { 
          const ctx = (task._context as TaskContext)
          const existing = ctx.globalCompiler.externalDefinitions.find(x => x.name === name.token.value)
          const paramHash = hashValues(call.result.concreteTypes)
          compilerAssert(!existing || existing.paramHash === paramHash, "Function exists with different param hash", { existing })
          const binding = existing?.binding ?? externalBuiltinBindings[name.token.value] ?? new Binding(name.token.value, FunctionType)

          if (!existing) ctx.globalCompiler.externalDefinitions.push({ name: name.token.value, binding, paramHash, paramTypes: call.result.concreteTypes, returnType: call.result.returnType })
          compilerAssert(call.result.returnType, "Expected return type got $returnType", { returnType: call.result.returnType })
          const mappedArgs = args.map((ast, i) => {
            propagateLiteralType(call.result.concreteTypes[i], ast)
            compilerAssert(func.params[i].storage !== 'ref', "Not implemented")
            return ast
          })
          return Task.of(new UserCallAst(call.result.returnType, location, binding, mappedArgs))
        })
      )
    }
    
    if (func.inline) return (
      TaskDef(compileAndExecuteFunctionHeaderTask, call)
      .chain(TaskDef(functionInlineTask, call))
    )
    return (
      TaskDef(compileAndExecuteFunctionHeaderTask, call)
      .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
      .chainFn((task, compiledFunction) => {
        const binding = compiledFunction.binding
        const returnType = compiledFunction.returnType // TODO: Properly check result
        const mappedArgs = args.map((ast, i) => {
          propagateLiteralType(call.result.concreteTypes[i], ast)
          if (func.params[i].storage === 'ref') {
            compilerAssert(ast instanceof BindingAst, "Expected binding ast for now", { ast })
            return new AddressAst(RawPointerType, ast.location, ast.binding)
          }
          return ast
        })
        return Task.of(new UserCallAst(returnType, location, binding, mappedArgs))
      })
    )
  }

  compilerAssert(false, "Not supported value $value", { value })

}

export const createMethodCall = (vm: Vm, receiver: Ast, name: string, typeArgs: unknown[], args: Ast[]) => {
  const type = receiver.type instanceof ParameterizedType ? receiver.type.typeConstructor : receiver.type
  const t = type instanceof ConcreteClassType ? type.compiledClass.classDefinition : type;
  compilerAssert(t instanceof ClassDefinition || t instanceof ExternalTypeConstructor || t instanceof PrimitiveType, "Expected class, type or type constructor got $t", { t })

  // TODO: Must wait for event if not found
  const findClosure = (initialScope: Scope) => {
    let checkScope: Scope | undefined = initialScope;
    while (checkScope) {
      const methods = vm.context.globalCompiler.methods.get(checkScope)
      if (methods) {
        const found = methods.find(x => x[0] === t && x[1].func.name?.token.value === name)
        if (found) return found[1]
      }
      if (checkScope[name] !== undefined) return checkScope[name]
      checkScope = (checkScope as any)[ScopeParentSymbol]
    }
  }
  let closure = findClosure(vm.scope)
  if (!closure && type instanceof ConcreteClassType) {
    closure = findClosure(type.compiledClass.classDefinition.parentScope)
  } else if (!closure && type instanceof ClassDefinition) {
    closure = findClosure(type.parentScope)
  }

  compilerAssert(closure && closure instanceof Closure, "No method $name found for type $t", { name, t })
  return createCallAstFromValueAndPushValue(vm, closure, typeArgs, [receiver, ...args])

}
