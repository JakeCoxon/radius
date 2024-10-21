import { BytecodeDefault, BytecodeSecondOrder, callFunctionFromValueTask, compileClassTask, compileFunctionPrototype, createBytecodeVmAndExecuteTask, pushBytecode, pushGeneratedBytecode, unknownToAst, visitParseNode, visitParseNodeAndError } from "./compiler";
import { externalBuiltinBindings, getEnumOf } from "./compiler_sugar";
import { getCommonType, hashValues, isTypeInteger, normalizeNumberType, numberTypeToConcrete, propagateLiteralType, propagatedLiteralAst, typeCheckAssert, typeMatcherEquals, typeCheckFunctionResult, tryMatchType, typeArgumentsToType } from "./compiler_types";
import { BytecodeWriter, FunctionDefinition, Type, Binding, LetAst, Ast, StatementsAst, Scope, createScope, compilerAssert, VoidType, Vm, bytecodeToString, ParseIdentifier, ParseNode, CompiledFunction, AstRoot, isAst, pushSubCompilerState, ParseNil, createToken, ParseStatements, FunctionType, ParserFunctionDecl, Tuple, TaskContext, GlobalCompilerState, isType, ParseNote, createAnonymousToken, textColors, CompilerError, PrimitiveType, CastAst, CallAst, IntType, Closure, UserCallAst, ParameterizedType, expectMap, ConcreteClassType, ClassDefinition, ParseCall, TypeVariable, TypeMatcher, SourceLocation, ExternalTypeConstructor, ScopeParentSymbol, SubCompilerState, CompilerFunction, IntLiteralType, FloatLiteralType, FloatType, RawPointerType, AddressAst, BindingAst, UnknownObject, NeverType, CompilerFunctionCallContext, CompileTimeObjectType, CompTimeObjAst, ParseString, NamedArgAst, TypeCheckResult, u8Type, TypeCheckVar, ParseFreshIden, NumberAst, BoolAst, createStatements, ExternalFunction, BlockAst, LabelBlock, ConstructorAst, VariantCastAst, EnumVariantAst, FunctionParameter, Capability } from "./defs";
import { Task, TaskDef, Unit } from "./tasks";


export const insertFunctionDefinition = (compilerState: GlobalCompilerState, decl: ParserFunctionDecl) => {
  const existing = compilerState.functionDefinitionsByDeclaration.get(decl)
  if (existing) return existing

  const id = compilerState.functionDefinitions.length;
  const keywords = decl.keywords.map(x => x instanceof ParseNote ? x.expr.token.value : x.token.value)
  const inline = !!decl.anonymous || keywords.includes('inline')
  const funcDef = new FunctionDefinition(
    id, decl.debugName,
    decl.name, decl.typeParams, decl.params,
    decl.returnType, decl.body,
    inline, decl.annotations)
  funcDef.variadic = decl.variadic
  funcDef.keywords.push(...keywords)
  compilerState.functionDefinitionsByDeclaration.set(decl, funcDef)

  if (funcDef.keywords.includes("external")) {
    compilerAssert(decl.name, "Expected name")
    funcDef.externalName = decl.name.token.value
  } 
  const ann = decl.annotations.find(x => x instanceof ParseCall && x.left instanceof ParseIdentifier && x.left.token.value === 'external')
  if (ann) {
      compilerAssert(ann instanceof ParseCall)
      compilerAssert(ann.args.length === 1 && ann.args[0] instanceof ParseString)
      funcDef.externalName = ann.args[0].string
    }

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

export function compileAndExecuteFunctionHeaderTask(ctx: TaskContext, { func, args, typeArgs, parentScope, result }: TypeCheckHeaderArg): Task<Unit, CompilerError> {

  typeCheckAssert(result, typeArgs.length <= func.typeParams.length, "Expected $expected type parameters, got $got", { expected: func.typeParams.length, got: typeArgs.length, func })
  typeCheckAssert(result, args.length >= func.params.length, 'Not enough params. Expected $expected args got $got', { expected: func.params.length, got: args.length, args, func })
  typeCheckAssert(result, func.variadic || args.length == func.params.length, 'Too many params. Expected $expected args got $got', { expected: func.params.length, got: args.length, args, func })

  // if (func.params.length === 0 && func.returnType === null) return Task.success()
  

  const scope = createScope({}, parentScope)
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} header`, scope, lexicalParent: ctx.subCompilerState });
  ;(subCompilerState as any).location = func.name?.token.location
  subCompilerState.functionCompiler = subCompilerState

  const sortedArgs: Ast[] = args.filter(x => !(x instanceof NamedArgAst))
  args.forEach((arg, i) => {
    if (arg instanceof NamedArgAst) {
      const namedParamIndex = func.params.findIndex(x => x.name.token.value === arg.name)
      typeCheckAssert(result, namedParamIndex > -1, "Unexpected function parameter named $name", { name: arg.name })
      typeCheckAssert(result, !sortedArgs[namedParamIndex], "Already specified parameter at index $namedParamIndex for argument $name", { namedParamIndex, name: arg.name })
      sortedArgs[namedParamIndex] = arg.expr
    }
  })
  result.sortedArgs = sortedArgs

  func.params.forEach((param, i) => {
    scope[param.name.token.value] = sortedArgs[i]
  })
  func.typeParams.forEach((typeParam, i) => {
    compilerAssert(typeParam instanceof ParseIdentifier)
    if (!typeArgs[i]) scope[typeParam.token.value] = new TypeVariable(typeParam.token.value)
    else scope[typeParam.token.value] = typeArgs[i];
  })

  if (!func.headerPrototype) {

    func.headerPrototype = { name: `${func.debugName} header`, body: null!, initialInstructionTable: BytecodeDefault, params: func.params };
    func.headerPrototype.bytecode = { code: [], locations: [] }
    const out: BytecodeWriter = {
      location: undefined!,
      bytecode: func.headerPrototype.bytecode,
      instructionTable: func.headerPrototype.initialInstructionTable,
      globalCompilerState: ctx.globalCompiler,
      state: { labelBlock: null, expansion: null }
    }
    // visitParseNode(out, func.headerPrototype.body);
    func.params.forEach(({ name, type }, i) => {
      if (type === null) return visitParseNodeAndError(out, new ParseNil(createAnonymousToken('')));
      visitParseNodeAndError(out, type)
      pushBytecode(out, type.token, { type: 'totype' })
    })

    pushGeneratedBytecode(out, { type: "tuple", count: func.params.length })
    if (func.returnType === null) visitParseNodeAndError(out, new ParseNil(createAnonymousToken('')))
    else {
      visitParseNodeAndError(out, func.returnType)
      pushBytecode(out, func.returnType.token, { type: 'totype' })
    }
    pushGeneratedBytecode(out, { type: "tuple", count: 2 })
    pushGeneratedBytecode(out, { type: "halt" })

    ctx.globalCompiler.logger.log(textColors.cyan(`Compiled ${func.headerPrototype.name}`))
    ctx.globalCompiler.logger.log(bytecodeToString(func.headerPrototype.bytecode))
    ctx.globalCompiler.logger.log("")
  }

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.headerPrototype!.bytecode!, scope)
    .chainFn((task, resultTuple) => {
      compilerAssert(resultTuple instanceof Tuple, "Expected tuple")
      let [compiledParamTypesTuple, returnType] = resultTuple.values
      compilerAssert(compiledParamTypesTuple instanceof Tuple, "Expected tuple")
      const compiledParamTypes = compiledParamTypesTuple.values
      returnType = returnType || VoidType
      
      typeCheckFunctionResult(result, compiledParamTypes, returnType)
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
    func.templatePrototype = { name: `${func.debugName} template bytecode`, body: func.body, initialInstructionTable: BytecodeSecondOrder, params: func.params };
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
    binding.storage = storage // TODO: Use Capability
    compilerAssert(storage !== 'ref', "Not implemented yet. use capability")
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
      
      const parameters = argBindings.map((argBinding, i) => {

        const capability = func.params[i].capability
        // @ParameterPassing
        const reference = !((capability === Capability.Let || capability === Capability.Sink)
          && argBinding.type instanceof PrimitiveType);
        const passingType = reference ? RawPointerType : argBinding.type;
        return new FunctionParameter(argBinding, argBinding.type, reference, passingType, capability);
      })
      const compiledFunction = new CompiledFunction(
          binding, func, returnType, result.concreteTypes, ast, argBindings, parameters, typeArgs, typeParamHash);
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

function functionInlineTask(ctx: TaskContext, { location, func, typeArgs, parentScope, lexicalParent, result }: FunctionCallArg): Task<Ast, CompilerError> {

  const argBindings: Binding[] = [];
  const statements: Ast[] = []

  compilerAssert(func.body, "Expected body to inline function", { func })

  if (!func.templatePrototype)  {
    func.templatePrototype = { name: `${func.debugName} inline bytecode`, body: func.body, initialInstructionTable: BytecodeSecondOrder, params: func.params };
    compileFunctionPrototype(ctx, func.templatePrototype);
  }
  compilerAssert(func.templatePrototype);
  
  const inlineInto = ctx.subCompilerState
  const templateScope = createScope({}, parentScope)
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} inline`, lexicalParent, scope: templateScope })
  const breakBlock = subCompilerState.functionReturnBreakBlock = new LabelBlock(null, "return", 'break', new Binding('returnBreak', NeverType))
  subCompilerState.inlineIntoCompiler = inlineInto
  subCompilerState.labelBlock = lexicalParent.labelBlock
  subCompilerState.nextLabelBlockDepth = inlineInto.nextLabelBlockDepth
  subCompilerState.functionCompiler = subCompilerState // functionCompiler is used to set locals etc

  const { concreteTypes } = result

  const args = result.sortedArgs
  
  func.params.forEach(({ name, type, storage, capability }, i) => {
    compilerAssert(concreteTypes[i], `Expected type`, { func, args, concreteTypes })
    compilerAssert(concreteTypes[i] !== IntLiteralType)
    const arg = args[i]
    const nameValue = name instanceof ParseFreshIden ? name.freshBindingToken.identifier : name.token.value
    
    if (arg instanceof CompTimeObjAst) { // Special case compile time objects
      templateScope[nameValue] = arg.value
      return
    }
    let isSimpleObj = arg instanceof BindingAst || arg instanceof AddressAst || arg instanceof NumberAst || arg instanceof BoolAst
    if (isSimpleObj) {
      propagateLiteralType(concreteTypes[i], arg)
      templateScope[nameValue] = arg
      return
    }
    const binding = new Binding(nameValue, concreteTypes[i])
    binding.storage = storage // is this right for inline?
    compilerAssert(storage !== 'ref', "Not implemented yet")
    binding.definitionCompiler = inlineInto
    templateScope[nameValue] = binding
    propagateLiteralType(concreteTypes[i], arg)
    // TODO: This breaks sink params stuff. think about having another capability to allow sinking
    // TODO: Handle inout params differently so we don't sink by default
    const mutable = capability === Capability.Sink || capability === Capability.Inout
    // compilerAssert(capability !== Capability.Inout, "Not implemented inlining an inout parameter", { name, func: func.debugName, capability })
    statements.push(new LetAst(VoidType, location, binding, args[i], mutable))
    argBindings.push(binding)
  });
  
  func.typeParams.forEach((typeParam, i) => {
    compilerAssert(typeParam instanceof ParseIdentifier, "Not implemented")
    let typeArg = typeArgs[i]
    if (typeArg === undefined) typeArg = result.substitutions[typeParam.token.value]
    compilerAssert(typeArg !== undefined, "Type arg not found $name", { name: typeParam.token.value, typeArgs })
    templateScope[typeParam.token.value] = typeArg
  });

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.templatePrototype.bytecode!, templateScope)
    .chainFn((task, ast) => {
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      // ctx.globalCompiler.logger.log(textColors.cyan(`Compiled inline ${func.debugName}`))
      // TODO: This is basically what endblockast instruction does. can we just emit block instructions?
      let type = propagatedLiteralAst(ast).type
      if (breakBlock.didBreak) {
        type = getCommonType([type, ...breakBlock.breaks.map(x => x.expr!.type)])
      }
      const stmts = createStatements(location, [...statements, ast])
      const breakExprBinding = breakBlock.breakWithExpr ? new Binding('breakExpr', type) : null
      return Task.of(new BlockAst(type, location, breakBlock.binding!, breakExprBinding, stmts))
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
    func.compileTimePrototype = { name: `${func.debugName} comptime bytecode`, body: func.body, initialInstructionTable: BytecodeDefault, params: func.params };
  compilerAssert(func.compileTimePrototype)

  const scope = createScope({}, parentScope)
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} comptime`, lexicalParent: ctx.subCompilerState, scope })
  subCompilerState.functionCompiler = subCompilerState

  args.forEach((arg, i) => {
    const name_ = func.params[i].name
    const name = name_ instanceof ParseFreshIden ? name_.freshBindingToken.identifier : name_.token.value
    scope[name] = arg;
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
  const ctx: CompilerFunctionCallContext = { location: vm.location, compilerState: vm.context.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
  return (
    createCallAstFromValue(ctx, value, typeArgs, args)
    .chainFn((task, ast) => {vm.stack.push(ast); return Task.success() })
  )
}

export function createCallAstFromValue(ctx: CompilerFunctionCallContext, value: unknown, typeArgs: unknown[], args: Ast[]): Task<Ast, CompilerError> {
  const location = ctx.location 
  compilerAssert(location, "Expected location. This function is not a TaskDef task!!!")
  if (value instanceof PrimitiveType) {
    compilerAssert(args.length === 1 && typeArgs.length === 0, "Expected 1 arg got $count", { count: args.length })
    const ast = propagatedLiteralAst(args[0])
    // Special case for zero
    if ((isTypeInteger(value) || value === RawPointerType) && ast instanceof NumberAst && ast.value === 0) {
      return Task.of(new NumberAst(value, location, 0))
    }
    if (ast.type === value) return Task.of(ast)
    return Task.of(new CastAst(value, location, propagatedLiteralAst(args[0])))
  }

  // Resolve ClassDefinitions into concrete types before instantiating any classes or functions
  if (typeArgs.some(x => x instanceof ClassDefinition)) {
    return (
      typeArgumentsToType(typeArgs).chainFn((task, compiledTypeArgs) => 
        createCallAstFromValue({...ctx}, value, compiledTypeArgs, args) // Call self
      )
    )
  }

  if (value instanceof ClassDefinition) {
    return (
      TaskDef(compileClassTask, { classDef: value, typeArgs }).
      chainFn((task, clssType) => {
        const constructor = expectMap(clssType.typeInfo.metaobject, 'constructor', "Expected constructor in metaobject for object $obj", { obj: value })
        return createCallAstFromValue({...ctx}, constructor, [], args)
      })
    )
  }

  if (value instanceof CompilerFunction) {
    return value.func(ctx, typeArgs, args)
  }

  if (value instanceof Closure) {
    
    const { func, scope: parentScope, lexicalParent } = value
    const typeCheckResult: TypeCheckResult = { func, concreteTypes: [], substitutions: {}, returnType: undefined!, sortedArgs: [], checkFailed: false }
    ctx.typeCheckResult = typeCheckResult
    const call: FunctionCallArg = { location, func, typeArgs, args, parentScope, lexicalParent, result: typeCheckResult }

    if (func.externalName !== undefined) {
      const externalName = func.externalName
      return (
        TaskDef(compileAndExecuteFunctionHeaderTask, call)
        .chainFn((task, headerResult) => { 
          const ctx = (task._context as TaskContext)
          const existing = ctx.globalCompiler.externalDefinitions.find(x => x.name === externalName)
          const paramHash = hashValues(call.result.concreteTypes)
          compilerAssert(!existing || existing.paramHash === paramHash, "Function exists with different param hash", { existing })
          const binding = existing?.binding ?? externalBuiltinBindings[externalName] ?? new Binding(externalName, FunctionType)

          if (!existing) ctx.globalCompiler.externalDefinitions.push({ name: externalName, binding, paramHash, paramTypes: call.result.concreteTypes, returnType: call.result.returnType })
          compilerAssert(call.result.returnType, "Expected return type got $returnType", { returnType: call.result.returnType })
          const mappedArgs = call.result.sortedArgs.map((ast, i) => {
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
        const mappedArgs = call.result.sortedArgs.map((ast, i) => {
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

  if (value instanceof ExternalTypeConstructor) {
    // TOOD: Do this properly
    const types = typeArgs.length ? typeArgs : args.map(x => x.type)
    return (
      TaskDef(callFunctionFromValueTask, ctx.compilerState.vm, value, types, [])
      .chainFn((task, value) => {
        const type = ctx.compilerState.vm.stack.pop()
        compilerAssert(isType(type), "Expected type got $type", { type })
        if (type.typeInfo.metaobject.isEnumVariant) {
          return (
            getEnumOf(ctx.compilerState.globalCompiler, type)
            .chainFn((task, enumVariantOf) => {
              const variantIndex = type.typeInfo.metaobject.enumVariantIndex
              compilerAssert(typeof variantIndex === 'number', "Expected number", { type, meta: type.typeInfo.metaobject })
              const num = new NumberAst(IntType, location, variantIndex)
              args.forEach(x => propagatedLiteralAst(x))
              // TODO: Properly check arg types
              const cast = new EnumVariantAst(enumVariantOf, location, type, enumVariantOf, [num, ...args])
              return Task.of(cast)
            })
          )
        }
        compilerAssert(false, "Not implemented", { type })
      })
    )
  }

  compilerAssert(false, "Not supported value $value", { value, typeArgs, args })

}

export const compileExportedFunctionTask = (ctx: TaskContext, { exportName, closure } : { exportName?: string, closure: Closure }): Task<CompiledFunction, CompilerError> => {
  // TODO: Handle functions with arguments
  const { func, lexicalParent, scope } = closure
  const typeCheckResult: TypeCheckResult = { func, concreteTypes: [], substitutions: {}, returnType: undefined!, sortedArgs: [], checkFailed: false }
  const call: FunctionCallArg = { location: SourceLocation.anon, func, typeArgs: [], args: [], parentScope: scope, lexicalParent, result: typeCheckResult }

  return (
    TaskDef(compileAndExecuteFunctionHeaderTask, call)
    .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
    .chainFn((task, compiledFunction) => {
      if (exportName) {
        compilerAssert(!ctx.globalCompiler.exports[exportName], "Already exported '$exportName'", { exportName })
        ctx.globalCompiler.exports[exportName] = compiledFunction
      }
      return Task.of(compiledFunction)
    })
  )
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

  compilerAssert(closure && closure instanceof Closure, "No method $name found for type $type", { name, t: receiver.type, receiver })
  return createCallAstFromValueAndPushValue(vm, closure, typeArgs, [receiver, ...args])

}
