import { BytecodeDefault, BytecodeSecondOrder, compileClassTask, compileFunctionPrototype, createBytecodeVmAndExecuteTask, pushBytecode, pushGeneratedBytecode, visitParseNode } from "./compiler";
import { BytecodeWriter, FunctionDefinition, Type, Binding, LetAst, Ast, StatementsAst, Scope, createScope, compilerAssert, VoidType, Vm, bytecodeToString, ParseIdentifier, ParseNode, CompiledFunction, AstRoot, isAst, pushSubCompilerState, ParseNil, createToken, ParseStatements, FunctionType, ParserFunctionDecl, Tuple, hashValues, TaskContext, GlobalCompilerState, isType, ParseNote, createAnonymousToken, textColors, CompilerError, PrimitiveType, CastAst, ExternalFunction, CallAst, IntType, Closure, UserCallAst, ExternalType, ParameterizedType, expectMap, ConcreteClassType, ClassDefinition, ParseTypeCheck, ParseCall, TypeVariable, TypeMatcher, typeMatcherEquals } from "./defs";
import { Task, TaskDef, Unit } from "./tasks";


export const insertFunctionDefinition = (compilerState: GlobalCompilerState, decl: ParserFunctionDecl) => {
  if (decl.id !== undefined) return compilerState.functionDefinitions[decl.id];

  decl.id = compilerState.functionDefinitions.length;
  const keywords = decl.keywords.map(x => x instanceof ParseNote ? x.expr.token.value : x.token.value)
  const inline = !!decl.anonymous || keywords.includes('inline')
  const funcDef = new FunctionDefinition(
    decl.id, decl.debugName,
    decl.name, decl.typeArgs, decl.args,
    decl.returnType, decl.body,
    inline)

  compilerState.functionDefinitions.push(funcDef);
  return funcDef;
}

export type TypeCheckHeaderArg = {
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  concreteTypes: Type[] // output
}

function compileAndExecuteFunctionHeaderTask(ctx: TaskContext, { func, args, typeArgs, parentScope, concreteTypes }: TypeCheckHeaderArg): Task<Unit, CompilerError> {

  compilerAssert(typeArgs.length <= func.typeArgs.length, "Expected $expected type parameters, got $got", { expected: func.typeArgs.length, got: typeArgs.length, func })
  compilerAssert(args.length === func.args.length, 'Expected $expected args got $got', { expected: func.args.length, got: args.length, func })

  if (func.args.length === 0) return Task.success;
  
  if (!func.headerPrototype) {

    func.headerPrototype = { name: `${func.debugName} header`, body: null!, initialInstructionTable: BytecodeDefault };
    func.headerPrototype.bytecode = { code: [], locations: [] }
    const out: BytecodeWriter = {
      bytecode: func.headerPrototype.bytecode,
      instructionTable: func.headerPrototype.initialInstructionTable,
      globalCompilerState: ctx.globalCompiler,
      state: { labelBlock: null }
    }
    // visitParseNode(out, func.headerPrototype.body);
    func.args.forEach(([name, type], i) => {
      if (type === null) return visitParseNode(out, new ParseNil(createAnonymousToken('')));
      visitParseNode(out, type)
      pushBytecode(out, type.token, { type: 'totype' });
    })

    pushGeneratedBytecode(out, { type: "tuple", count: func.args.length })
    pushGeneratedBytecode(out, { type: "halt" })

    ctx.globalCompiler.logger.log(textColors.cyan(`Compiled ${func.headerPrototype.name}`))
    ctx.globalCompiler.logger.log(bytecodeToString(func.headerPrototype.bytecode))
    ctx.globalCompiler.logger.log("")
  }

  const scope = createScope({}, undefined) // TODO: parent scope
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} header`, scope, lexicalParent: ctx.subCompilerState });
  ;(subCompilerState as any).location = func.name?.token.location
  subCompilerState.functionCompiler = subCompilerState

  func.args.forEach((arg, i) => {
    scope[arg[0].token.value] = args[i]
  })
  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier)
    if (!typeArgs[i]) scope[typeArg.token.value] = new TypeVariable(typeArg.token.value)
    else scope[typeArg.token.value] = typeArgs[i];
  })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.headerPrototype!.bytecode!, scope)
    .chainFn((task, compiledArgTypes) => {

      compilerAssert(compiledArgTypes instanceof Tuple, "Expected tuple")
      compiledArgTypes.values.forEach((type, i) => {
        if (type === null) {
          concreteTypes.push(args[i].type)
          return
        }
        if (type instanceof TypeMatcher) {
          const output = {}
          const matches = typeMatcherEquals(type, args[i].type, output)
          compilerAssert(matches, "Type check failed. Expected $expected got $got", { expected: type, got: args[i].type })
          concreteTypes.push(args[i].type) // TODO: Fill in the type for real
        } else {
          compilerAssert(isType(type));
          compilerAssert(args[i].type === type, "Argument $name of type $value does not match $expected", { name: func.args[i][0].token, value: args[i].type, expected: type })
          concreteTypes.push(type)
        }
      })
      return Task.success
    })
  )
}

export type TypeCheckAndCompileArg = {
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  concreteTypes: Type[]
}

export function functionTemplateTypeCheckAndCompileTask(ctx: TaskContext, { func, typeArgs, args, parentScope, concreteTypes }: TypeCheckAndCompileArg): Task<CompiledFunction, CompilerError> {

  const argBindings: Binding[] = [];

  compilerAssert(func.body)

  if (!func.templatePrototype)  {
    func.templatePrototype = { name: `${func.debugName} template bytecode`, body: func.body, initialInstructionTable: BytecodeSecondOrder };
    compileFunctionPrototype(ctx, func.templatePrototype);
  }
  compilerAssert(func.templatePrototype);

  const typeParamHash = hashValues(typeArgs)
  const existing = func.compiledFunctions.find(compiledFunc => {
    if (compiledFunc.typeParamHash === typeParamHash) {
      if (compiledFunc.typeParameters.every((x, i) => x === typeArgs[i])) return true
    }
  })
  if (existing) return Task.of(existing);

  const templateScope = createScope({}, undefined); // TODO: parent scope
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} template`, scope: templateScope, lexicalParent: ctx.subCompilerState });
  ;(subCompilerState as any).location = func.name?.token.location
  subCompilerState.functionCompiler = subCompilerState
  
  func.args.forEach(([iden, type], i) => {
    const binding = new Binding(iden.token.value, concreteTypes[i]);
    templateScope[iden.token.value] = binding;
    argBindings.push(binding);
  });
  
  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    templateScope[typeArg.token.value] = typeArgs[i];
  });

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.templatePrototype.bytecode!, templateScope)
    .chainFn((task, ast) => {

      const concreteTypes = []

      ctx.globalCompiler.logger.log(textColors.cyan(`Compiled template ${func.debugName}`))
      
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      const id = func.compiledFunctions.length;
      const binding = new Binding(`${func.debugName} compiled ${id}`, FunctionType);
      const returnType = ast.type;
      const compiledFunction = new CompiledFunction(
          binding, func, returnType, concreteTypes, ast, argBindings, typeArgs, typeParamHash);
      ctx.globalCompiler.compiledFunctions.set(binding, compiledFunction);
      func.compiledFunctions.push(compiledFunction)
      
      return Task.of(compiledFunction)
    })
  )

}

export type FunctionCallArg = {
  vm: Vm
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  concreteTypes: Type[]
}

function functionInlineTask(ctx: TaskContext, { vm, func, typeArgs, args, parentScope, concreteTypes }: FunctionCallArg): Task<Unit, CompilerError> {

  const argBindings: Binding[] = [];
  const statements: Ast[] = []
  const location = vm.location;

  compilerAssert(func.body)

  if (!func.templatePrototype)  {
    func.templatePrototype = { name: `${func.debugName} inline bytecode`, body: func.body, initialInstructionTable: BytecodeSecondOrder };
    compileFunctionPrototype(ctx, func.templatePrototype);
  }
  compilerAssert(func.templatePrototype);
  
  const templateScope = createScope({}, parentScope)
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} inline`, lexicalParent: ctx.subCompilerState, scope: templateScope })
  
  func.args.forEach(([iden, type], i) => {
    compilerAssert(concreteTypes[i], `Expected type`, { args, concreteTypes })
    const binding = new Binding(iden.token.value, concreteTypes[i]);
    templateScope[iden.token.value] = binding;
    statements.push(new LetAst(VoidType, location, binding, args[i]))
    argBindings.push(binding);
  });
  
  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    templateScope[typeArg.token.value] = typeArgs[i];
  });

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.templatePrototype.bytecode!, templateScope)
    .chainFn((task, ast) => {

      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      ctx.globalCompiler.logger.log(textColors.cyan(`Compiled inline ${func.debugName}`))
      vm.stack.push(new StatementsAst(ast.type, location, [...statements, ast]));
      
      return Task.success
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
  compilerAssert(args.length === func.args.length, "Expected $expected arguments got $got", { expected: func.args.length, got: func.args.length, func })

  if (!func.compileTimePrototype) 
    func.compileTimePrototype = { name: `${func.debugName} comptime bytecode`, body: func.body, initialInstructionTable: BytecodeDefault };
  compilerAssert(func.compileTimePrototype)

  const scope = Object.create(parentScope);
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} comptime`, lexicalParent: ctx.subCompilerState, scope })
  subCompilerState.functionCompiler = subCompilerState

  args.forEach((arg, i) => {
    scope[func.args[i][0].token.value] = arg;
  });
  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    scope[typeArg.token.value] = typeArgs[i];
  });
  compileFunctionPrototype(ctx, func.compileTimePrototype);

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, func.compileTimePrototype.bytecode!, scope)
    .chainFn((task, res) => { vm.stack.push(res); return Task.success })
  );
};

export function createCallAstFromValue(vm: Vm, value: unknown, typeArgs: unknown[], args: Ast[]): Task<Unit, CompilerError> {
  if (value instanceof PrimitiveType) {
    compilerAssert(args.length === 1 && typeArgs.length === 0, "Expected 1 arg got $count", { count: args.length })
    vm.stack.push(new CastAst(value, vm.location, args[0]))
    return Task.success
  }

  if (value instanceof ClassDefinition) {
    return (
      TaskDef(compileClassTask, { classDef: value, typeArgs }).
      chainFn((task, clssType) => {
        const constructor = expectMap(clssType.metaobject, 'constructor', "Expected constructor in metaobject for object $obj", { obj: value })
        return createCallAstFromValue(vm, constructor, [], args)
      })
    )
  }

  if (value instanceof ExternalFunction) {
    vm.stack.push(new CallAst(value.returnType, vm.location, value, args));
    return Task.success
  }

  if (value instanceof Closure) {

    const { func, scope: parentScope } = value
    const call: FunctionCallArg = { vm, func, typeArgs, args, parentScope, concreteTypes: [] }
    
    if (func.inline) return (
      TaskDef(compileAndExecuteFunctionHeaderTask, call)
      .chain(TaskDef(functionInlineTask, call))
    )
    return (
      TaskDef(compileAndExecuteFunctionHeaderTask, call)
      .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
      .chainFn((task, compiledFunction) => {
        const binding = compiledFunction.binding;
        const returnType = compiledFunction.returnType;
        vm.stack.push(new UserCallAst(returnType, vm.location, binding, args));
        return Task.success
      })
    )
  }

  compilerAssert(false, "Not supported value $value", { value })

}
