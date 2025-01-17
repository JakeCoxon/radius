import { createDefaultConstructorAst } from "./codegen_ast";
import { compileExportedFunctionTask, insertFunctionDefinition } from "./compiler_functions";
import { createCompilerModuleTask, defaultMetaFunction } from "./compiler_sugar";
import { hashValues, typeTableGetOrInsert } from "./compiler_types";
import { BytecodeDefault, BytecodeSecondOrder, compileFunctionPrototype, createBytecodeVmAndExecuteTask, pushGeneratedBytecode, visitParseNodeAndError } from "./compiler_vm";
import { Binding, BytecodeWriter, ClassDefinition, Closure, CompiledClass, CompiledFunction, CompilerError, CompilerFunctionCallContext, ConcreteClassType, ExternalFunction, ExternalTypeConstructor, FunctionType, IntType, LetAst, Module, NumberAst, ParameterizedType, ParseBlock, ParseCall, ParseIdentifier, ParseImport, ParseLet, ParseLetConst, ParseNode, ParseNote, ParseStatements, ParsedModule, ParserClassDecl, ParserFunctionDecl, Scope, ScopeEventsSymbol, ScopeParentSymbol, SetAst, SourceLocation, StatementsAst, TaskContext, Type, TypeFieldDef, TypeInfo, UserCallAst, VoidType, bytecodeToString, compilerAssert, createAnonymousToken, createCompilerError, createScope, createStatements, insertTypeInfoFields, isAst, pushSubCompilerState, textColors } from "./defs";
import { Event, Task, TaskDef } from "./tasks";


export function resolveScope(ctx: TaskContext, scope: Scope, name: string): Task<unknown, CompilerError> {
  let checkScope: Scope | undefined = scope;
  while (checkScope) {
    if (checkScope[name] !== undefined) return Task.of(checkScope[name])
    checkScope = (checkScope as any)[ScopeParentSymbol]
  }

  // TODO: This should attach events to every ancestor in the scope chain
  if (!scope[ScopeEventsSymbol]) scope[ScopeEventsSymbol] = {}
  if (!scope[ScopeEventsSymbol][name]) scope[ScopeEventsSymbol][name] = new Event<string, CompilerError>()
  ctx.globalCompiler.allWaitingEvents.push(scope[ScopeEventsSymbol][name])
  return Task.waitFor(scope[ScopeEventsSymbol][name]).mapRejected((error) => {
    return createCompilerError('Binding $name not found in scope', { name })
  })
}


export const setScopeValueAndResolveEvents = (scope: Scope, name: string, value: unknown) => {
  // TODO: Make sure metagenerated names don't shadow something because this won't work
  // To do that might be difficult because the shadow might compile before the thing being shadowed.
  // Can we enforce outer scopes are compiled first?
  scope[name] = value;
  const events = scope[ScopeEventsSymbol];
  if (events && events[name]) {
    (events[name] as Event<unknown, unknown>).success(value);
    delete events[name]
  }
}


export function compileClassTask(ctx: TaskContext, { classDef, typeArgs }: { classDef: ClassDefinition, typeArgs: unknown[] }): Task<ConcreteClassType | ParameterizedType, CompilerError> {
  const binding = new Binding(classDef.debugName, VoidType);
  compilerAssert(typeArgs.length === classDef.typeArgs.length, "Expected $x type parameters for class $classDef, got $y", { x: classDef.typeArgs.length, y: typeArgs.length, classDef })

  if (!classDef.templatePrototype)  {
    compilerAssert(classDef.body, "Expected class body");
    const bodyNode = classDef.body instanceof ParseBlock ? classDef.body.statements : classDef.body
    classDef.templatePrototype = { name: `${classDef.debugName} class template bytecode`, body: bodyNode, initialInstructionTable: BytecodeSecondOrder, params: [] }; 
    compileFunctionPrototype(ctx, classDef.templatePrototype);
  }

  const typeParamHash = hashValues(typeArgs, { classDef })
  const existing = classDef.compiledClasses.find(compiledClass => {
    if (compiledClass.typeArgHash === typeParamHash) {
      if (compiledClass.typeArguments.every((x, i) => x === typeArgs[i])) return true
    }
  })
  if (existing) return Task.of(existing.type);

  const templateScope = Object.create(classDef.parentScope);
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${classDef.debugName} class template`, lexicalParent: ctx.subCompilerState, scope: templateScope })
  subCompilerState.functionCompiler = subCompilerState; // Consider class as the functionCompiler - could be renamed something else

  compilerAssert(!typeArgs.some(x => x instanceof ClassDefinition), "Programmer error. Type args must have already been compiled by this point", { typeArgs })
  classDef.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    templateScope[typeArg.token.value] = typeArgs[i];
  });

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, classDef.templatePrototype!.bytecode!, templateScope)
    .chainFn((task, ast) => {
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      const debugName = typeArgs.length === 0 ? classDef.debugName :
        `${classDef.debugName}!(...)`
      const compiledClass = new CompiledClass(
          classDef.location, debugName,
          binding, classDef, null!, null as any, [], typeArgs, typeParamHash)

      const typeInfo: TypeInfo = { sizeof: 0, alignment: 0, fields: compiledClass.fields, metaobject: compiledClass.metaobject, isReferenceType: true }
      let type: Type
      if (classDef.typeArgs.length === 0) { 
        type = new ConcreteClassType(compiledClass, typeInfo)
        classDef.concreteType = type;
      } else {
        type = typeTableGetOrInsert(ctx.globalCompiler.typeTable, new ParameterizedType(classDef, typeArgs, typeInfo))
      }
      compilerAssert(type instanceof ConcreteClassType || type instanceof ParameterizedType, "Expected concrete class type or parameterized type", { type })
      compiledClass.type = type;
      binding.type = type;

      const fieldDefs: TypeFieldDef[] = []
      for (const name of Object.getOwnPropertyNames(templateScope)) {
        if (templateScope[name] instanceof Binding)
          fieldDefs.push({ sourceLocation: SourceLocation.anon, name, fieldType: templateScope[name].type })
      }
      insertTypeInfoFields(type, fieldDefs)

      classDef.compiledClasses.push(compiledClass)

      const returnType = type;
      const definitionScope = classDef.parentScope
      
      if (classDef.metaClass) {
        return (
          TaskDef(resolveScope, classDef.parentScope, classDef.metaClass.token.value)
          .chainFn((task, func) => {
            if (func instanceof ExternalFunction) {
              const fnctx: CompilerFunctionCallContext = { location: SourceLocation.anon, compilerState: ctx.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
              func.func(fnctx, [compiledClass])
            } else compilerAssert(false, "Not implemented yet", { func })

            return (
              defaultMetaFunction(subCompilerState, compiledClass, definitionScope, templateScope)
              .chainFn(() => Task.of(returnType))
            )
          })
        )
      }

      return (
        defaultMetaFunction(subCompilerState, compiledClass, definitionScope, templateScope)
        .chainFn(() => Task.of(returnType))
      )

    })
  )
  
}


function topLevelFunctionDefinitionTask(ctx: TaskContext, funcDecl: ParserFunctionDecl, scope: Scope) {
  const funcDef = insertFunctionDefinition(ctx.globalCompiler, funcDecl)

  if (funcDef.keywords.includes('method')) {
    compilerAssert(funcDecl.params[0]?.name, "Expected type for first argument")
    let t = funcDecl.params[0].type!;
    const type = t instanceof ParseCall ? t.left : t
    return (
      TaskDef(resolveScope, scope, type.token.value)
      .chainFn((task, result) => {
        let methods = ctx.globalCompiler.methods.get(scope)
        if (!methods) { methods = []; ctx.globalCompiler.methods.set(scope, methods) }
        compilerAssert(result instanceof ClassDefinition || result instanceof ExternalTypeConstructor, "Expected class definition or type constructor, got $result", { result })

        methods.push([result, new Closure(funcDef, scope, ctx.subCompilerState)])
        return Task.success()
      })
    )
    
  }

  compilerAssert(!Object.hasOwn(scope, funcDef.name!.token.value), "$name already in scope", { name: funcDef.name!.token.value, value: scope[funcDef.name!.token.value] })

  setScopeValueAndResolveEvents(scope, funcDef.name!.token.value, new Closure(funcDef, scope, ctx.subCompilerState))

  return Task.success()
}
function topLevelClassDefinitionTask(ctx: TaskContext, decl: ParserClassDecl, scope: Scope) {

  const g = ctx.globalCompiler
  if (decl.id !== undefined) return Task.success();

  decl.id = g.functionDefinitions.length;
  const classDef = new ClassDefinition(
    decl.id, decl.token.location, scope, decl.debugName,
    decl.name, decl.typeArgs, decl.body)
  classDef.metaClass = decl.metaType
  const keywords = decl.keywords.map(x => x instanceof ParseNote ? x.expr.token.value : x.token.value)
  classDef.keywords.push(...keywords)

  g.classDefinitions.push(classDef);

  setScopeValueAndResolveEvents(scope, decl.name!.token.value, classDef)

  return Task.success()

}

const topLevelLetConst = (ctx: TaskContext, expr: ParseLetConst, rootScope: Scope) => {
  const out: BytecodeWriter = {
    location: expr.token.location,
    bytecode: { code: [], locations: [] },
    instructionTable: BytecodeDefault,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNodeAndError(out, expr.value);
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan("Compiled top level let const"))
  ctx.globalCompiler.logger.log(bytecodeToString(out.bytecode))
  ctx.globalCompiler.logger.log("")

  const subCompilerState = pushSubCompilerState(ctx, { debugName: 'top level const', lexicalParent: ctx.subCompilerState, scope: ctx.subCompilerState.scope })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, out.bytecode, rootScope)
    .chainFn((task, result) => {
      setScopeValueAndResolveEvents(rootScope, expr.name.token.value, result)
      return Task.success()
    })
  );
}

const topLevelLet = (ctx: TaskContext, expr: ParseLet, moduleScope: Scope) => {
  if (!expr.value) return Task.success()

  const out: BytecodeWriter = {
    location: expr.token.location,
    bytecode: { code: [], locations: [] },
    instructionTable: BytecodeSecondOrder,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNodeAndError(out, expr)
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan("Compiled top level let"))
  ctx.globalCompiler.logger.log(bytecodeToString(out.bytecode))
  ctx.globalCompiler.logger.log("")

  const subCompilerState = pushSubCompilerState(ctx, { debugName: 'top level let', lexicalParent: ctx.subCompilerState, scope: ctx.subCompilerState.scope })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, out.bytecode, moduleScope)
    .chainFn((task, result) => {
      compilerAssert(result instanceof LetAst, "Expected let ast")
      ctx.globalCompiler.globalLets.push(result)
      ctx.globalCompiler.globalVars.set(result.binding, {
        binding: result.binding,
        initializer: result.value,
        letType: expr.letType,
        location: expr.token.location,
        type: result.value?.type ?? VoidType,
        register: "",
      })
      return Task.success()
    })
  );
}

export const loadModule = (ctx: TaskContext, location: SourceLocation, moduleName: string): Task<Module, CompilerError> => {
  const loader = ctx.globalCompiler.moduleLoader
  if (loader.cache[moduleName]) return Task.of(loader.cache[moduleName])
  const rootScope = ctx.globalCompiler.rootScope
  const parsedModule = loader.loadModule(moduleName)
  const moduleScope = createScope({ ...rootScope }, undefined)

  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${moduleName} module`, lexicalParent: undefined, scope: moduleScope })
  subCompilerState.moduleCompiler = subCompilerState
  ;(subCompilerState as any).location = location

  return (
    TaskDef(runTopLevelTask, parsedModule.rootNode, moduleScope)
    .chainFn((task, _) => {
      const module = new Module(moduleName, subCompilerState, parsedModule)
      loader.cache[moduleName] = module
      return Task.of(module)
    }) as Task<Module, CompilerError>
  )
}

export const importModule = (ctx: TaskContext, importNode: ParseImport, existingScope: Scope) => {
  const moduleName = importNode.module.token.value
  const loader = ctx.globalCompiler.moduleLoader

  const expandIntoScope = (module: Module) => {
    if (importNode.imports.length === 0) {
      const newName = importNode.rename ? importNode.rename.token.value : importNode.module.token.value
      setScopeValueAndResolveEvents(existingScope, newName, module)
    }

    const tasks = importNode.imports.map((importName) => {
      const originalName = importName.token.value
      const newName = importName.rename ? importName.rename.token.value : originalName
      const insertIntoScope = (task: unknown, result: unknown) => {
        setScopeValueAndResolveEvents(existingScope, newName, result)
        return Task.success()
      }
      return TaskDef(resolveScope, module.compilerState.scope, originalName).chainFn(insertIntoScope)
    })
    
    return Task.concurrency(tasks)
  }
  return (
    TaskDef(loadModule, importNode.token.location, moduleName)
    .chainFn((task, module) => {
      ctx.subCompilerState = loader.cache[moduleName].compilerState
      return expandIntoScope(module)
    })
  )
}

export const topLevelComptimeTask = (ctx: TaskContext, expr: ParseNode, moduleScope: Scope) => {
  const out: BytecodeWriter = {
    location: expr.token.location,
    bytecode: { code: [], locations: [] },
    instructionTable: BytecodeDefault,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNodeAndError(out, expr)
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan("Compiled top level comptime"))
  ctx.globalCompiler.logger.log(bytecodeToString(out.bytecode))
  ctx.globalCompiler.logger.log("")

  const subCompilerState = pushSubCompilerState(ctx, { debugName: 'top level comptime', lexicalParent: ctx.subCompilerState, scope: moduleScope })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, out.bytecode, moduleScope)
  );

}

export const runTopLevelTask = (ctx: TaskContext, stmts: ParseStatements, moduleScope: Scope) => {
  const tasks: Task<unknown, CompilerError>[] = []

  const copyEverythingIntoScope = (module: Module) => {
    Object.getOwnPropertyNames(module.compilerState.scope).forEach((k) => {
      const v = module.compilerState.scope[k]
      // TODO: All of rootScope is also copied here
      setScopeValueAndResolveEvents(moduleScope, k, v)
    })

    if (ctx.globalCompiler.methods.get(module.compilerState.scope)) {
      const methods = [...ctx.globalCompiler.methods.get(module.compilerState.scope)!]
      ctx.globalCompiler.methods.set(moduleScope, methods)
    }
  }
  const loader = ctx.globalCompiler.moduleLoader
  if (loader.cache["_preload"] && loader.cache["_preload"].compilerState.scope != moduleScope)
    copyEverythingIntoScope(loader.cache['_preload'])
  
  stmts.exprs.forEach(node => {
    if (node.key === 'import') {
      tasks.push(TaskDef(importModule, node, moduleScope));
    } else if (node.key === 'letconst') {
      tasks.push(TaskDef(topLevelLetConst, node, moduleScope));
    } else if (node.key === 'let') {
      tasks.push(TaskDef(topLevelLet, node, moduleScope));
    } else if (node.key === 'function') {
      tasks.push(TaskDef(topLevelFunctionDefinitionTask, node.functionDecl, moduleScope ));
    } else if (node.key === 'class') {
      tasks.push(TaskDef(topLevelClassDefinitionTask, node.classDecl, moduleScope ));
    } else if (node.key === 'comptime') {
      tasks.push(TaskDef(topLevelComptimeTask, node.expr, moduleScope ));
    } else {
      compilerAssert(false, `Not supported at top level $key`, { key: node.key })
    }
  })

  return Task.concurrency(tasks);
}

const createInitializerFunctionTask = (ctx: TaskContext) => {
  const decl: ParserFunctionDecl = {
    debugName: `<initializer>`,
    token: createAnonymousToken(''), functionMetaName: null, name: null, typeParams: [], params: [],
    keywords: [], anonymous: true, returnType: null, body: null, annotations: [], variadic: false
  }
  const func = insertFunctionDefinition(ctx.globalCompiler, decl)

  // Map initializers 
  const lets = ctx.globalCompiler.globalLets.map(globalLet => {
    const value = globalLet.value || createDefaultConstructorAst(globalLet.binding.type, globalLet.location)
    return new SetAst(VoidType, globalLet.location, globalLet.binding, value)
  })
  const ast = new StatementsAst(VoidType, SourceLocation.anon, [...lets])

  const binding = ctx.globalCompiler.initializerFunctionBinding
  const compiledFunction = new CompiledFunction(
      binding, func, VoidType, [], ast, [], [], [], 0)
  ctx.globalCompiler.compiledFunctions.set(binding, compiledFunction)
  func.compiledFunctions.push(compiledFunction)
  ctx.globalCompiler.initializerFunction = compiledFunction

  return Task.success()
}

const createEntryFunctionTask = (ctx: TaskContext) => {
  const decl: ParserFunctionDecl = {
    debugName: `<entry>`,
    token: createAnonymousToken(''), functionMetaName: null, name: null, typeParams: [], params: [],
    keywords: [], anonymous: true, returnType: null, body: null, annotations: [], variadic: false
  }
  const func = insertFunctionDefinition(ctx.globalCompiler, decl)

  compilerAssert(ctx.globalCompiler.mainFunction, "Expected main function")

  const return_ = ctx.globalCompiler.mainFunction.returnType === VoidType ? 
    [new NumberAst(IntType, SourceLocation.anon, 0)] : []

  // Call initializer and main
  const ast = createStatements(SourceLocation.anon, [
    new UserCallAst(VoidType, SourceLocation.anon, ctx.globalCompiler.initializerFunctionBinding, []),
    new UserCallAst(VoidType, SourceLocation.anon, ctx.globalCompiler.mainFunction.binding, []),
    ...return_,
  ])

  const id = func.compiledFunctions.length
  const binding = new Binding(`${func.debugName} compiled ${id}`, FunctionType)
  const compiledFunction = new CompiledFunction(
      binding, func, IntType, [], ast, [], [], [], 0)
  ctx.globalCompiler.compiledFunctions.set(binding, compiledFunction)
  func.compiledFunctions.push(compiledFunction)
  ctx.globalCompiler.entryFunction = compiledFunction
  compilerAssert(!ctx.globalCompiler.exports['main'], "Already got main export")
  ctx.globalCompiler.exports['main'] = compiledFunction

  return Task.success()
}

export const programEntryTask = (ctx: TaskContext, entryModule: ParsedModule): Task<unknown, CompilerError> => {
  const moduleScope = ctx.subCompilerState.scope

  return (
    TaskDef(createCompilerModuleTask)
    .chainFn((task, compilerModule) => {
      ctx.globalCompiler.moduleLoader.cache['compiler'] = compilerModule
      return TaskDef(loadModule, SourceLocation.anon, '_preload')
    })
    .chainFn(() => {
      return TaskDef(runTopLevelTask, entryModule.rootNode, moduleScope)
    })
    .chainFn((task, arg) => {

      const tasks: Task<unknown, CompilerError>[] = []
      
      if (moduleScope['main']) {
        compilerAssert(moduleScope['main'] instanceof Closure, "Expected main to be callable")

        const task = TaskDef(compileExportedFunctionTask, { closure: moduleScope['main'] })
          .chainFn((task, compiledFunction) => {
            ctx.globalCompiler.mainFunction = compiledFunction
            return TaskDef(createEntryFunctionTask)
          })
        
        tasks.push(task)
      }
      Object.values(moduleScope).forEach(value => {
        if (value instanceof Closure && value.func.keywords.includes("export")) {
          const exportName = value.func.externalName = value.func.debugName // TODO: Don't call this debugName
          tasks.push(TaskDef(compileExportedFunctionTask, { exportName, closure: value }))
        }
      })
      compilerAssert(tasks.length, "No 'main' and no exported function found")
      return Task.concurrency(tasks)
    })
    .chainFn(() => {
      return TaskDef(createInitializerFunctionTask)
    })
  )
  
}
