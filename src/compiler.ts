import { isParseVoid, BytecodeOut, FunctionDefinition, Type, Binding, LetAst, UserCallAst, CallAst, Ast, NumberAst, OperatorAst, SetAst, OrAst, AndAst, ListAst, IfAst, StatementsAst, Scope, createScope, Closure, ExternalFunction, compilerAssert, VoidType, IntType, FunctionPrototype, Vm, MetaInstructionTable, Token, expect, createStatements, DoubleType, FloatType, StringType, expectMap, bytecodeToString, ParseCall, ParseIdentifier, ParseAst, CompiledFunction, AstRoot, isAst, pushSubCompilerState, addFunctionDefinition, ParseNil, createToken, ParseStatements, FunctionType, StringAst, WhileAst, BoolAst, BindingAst, SourceLocation, BytecodeInstr, ReturnAst, BytecodeGen, ParserFunctionDecl, ScopeEventsSymbol, BoolType, Tuple, ParseTuple, hashValues, TaskContext, ParseElse, ParseIf, InstructionMapping, GlobalCompilerState } from "./defs";
import { Event, Task, createTaskDef, isTask, isTaskResult } from "./tasks";

const pushBytecode = (out: BytecodeOut, token: Token, instr: BytecodeInstr) => {
  out.bytecode.locations.push(token.location);
  out.bytecode.code.push(instr)
}

const writeBytecode = (out: BytecodeOut, expr: ParseAst) => {
  compilerAssert(expr.key, "$expr not found", { expr })
  const table = out.table
  const instrWriter = expectMap(table, expr.key, "Not implemented parser node $key")
  instrWriter(out, expr)
}
const writeAll = (out: BytecodeOut, exprs: ParseAst[]) => {
  exprs.forEach(expr => writeBytecode(out, expr))
}
const writeMeta = (out: BytecodeOut, expr: ParseAst) => {
  writeBytecode({ bytecode: out.bytecode, table: bytecodeDefault, globalCompilerState: out.globalCompilerState }, expr)
}

const bytecodeDefault: MetaInstructionTable = {
  identifier: (out, ast) => pushBytecode(out, ast.token, { type: "binding", name: ast.token.value }), // prettier-ignore
  number:  (out, ast) => pushBytecode(out, ast.token, { type: "push", value: Number(ast.token.value) }), // prettier-ignore
  name:    (out, ast) => pushBytecode(out, ast.token, { type: "name", name: ast.token.value }), // prettier-ignore
  string:  (out, ast) => pushBytecode(out, ast.token, { type: "push", value: ast.token.value }), // prettier-ignore
  nil:     (out, ast) => pushBytecode(out, ast.token, { type: "push", value: null }), // prettier-ignore
  boolean: (out, ast) => pushBytecode(out, ast.token, { type: "push", value: ast.token.value !== 'false' }), // prettier-ignore

  operator: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operator', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  set:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'set', name: ast.name })), // prettier-ignore
  letconst: (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'let', name: ast.name })), // prettier-ignore
  meta:     (out, ast) => (writeBytecode(out, ast.expr)),
  comptime: (out, ast) => (writeBytecode(out, ast.expr)),

  list:  (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'list', count: ast.exprs.length })),
  tuple: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'tuple', count: ast.exprs.length })),
  
  let: (out, ast) => {
    compilerAssert(ast.value, "Not implemented yet");
    compilerAssert(ast.type, "Not implemented yet");
    (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'let', name: ast.name })) // prettier-ignore
  },

  function: (out, ast) => {
    pushBytecode(out, ast.token, { type: "closure", id: addFunctionDefinition(out.globalCompilerState, ast.functionDecl).id }) // prettier-ignore
  },
  call:    (out, ast) => {
    compilerAssert(ast.typeArgs.length === 0, "Not implemented")
    writeAll(out, ast.args);
    if (ast.left instanceof ParseIdentifier) {
      pushBytecode(out, ast.token, { type: "call", name: ast.left.token.value, count: ast.args.length }); // prettier-ignore
      return;
    }
    compilerAssert(false, "Call with non-identifier not implemented yet")
  },
  return: (out, ast) => {
    if (ast.expr) writeBytecode(out, ast.expr);
    pushBytecode(out, ast.token, { type: 'return', r: !!ast.expr })
  },

  and: (out, ast) => {
    writeAll(out, ast.exprs)
    pushBytecode(out, ast.token, { type: "and", count: ast.exprs.length })
  },
  or: (out, ast) => {
    writeAll(out, ast.exprs)
    pushBytecode(out, ast.token, { type: "or", count: ast.exprs.length })
  },

  statements: (out, ast) => {
    ast.exprs.forEach((stmt, i) => {
      writeBytecode(out, stmt);
      if (i !== ast.exprs.length - 1) pushBytecode(out, ast.token, { type: "pop" });
    });
  },
  
  else: (out, ast) => writeBytecode(out, ast.body),

  if: (out, ast) => {
    writeBytecode(out, ast.condition);
    const jump1 = { type: "jumpf", address: 0 };
    pushBytecode(out, ast.condition.token, jump1);
    writeBytecode(out, ast.trueBody);
    if (ast.falseBody) {
      const jump2 = { type: "jump", address: 0 };
      pushBytecode(out, ast.trueBody.token, jump2);
      jump1.address = out.bytecode.code.length;
      writeBytecode(out, ast.falseBody);
      jump2.address = out.bytecode.code.length;
    } else {
      jump1.address = out.bytecode.code.length;
    }
  },
};

const bytecodeSecond: MetaInstructionTable = {
  identifier: (out, ast) => pushBytecode(out, ast.token, { type: "bindingast", name: ast.token.value }), // prettier-ignore
  number:  (out, ast) => pushBytecode(out, ast.token, { type: "numberast", value: Number(ast.token.value) }), // prettier-ignore
  name:    (out, ast) => pushBytecode(out, ast.token, { type: "nameast", name: ast.token.value }), // prettier-ignore
  string:  (out, ast) => pushBytecode(out, ast.token, { type: "stringast", value: ast.token.value }), // prettier-ignore
  boolean: (out, ast) => pushBytecode(out, ast.token, { type: "boolast", value: ast.token.value !== 'false' }), // prettier-ignore

  set:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'setast', name: ast.name })), // prettier-ignore,
  operator: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operatorast', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  meta:     (out, ast) => (writeMeta(out, ast.expr), pushBytecode(out, ast.token, { type: 'toast' })),
  comptime: (out, ast) => writeMeta(out, ast.expr),
  letconst: (out, ast) => (writeMeta(out, ast.value), pushBytecode(out, ast.token, { type: 'letlocal', name: ast.name.token.value })),
  tuple:    (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'tuple', count: ast.exprs.length })),

  while: (out, ast) => (writeBytecode(out, ast.body), writeBytecode(out, ast.condition), pushBytecode(out, ast.token, { type: 'whileast' })),

  function: (out, ast) => {
    compilerAssert(false, "Function not implemented yet")
    // pushBytecode(out, ast.token, { type: "closure", id: ast.id }), // prettier-ignore
  },
  return: (out, ast) => {
    if (ast.expr) writeBytecode(out, ast.expr);
    pushBytecode(out, ast.token, { type: 'returnast', r: !!ast.expr })
  },

  list: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'listast', count: ast.exprs.length })),

  and: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "andast", count: ast.exprs.length })),
  or: (out, ast) =>  (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "orast", count: ast.exprs.length })),
  
  let: (out, ast) => {
    if (ast.value) writeBytecode(out, ast.value);
    if (ast.type) writeMeta(out, ast.type);
    pushBytecode(out, ast.token, { type: 'letast', name: ast.name.token.value, t: !!ast.type, v: !!ast.value })
  },

  call: (out, ast) => {
    ast.typeArgs.forEach(x => writeMeta(out, x));
    writeAll(out, ast.args);
    if (ast.left instanceof ParseIdentifier) {
      pushBytecode(out, ast.token, { type: "callast", name: ast.left.token.value, count: ast.args.length, tcount: ast.typeArgs.length }); // prettier-ignore
      return;
    }
    compilerAssert(false, "Call with non-identifier not implemented yet")
  },

  statements: (out, ast) => {
    pushBytecode(out, ast.token, { type: "pushqs" });
    ast.exprs.forEach((stmt, i) => {
      writeBytecode(out, stmt);
      if (!isParseVoid(stmt)) pushBytecode(out, ast.token, { type: "appendq" });
      pushBytecode(out, ast.token, { type: "pop" }); // Even pop the final value
    });
    pushBytecode(out, ast.token, { type: "popqs" });
  },

  else: (out, ast) => writeBytecode(out, ast.body),

  metaif: (out, ast) => {
    const if_ = ast.expr
    writeMeta(out, if_.condition);
    const jump1 = { type: "jumpf", address: 0 };
    pushBytecode(out, if_.condition.token, jump1);
    writeBytecode(out, if_.trueBody);
    if (if_.falseBody) {
      const jump2 = { type: "jump", address: 0 };
      pushBytecode(out, if_.trueBody.token, jump2);
      jump1.address = out.bytecode.code.length;
      compilerAssert(!(if_.falseBody instanceof ParseIf), "Meta elif not implemented yet")
      writeBytecode(out, if_.falseBody);
      jump2.address = out.bytecode.code.length;
    } else {
      jump1.address = out.bytecode.code.length;
    }
  },

  if: (out, ast) => {
    if (ast.falseBody) writeBytecode(out, ast.falseBody)
    writeBytecode(out, ast.trueBody)
    writeBytecode(out, ast.condition)
    pushBytecode(out, ast.token, { type: "ifast", f: !!ast.falseBody });
  }
};

const compileFunctionPrototype = (ctx: TaskContext, prototype: FunctionPrototype) => {
  if (prototype.bytecode) return prototype.bytecode;

  prototype.bytecode = { code: [], locations: [] }
  const out: BytecodeOut = {
    bytecode: prototype.bytecode,
    table: prototype.instructionTable,
    globalCompilerState: ctx.globalCompiler
  }
  writeBytecode(out, prototype.body);
  prototype.bytecode.code.push({ type: "halt" });
  prototype.bytecode.locations.push(new SourceLocation(-1, -1));

  console.log(`Compiled ${prototype.name}`)
  console.log(bytecodeToString(prototype.bytecode))
  console.log("")
  return prototype.bytecode;
};

type ExecuteVmArg = {
  bytecode: BytecodeGen, scope: Scope
};

function executeBytecodeImpl(ctx, { bytecode, scope }: ExecuteVmArg): Task<unknown, never> {
  compilerAssert(scope, "Expected scope")

  const vm: Vm = { ip: 0, stack: [], scope, location: undefined!, bytecode: bytecode!, context: ctx };
  pushSubCompilerState(ctx, { vm, func: null });
  compilerAssert(bytecode)

  return (
    executeVmTask.create({ vm  })
    .chainFn((task, arg) => {

      compilerAssert(vm.stack.length === 1, "Expected 1 value on stack at end of function. Got $num", { num: vm.stack.length, vm })

      const result = vm.stack.pop();
      // vm = compilerState ? compilerState.vm : undefined!;
      return Task.of(result);
    })
  );
};
const executeBytecodeTask = createTaskDef<ExecuteVmArg, void, unknown, never>(executeBytecodeImpl)


const compileAndExecuteFunctionHeaderDef = createTaskDef<TypeCheckAndCompileArg, void, string, never>(
  function compileAndExecuteFunctionHeaderDef(ctx: TaskContext, { func, args, parentScope }, param) {

    compilerAssert(args.length === func.args.length, 'Expected $x args got $y', { x: func.args.length, y: args.length })
    func.args.forEach((arg, i) => {
      // expectArg(arg[0], args[i].type, arg[1])
    })
    if (func.args.length === 0) return Task.of("success");
    
    if (!func.headerPrototype) {
      const args = func.args.map(([name, type], i) => type ? type : new ParseNil(createToken('')))
      const body = new ParseTuple(createToken(''), args);
      func.headerPrototype = { name: `${func.debugName} header`, body, instructionTable: bytecodeDefault }; // prettier-ignore
      compileFunctionPrototype(ctx, func.headerPrototype)
    }

    const scope = Object.create(parentScope);

    return (
      executeBytecodeTask.create({ bytecode: func.headerPrototype!.bytecode!, scope })
      .chainFn((task, compiledArgTypes) => {

        compilerAssert(compiledArgTypes instanceof Tuple, "Expected tuple")
        compiledArgTypes.values.forEach((type, i) => {
          if (type === null) return
          compilerAssert(type instanceof Type);
          compilerAssert(args[i].type === type, "Argument $name of type $value does not match $expected", { name: func.args[i][0].token, value: args[i].type, expected: type })
        })
        return Task.of("success")
      })
    )

  });

type TypeCheckAndCompileArg = {
    func: FunctionDefinition,
    typeArgs: unknown[],
    args: Ast[],
    parentScope: Scope
}

function functionTemplateTypeCheckAndCompileImpl(ctx: TaskContext, { func, typeArgs, args, parentScope }: TypeCheckAndCompileArg, param): Task<CompiledFunction, never> {

  const argBindings: Binding[] = [];
  let typeParamHash;

  return (
    compileAndExecuteFunctionHeaderDef.create({ func, args, typeArgs, parentScope })
    .chainFn((task, arg) => {

      compilerAssert(func.body)

      if (!func.templatePrototype)  {
        func.templatePrototype = { name: `${func.debugName} template bytecode`, body: func.body, instructionTable: bytecodeSecond }; // prettier-ignore
        compileFunctionPrototype(ctx, func.templatePrototype);
      }
      compilerAssert(func.templatePrototype);

      compilerAssert(typeArgs.length === func.typeArgs.length, "Expected $expected type parameters, got $got", { expected: func.typeArgs.length, got: typeArgs.length })
      typeParamHash = hashValues(typeArgs)
      const existing = func.compiledFunctions.find(compiledFunc => {
        if (compiledFunc.typeParamHash === typeParamHash) {
          if (compiledFunc.typeParameters.every((x, i) => x === typeArgs[i])) return true
        }
      })
      if (existing) return Task.of(existing);

      const templateScope = Object.create(parentScope);
      
      func.args.forEach(([iden, type], i) => {
        const binding = new Binding(iden.token.value, VoidType);
        templateScope[iden.token.value] = binding;
        argBindings.push(binding);
      });
      
      func.typeArgs.forEach((typeArg, i) => {
        compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
        templateScope[typeArg.token.value] = typeArgs[i];
      });

      return (
        executeBytecodeTask.create({ bytecode: func.templatePrototype.bytecode!, scope: templateScope })
        .chainFn((task, ast) => {

          const concreteTypes = []

          console.log(`Compiled template ${func.debugName}`)
          
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
    })
  )

}

export const functionTemplateTypeCheckAndCompileTask = createTaskDef<TypeCheckAndCompileArg, void, CompiledFunction, never>(functionTemplateTypeCheckAndCompileImpl)

type FunctionCallArg = {
  func: FunctionDefinition,
  typeArgs: any[],
  args: any[],
  parentScope: Scope
}
function functionCompileTimeCompileImpl(ctx: TaskContext, { func, typeArgs, args, parentScope }: FunctionCallArg) {
  compilerAssert(func.body, "Expected body");
  compilerAssert(args.length === func.args.length, "Expected $expected arguments got $got", { expected: func.args.length, got: func.args.length, func })

  if (!func.compileTimePrototype) 
    func.compileTimePrototype = { name: `${func.debugName} comptime bytecode`, body: func.body, instructionTable: bytecodeDefault }; // prettier-ignore
  compilerAssert(func.compileTimePrototype)

  const scope = Object.create(parentScope);
  args.forEach((arg, i) => {
    scope[func.args[i][0].token.value] = arg;
  });
  // func.typeArgs.forEach((typeArg, i) => {
  //   scope[typeArg] = typeArgs[i];
  // });
  compileFunctionPrototype(ctx, func.compileTimePrototype)
  return executeBytecodeTask.create({ bytecode: func.compileTimePrototype.bytecode!, scope });
};
const functionCompileTimeCompileTask = createTaskDef<FunctionCallArg, void, unknown, never>(functionCompileTimeCompileImpl)


const popValues = (vm: Vm, num: number) => {
  if (vm.stack.length < num) throw new Error(`Expected ${num} values on stack`);
  return Array.from(new Array(num)).map(() => vm.stack.pop()).reverse() // prettier-ignore
};
const popStack = (vm: Vm) => {
  if (vm.stack.length === 0) {
    console.log(vm.stack)
    throw new Error(`Expected 1 value on stack got ${vm.stack.length}`);
  }
  return vm.stack.pop();
};


const operators = {
  "-": (a, b) => a - b,
  "*": (a, b) => a * b,
  "+": (a, b) => a + b,
  "/": (a, b) => a / b,
  "<": (a, b) => a < b,
  ">": (a, b) => a > b,
  ">=": (a, b) => a >= b,
  "<=": (a, b) => a <= b,
  "==": (a, b) => a == b,
  "!=": (a, b) => a != b,
};


const letLocal = (vm: Vm, name: string, type: Type | null, value: Ast | null) => {
  compilerAssert(type || value);
  compilerAssert(!Object.hasOwn(vm.scope, name), `Already defined $name`, { name });
  const inferType = type || value!.type
  console.log("inferType", inferType)
  const binding = (vm.scope[name] = new Binding(name, inferType));
  return new LetAst(VoidType, vm.location, binding, value);
}

type CallArgs = { vm: Vm, name: string, count: number, tcount: number }
function createCallAstImpl(ctx, { vm, name, count, tcount }: CallArgs): Task<string, never> {
  const args = popValues(vm, count)
  compilerAssert(args.every(isAst), "Expected ASTs", { args })
  const typeArgs = popValues(vm, tcount || 0);
  const func = expectMap(vm.scope, name, `$key not in scope`)
  if (func instanceof ExternalFunction) {
    vm.stack.push(new CallAst(IntType, vm.location, func, args));
    return Task.of("success")
  }
  if (func instanceof Closure) {
    return (
      functionTemplateTypeCheckAndCompileTask.create({ func: func.func, typeArgs, args, parentScope: func.scope })
      .chainFn((task, compiledFunction) => {
        const binding = compiledFunction.binding;
        const returnType = compiledFunction.returnType;
        vm.stack.push(new UserCallAst(returnType, vm.location, binding, args));
        return Task.of("success")

      })
    )
  }
  compilerAssert(false, "Not supported value $func", { func })
}
const createCallAstDef = createTaskDef<CallArgs, void, string, never>(createCallAstImpl);

const newEvent = () => {
  return new Event<string, never>();
}

function resolveScope(scope: Scope, name: string) {
  if (scope[name]) return Task.of(scope[name])
  if (!scope[ScopeEventsSymbol]) scope[ScopeEventsSymbol] = {}
  if (!scope[ScopeEventsSymbol][name]) scope[ScopeEventsSymbol][name] = newEvent()
  return Task.waitFor(scope[ScopeEventsSymbol][name])
}

function callFunctionImpl(ctx, { vm, name, count, tcount }: CallArgs): Task<string, never> {
  const values = popValues(vm, count);
  const typeArgs = popValues(vm, tcount || 0);
  return (
    resolveScope(vm.scope, name)
    .chainFn((task, func) => {

      if (func instanceof ExternalFunction) {
        const functionResult = func.func(...values);
        vm.stack.push(functionResult);
        return Task.of('success');
      }

      if (func instanceof Closure) {
        const call: FunctionCallArg = { func: func.func, typeArgs, args: values, parentScope: func.scope };
        const task = functionCompileTimeCompileTask.create(call)
        return task.chainFn((task, res) => { vm.stack.push(res); return Task.of("success") })
      }
      compilerAssert(!(func instanceof FunctionDefinition), "$func is not handled", { func })
      compilerAssert(false, "$func is not a function", { func })
    })
  )
}
const callFunctionTask = createTaskDef<CallArgs, void, string, never>(callFunctionImpl);

const unknownToAst = (location: SourceLocation, value: unknown) => {
  if (typeof value === 'number') return new NumberAst(IntType, location, value);
  if (isAst(value)) return value;
  if (value instanceof Binding) return new BindingAst(value.type, location, value);
  compilerAssert(false, "Type is not convertable to an AST: $value", { value })
}

const instructions: InstructionMapping = {
  push: (vm, { value }) => vm.stack.push(value),
  nil: (vm) => vm.stack.push(null),

  operatorast: (vm, { name, count }) => vm.stack.push(new OperatorAst(IntType, vm.location, name, popValues(vm, count))),
  numberast: (vm, { value }) => vm.stack.push(new NumberAst(IntType, vm.location, value)),
  stringast: (vm, { value }) => vm.stack.push(new StringAst(StringType, vm.location, value)),
  boolast: (vm, { value }) =>   vm.stack.push(new BoolAst(BoolType, vm.location, value)),
  setast: (vm, { name }) =>     vm.stack.push(new SetAst(VoidType, vm.location, name, popStack(vm))),
  orast: (vm, { count }) =>     vm.stack.push(new OrAst(IntType, vm.location, popValues(vm, count))),
  andast: (vm, { count }) =>    vm.stack.push(new AndAst(IntType, vm.location, popValues(vm, count))),
  listast: (vm, { count }) =>   vm.stack.push(new ListAst(IntType, vm.location, popValues(vm, count))),
  ifast: (vm, { f }) =>         vm.stack.push(new IfAst(IntType, vm.location, popStack(vm), popStack(vm), f ? popStack(vm) : null)),
  whileast: (vm) =>             vm.stack.push(new WhileAst(VoidType, vm.location, popStack(vm), popStack(vm))),
  returnast: (vm, { r }) =>     vm.stack.push(new ReturnAst(VoidType, vm.location, r ? popStack(vm) : null)),
  letast: (vm, { name, t, v }) => vm.stack.push(letLocal(vm, name, t ? popStack(vm) : null, v ? popStack(vm) : null)),
  callast: (vm, { name, count, tcount }) => createCallAstDef.create({ vm, name, count, tcount }),
  toast: (vm) => vm.stack.push(unknownToAst(vm.location, popStack(vm))),

  bindingast: (vm, { name }) => {
    const value = expectMap(vm.scope, name, `No binding ${name}`);
    vm.stack.push(unknownToAst(vm.location, value));
  },
  return: (vm, { r }) => { 
    const ret = r ? vm.stack[vm.stack.length - 1] : null;
    vm.stack.length = 0
    vm.stack.push(ret);
    vm.ip = vm.bytecode.code.length - 1;
  },

  binding: (vm, { name }) => {
    return resolveScope(vm.scope, name).chainFn((task, res) => {
      vm.stack.push(res)
      return Task.of('success')
    })
  },
  
  pop: (vm) => vm.stack.pop(),
  jumpf: (vm, { address }) => {
    if (!vm.stack.pop()) vm.ip = address;
  },
  letlocal: (vm, { name }) => {
    expect(!Object.hasOwn(vm.scope, name), `$name is already in scope`, { name });
    setScopeValueAndResolveEvents(vm.scope, name, popStack(vm))
  },
  setlocal: (vm, { name }) => {
    expect(Object.hasOwn(vm.scope, name), `$name not existing in scope`, { name });
    vm.scope[name] = popStack(vm);
  },
  jump: (vm, { address }) => void (vm.ip = address),
  call: (vm, { name, count, tcount }) => callFunctionTask.create({ vm, name, count, tcount }),
  operator: (vm, { name, count }) => {
    const values = popValues(vm, count);
    compilerAssert(operators[name], `Invalid operator $name`, { name });
    const operatorResult = operators[name](...values);
    vm.stack.push(operatorResult);
  },
  closure: (vm, { id }) => {
    const globalCompiler = (vm.context as TaskContext).globalCompiler
    compilerAssert(id in globalCompiler.functionDefinitions, "Not found in func $id", { id })
    const func = globalCompiler.functionDefinitions[id];
    const closure = new Closure(func, vm.scope);
    vm.stack.push(closure);
    if (func.name) {
      console.log("Closure", func.name.token.value)
      compilerAssert(!Object.hasOwn(vm.scope, func.name.token.value), "$name already in scope", { name: func.name, value: vm.scope[func.name.token.value] })
      vm.scope[func.name.token.value] = closure;
    }
  },
  tuple: (vm, { count }) => vm.stack.push(new Tuple(popValues(vm, count))),
  pushqs: (vm) => vm.context.subCompilerState.quoteStack.push([]),
  popqs: (vm) => vm.stack.push(createStatements(vm.location, vm.context.subCompilerState.quoteStack.pop())),
  appendq: (vm) => {
    const value = vm.stack.pop();
    const compilerState = vm.context.subCompilerState;
    compilerState.quoteStack[compilerState.quoteStack.length - 1].push(value);
    vm.stack.push(null); // needed for statements
  },
};

function executeVmImpl(ctx, { vm } : { vm: Vm }, p: void): Task<string, never> {
  const {locations, code} = vm.bytecode;
  let current = code[vm.ip];
  vm.location = locations[vm.ip];
  compilerAssert(current, "Expected 'halt' instruction")
  while (current.type !== "halt") {
    const startIp = vm.ip;
    const instr = instructions[current.type];
    compilerAssert(instr, "Not inplemented yet instruction $type", { type: current.type, current })
    let res;
    try {
      res = instr(vm, current);
    } catch(ex) {
      console.log({ current, ip: vm.ip })
      throw ex;
    }

    if (isTaskResult(res) || !isTask(res)) {
      if (vm.ip === startIp) vm.ip++;
      current = code[vm.ip];
      vm.location = locations[vm.ip];
    } else {
      return (res as Task<string, never>).chainFn(() => {
        if (vm.ip === startIp) vm.ip++;
        current = code[vm.ip];
        vm.location = locations[vm.ip];

        return executeVmTask.create({ vm })
      })
    }
  }
  return Task.of('success')
};
export const executeVmTask = createTaskDef<{ vm: Vm }, void, string, never>(executeVmImpl);

const setScopeValueAndResolveEvents = (scope: Scope, name: string, value: unknown) => {
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

type TopLevelFuncDecArg = {
  funcDecl: ParserFunctionDecl,
  scope: Scope
}
function topLevelFunctionDefinitionImpl(ctx: TaskContext, { funcDecl, scope }: TopLevelFuncDecArg) {
  const funcDef = addFunctionDefinition(ctx.globalCompiler, funcDecl)

  compilerAssert(!Object.hasOwn(scope, funcDef.name!.token.value), "$name already in scope", { name: funcDef.name!.token.value, value: scope[funcDef.name!.token.value] })

  setScopeValueAndResolveEvents(scope, funcDef.name!.token.value, new Closure(funcDef, scope))

  return Task.of("Success")
}


const topLevelFunctionDefinitionTask = createTaskDef<TopLevelFuncDecArg, void, string, never>(topLevelFunctionDefinitionImpl)

export const runTopLevel = (globalCompiler: GlobalCompilerState, stmts: ParseStatements, rootScope: Scope) => {
  const tasks: Task<unknown, unknown>[] = []
  
  stmts.exprs.forEach(expr => {
    if (expr.key === 'letconst') {
      console.log(expr)

      const out: BytecodeOut = {
        bytecode: { code: [], locations: [] },
        table: bytecodeDefault,
        globalCompilerState: globalCompiler
      }
      writeBytecode(out, expr.value);
      out.bytecode.code.push({ type: "halt" });
      out.bytecode.locations.push(new SourceLocation(-1, -1));

      console.log(bytecodeToString(out.bytecode))

      const task = (
        executeBytecodeTask.create({ bytecode: out.bytecode, scope: rootScope })
        .chainFn((task, result) => {
          setScopeValueAndResolveEvents(rootScope, expr.name.token.value, result)
          return Task.of('success')
        })
      );
      tasks.push(task);
      return
    }
    if (expr.key === 'function') {

      tasks.push(topLevelFunctionDefinitionTask.create({ funcDecl: expr.functionDecl, scope: rootScope }));

      return

    }
    // compilerAssert(false, `Not supported at top level $key`, { key: expr.key })
  })

  return Task.concurrency(tasks);
}
