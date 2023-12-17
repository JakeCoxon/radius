import { isParseVoid, BytecodeOut, FunctionDefinition, Type, Binding, LetAst, UserCallAst, CallAst, Ast, NumberAst, OperatorAst, SetAst, OrAst, AndAst, ListAst, IfAst, StatementsAst, Scope, createScope, Closure, ExternalFunction, compilerAssert, VoidType, IntType, FunctionPrototype, Vm, MetaInstructionTable, Token, expect, createStatements, DoubleType, FloatType, StringType, expectMap, bytecodeToString, ParseCall, ParseIdentifier, ParseAst, CompiledFunction, AstRoot, isAst, compilerState, pushSubCompilerState, popSubCompilerState, addFunctionDefinition, ParseNil, createToken, ParseStatements, FunctionType, StringAst, WhileAst, BoolAst, BindingAst, SourceLocation, BytecodeInstr, ReturnAst, BytecodeGen, ParserFunctionDecl, ScopeEventsSymbol } from "./defs";
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
  writeBytecode({ bytecode: out.bytecode, table: bytecodeDefault }, expr)
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

  list: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'list', count: ast.exprs.length })),
  // binding: (out, ast) => pushBytecode(out, { type: "binding", name }), // prettier-ignore
  
  let: (out, ast) => {
    compilerAssert(ast.value, "Not implemented yet");
    compilerAssert(ast.type, "Not implemented yet");
    (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'let', name: ast.name })) // prettier-ignore
  },

  function: (out, ast) => {
    pushBytecode(out, ast.token, { type: "closure", id: addFunctionDefinition(ast.functionDecl).id }) // prettier-ignore
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

  // "call*": (out, ...rest) => (writeAll(out, rest), pushBytecode(out, { type: "call", args: rest.length })), // prettier-ignore
  // "callt*": (out, ...rest) => (writeAll(out, rest), pushBytecode(out, { type: "callt", args: rest.length })), // prettier-ignore
  // builtincall: (out, name, ...args) => (writeAll(out, args), pushBytecode(out, { type: "builtin", name, args: args.length })), // prettier-ignore
  // tuple: (out, ...args) => (writeAll(out, args), pushBytecode(out, { type: "tuple", args: args.length })), // prettier-ignore
  // appendquote: (out, quote) => (writeBytecode(out, quote), pushBytecode(out, { type: 'appendq'})), // prettier-ignore
  // deflocal: (out, name, value) => (writeBytecode(out, value), pushBytecode(out, { type: 'deflocal', name })), // prettier-ignore
  // setlocal: (out, name, value) => (writeBytecode(out, value), pushBytecode(out, { type: 'setlocal', name })), // prettier-ignore
  // operator: (out, name, ...args) => (writeAll(out, args), pushBytecode(out, { type: 'operator', name, args: args.length })), // prettier-ignore

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
  
  // "statements*": (out, ...stmts) => {
  //   stmts.forEach((x, i) => {
  //     writeBytecode(out, x);
  //     if (i !== stmts.length - 1) pushBytecode(out, { type: "pop" });
  //   });
  // },

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
  // string: (out, ast) => pushBytecode(out, ast.token, { type: "stringast", token: ast.value }), // prettier-ignore
  identifier: (out, ast) => pushBytecode(out, ast.token, { type: "bindingast", name: ast.token.value }), // prettier-ignore
  number:  (out, ast) => pushBytecode(out, ast.token, { type: "numberast", value: Number(ast.token.value) }), // prettier-ignore
  name:    (out, ast) => pushBytecode(out, ast.token, { type: "nameast", name: ast.token.value }), // prettier-ignore
  string:  (out, ast) => pushBytecode(out, ast.token, { type: "stringast", value: ast.token.value }), // prettier-ignore
  boolean: (out, ast) => pushBytecode(out, ast.token, { type: "boolast", value: ast.token.value !== 'false' }), // prettier-ignore
  // closure: (out, ast) => pushBytecode(out, { type: "closure", id }), // prettier-ignore

  // "callt*": (out, ast) => (writeAll(out, rest), pushBytecode(out, { type: "callt", args: rest.length })), // prettier-ignore
  // builtincall: (out, ast) => (writeAll(out, args), pushBytecode(out, { type: "builtin", name, args: args.length })), // prettier-ignore
  // tuple: (out, ast) => (writeAll(out, args), pushBytecode(out, { type: "tuple", args: args.length })), // prettier-ignore
  set:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'setast', name: ast.name })), // prettier-ignore,
  operator: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operatorast', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  meta:     (out, ast) => (writeMeta(out, ast.expr), pushBytecode(out, ast.token, { type: 'toast' })),
  comptime: (out, ast) => writeMeta(out, ast.expr),
  letconst: (out, ast) => (writeMeta(out, ast.value), pushBytecode(out, ast.token, { type: 'letlocal', name: ast.name.token.value })),

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
    compilerAssert(ast.value, "Not implemented yet");
    compilerAssert(ast.type, "Not implemented yet");
    (writeBytecode(out, ast.value), writeMeta(out, ast.type), pushBytecode(out, ast.token, { type: 'letast', name: ast.name.token.value })) // prettier-ignore,
  },

  call: (out, ast) => {
    writeAll(out, ast.typeArgs);
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
    // TODO: elsif
    const if_ = ast.expr
    writeMeta(out, if_.condition);
    const jump1 = { type: "jumpf", address: 0 };
    pushBytecode(out, if_.condition.token, jump1);
    writeBytecode(out, if_.trueBody);
    if (if_.falseBody) {
      const jump2 = { type: "jump", address: 0 };
      pushBytecode(out, if_.trueBody.token, jump2);
      jump1.address = out.bytecode.code.length;
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

const compileFunctionPrototype = (prototype: FunctionPrototype) => {
  if (prototype.bytecode) return prototype.bytecode;

  prototype.bytecode = { code: [], locations: [] }
  const out: BytecodeOut = {
    bytecode: prototype.bytecode,
    table: prototype.instructionTable
  }
  writeBytecode(out, prototype.body);
  prototype.bytecode.code.push({ type: "halt" });
  prototype.bytecode.locations.push(new SourceLocation(-1, -1));

  console.log(`Compiled ${prototype.name}`)
  console.log(bytecodeToString(prototype.bytecode))
  console.log("")
  return prototype.bytecode;
};

type ExecuteProtoArg = {
  func: FunctionDefinition, prototype: FunctionPrototype, scope: Scope
};

function executePrototypeImpl(ctx, { func, prototype, scope }: ExecuteProtoArg): Task<unknown, never> {
  compilerAssert(scope, "Expected scope")

  const newVm: Vm = { ip: 0, stack: [], scope, location: undefined!, bytecode: prototype.bytecode! };
  pushSubCompilerState({ vm: newVm, func });
  compilerAssert(prototype.bytecode)

  vm = newVm;
  return (
    executeVmTask.of({ vm })
    .chainFn((task, arg) => {

      popSubCompilerState();
      compilerAssert(vm.stack.length === 1, "Expected 1 value on stack at end of function. Got $num", { num: vm.stack.length })

      const result = vm.stack.pop();
      vm = compilerState ? compilerState.vm : undefined!;
      return Task.of(result);

    })
  );
};
const executePrototypeTask = createTaskDef<ExecuteProtoArg, void, unknown, never>(executePrototypeImpl)

type ExecuteVmArg = {
  bytecode: BytecodeGen, scope: Scope
};

function executeBytecodeImpl(ctx, { bytecode, scope }: ExecuteVmArg): Task<unknown, never> {
  compilerAssert(scope, "Expected scope")

  const newVm: Vm = { ip: 0, stack: [], scope, location: undefined!, bytecode: bytecode! };
  pushSubCompilerState({ vm: newVm, func: null });
  compilerAssert(bytecode)

  vm = newVm;
  return (
    executeVmTask.of({ vm })
    .chainFn((task, arg) => {

      popSubCompilerState();
      compilerAssert(vm.stack.length === 1, "Expected 1 value on stack at end of function. Got $num", { num: vm.stack.length })

      const result = vm.stack.pop();
      vm = compilerState ? compilerState.vm : undefined!;
      return Task.of(result);

    })
  );
};
const executeBytecodeTask = createTaskDef<ExecuteVmArg, void, unknown, never>(executeBytecodeImpl)


const compileAndExecuteFunctionHeaderDef = createTaskDef<TypeCheckAndCompileArg, void, string, never>(
  function compileAndExecuteFunctionHeaderDef(ctx, { func, args, parentScope }, param) {

    compilerAssert(args.length === func.args.length, 'Expected $x args got $y', { x: func.args.length, y: args.length })
    func.args.forEach((arg, i) => {
      // expectArg(arg[0], args[i].type, arg[1])
    })
    if (func.args.length === 0) return Task.of("success");
    
    if (!func.headerPrototype) {
      const args = func.args.map(([name, type], i) => type ? type : new ParseNil(createToken('')))
      const body = new ParseCall(createToken(''), new ParseIdentifier(createToken('__eval')), args, []);
      func.headerPrototype = { name: `${func.debugName} header`, body, instructionTable: bytecodeDefault }; // prettier-ignore
      compileFunctionPrototype(func.headerPrototype)
    }

    const concreteTypes = {};
    const expectArg = (name: string, value, expected) => {
      compilerAssert(value === expected, "Argument $name of type $value does not match $expected", { name, value, expected })
      concreteTypes[name] = expected;
    };
    const scope = Object.create(parentScope);
    Object.assign(scope, { __expectArg: new ExternalFunction('__expectArg', expectArg) });
    Object.assign(scope, { __eval: new ExternalFunction('__eval', (...args) => args) }); // TODO: replace with tuple

    // TODO: Do this once for compiled functions
    return (
      executePrototypeTask.of({ func, prototype: func.headerPrototype!, scope })
      .chainFn((task, compiledArgTypes) => {

        compilerAssert(Array.isArray(compiledArgTypes), "Error")
        compiledArgTypes.forEach((type, i) => {
          if (type === null) return
          compilerAssert(type instanceof Type);
          compilerAssert(args[i].type === type, "Argument $name of type $value does not match $expected", { name: func.args[i][0].token, value: args[i].type, expected: type })
        })
        return Task.of("success")
      })
    )

    // console.log('compiled args', res)
    // func.typeArgs.forEach((typeArg, i) => {
    //   // scope[typeArg] = typeArgs[i];
    // });
    // if (func.args.length) {
    //   
    //   executePrototype(func, func.headerPrototype!, scope);
    // }
  });

type TypeCheckAndCompileArg = {
    func: FunctionDefinition,
    typeArgs: unknown[],
    args: Ast[],
    parentScope: Scope
}

function functionTemplateTypeCheckAndCompileImpl(ctx, { func, typeArgs, args, parentScope }: TypeCheckAndCompileArg, param): Task<CompiledFunction, never> {

  const argBindings: Binding[] = [];

  return (
    compileAndExecuteFunctionHeaderDef.of({ func, args, typeArgs, parentScope })
    .chainFn((task, arg) => {

      compilerAssert(func.body)

      if (!func.templatePrototype) 
        func.templatePrototype = { name: `${func.debugName} template bytecode`, body: func.body, instructionTable: bytecodeSecond }; // prettier-ignore
      compilerAssert(func.templatePrototype);

      const templateScope = Object.create(parentScope);
      
      func.args.forEach(([iden, type], i) => {
        const binding = new Binding(iden.token.value, VoidType);
        templateScope[iden.token.value] = binding;
        argBindings.push(binding);
      });

      func.typeArgs.forEach((typeArg, i) => {
        compilerAssert(typeArgs[i], "Expected type argument number $i", { i });
        compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
        templateScope[typeArg.token.value] = typeArgs[i];
      });

      compileFunctionPrototype(func.templatePrototype);

      return executePrototypeTask.of({ func, prototype: func.templatePrototype, scope: templateScope });
    })
    .chainFn((task, ast) => {

      const concreteTypes = []

      console.log(`Compiled template ${func.debugName}`)
      
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      const binding = new Binding(`${func.debugName} compiled`, FunctionType);
      const returnType = ast.type;
      const compiledFunction = new CompiledFunction(binding, func, returnType, concreteTypes, ast, argBindings);
      compilerState.global.compiledFunctions.set(binding, compiledFunction);
      
      return Task.of(compiledFunction)
    })
  )

}

export const functionTemplateTypeCheckAndCompileDef = createTaskDef<TypeCheckAndCompileArg, void, CompiledFunction, never>(functionTemplateTypeCheckAndCompileImpl)

type FunctionCallArg = {
  func: FunctionDefinition,
  typeArgs: any[],
  args: any[],
  parentScope: Scope
}
function functionCompileTimeCompileImpl(ctx, { func, typeArgs, args, parentScope }: FunctionCallArg) {
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
  compileFunctionPrototype(func.compileTimePrototype)
  return executePrototypeTask.of({ func, prototype: func.compileTimePrototype, scope });
};
const functionCompileTimeCompileTask = createTaskDef<FunctionCallArg, void, unknown, never>(functionCompileTimeCompileImpl)


let vm: Vm = undefined!;

const popValues = (num) => {
  if (vm.stack.length < num) throw new Error(`Expected ${num} values on stack`);
  return Array.from(new Array(num)).map(() => vm.stack.pop()).reverse() // prettier-ignore
};
const popStack = () => {
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


const letLocal = (vm: Vm, name: string, type: Type, value: Ast) => {
  if (Object.hasOwn(vm.scope, name))
    throw new Error(`Already defined ${name}`);
  const binding = (vm.scope[name] = new Binding(name, type));
  return new LetAst(VoidType, vm.location, binding, value);
}

type CallArgs = { vm: Vm, name: string, count: number, tcount: number }
function createCallAstImpl(ctx, { vm, name, count, tcount }: CallArgs): Task<string, never> {
  const args = popValues(count)
  compilerAssert(args.every(isAst), "Expected ASTs", { args })
  const typeArgs = popValues(tcount || 0);
  const func = expectMap(vm.scope, name, `$key not in scope`)
  if (func instanceof ExternalFunction) {
    vm.stack.push(new CallAst(IntType, vm.location, func, args));
    return Task.of("success")
  }
  if (func instanceof Closure) {
    return (
      functionTemplateTypeCheckAndCompileDef.of({ func: func.func, typeArgs, args, parentScope: func.scope })
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
  const values = popValues(count);
  const typeArgs = popValues(tcount || 0);
  return (
    resolveScope(vm.scope, name)
    .chainFn((task, func) => {

      if (func instanceof ExternalFunction) {
        const functionResult = func.func(...values);
        vm.stack.push(functionResult);
        return Task.of('success');
      }
      if (func instanceof Closure) {
        return (
          functionCompileTimeCompileTask.of({ func: func.func, typeArgs, args: values, parentScope: func.scope })
          .chainFn((task, functionResult) => {
            vm.stack.push(functionResult);
            return Task.of("success")
          })
        )
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

type InstructionMapping = {[key:string]:(instr: BytecodeInstr) => unknown}

const instructions: InstructionMapping = {
  push: ({ value }) => vm.stack.push(value),
  nil: () => vm.stack.push(null),

  operatorast: ({ name, count }) => vm.stack.push(new OperatorAst(IntType, vm.location, name, popValues(count))),
  numberast:({ value }) =>  vm.stack.push(new NumberAst(IntType, vm.location, value)),
  stringast:({ value }) =>  vm.stack.push(new StringAst(StringType, vm.location, value)),
  boolast:({ value }) =>    vm.stack.push(new BoolAst(IntType, vm.location, value)),
  letast: ({ name }) =>     vm.stack.push(letLocal(vm, name, popStack(), popStack())),
  setast: ({ name }) =>     vm.stack.push(new SetAst(VoidType, vm.location, name, popStack())),
  orast: ({ count }) =>     vm.stack.push(new OrAst(IntType, vm.location, popValues(count))),
  andast: ({ count }) =>    vm.stack.push(new AndAst(IntType, vm.location, popValues(count))),
  listast: ({ count }) =>   vm.stack.push(new ListAst(IntType, vm.location, popValues(count))),
  ifast: ({ f }) =>         vm.stack.push(new IfAst(IntType, vm.location, popStack(), popStack(), f ? popStack() : null)),
  whileast: () =>           vm.stack.push(new WhileAst(VoidType, vm.location, popStack(), popStack())),
  returnast: ({ r }) =>     vm.stack.push(new ReturnAst(VoidType, vm.location, r ? popStack() : null)),
  callast: ({ name, count, tcount }) => createCallAstDef.of({ vm, name, count, tcount }),
  toast: () => vm.stack.push(unknownToAst(vm.location, popStack())),

  bindingast: ({ name }) => {
    const value = expectMap(vm.scope, name, `No binding ${name}`);
    vm.stack.push(unknownToAst(vm.location, value));
  },
  return: ({ r }) => { 
    const ret = r ? vm.stack[vm.stack.length - 1] : null;
    vm.stack.length = 0
    vm.stack.push(ret);
    vm.ip = vm.bytecode.code.length - 1;
  },

  binding: ({ name }) => {
    return resolveScope(vm.scope, name).chainFn((task, res) => {
      vm.stack.push(res)
      return Task.of('success')
    })
  },
  
  pop: () => vm.stack.pop(),
  jumpf: ({ address }) => {
    if (!vm.stack.pop()) vm.ip = address;
  },
  letlocal: ({ name }) => {
    expect(!Object.hasOwn(vm.scope, name), `$name is already in scope`, { name });
    setScopeValueAndResolveEvents(vm.scope, name, popStack())
  },
  setlocal: ({ name }) => {
    expect(Object.hasOwn(vm.scope, name), `$name not existing in scope`, { name });
    vm.scope[name] = popStack();
  },
  jump: ({ address }) => void (vm.ip = address),
  call: ({ name, count, tcount }) => callFunctionTask.of({ vm, name, count, tcount }),
  operator: ({ name, count }) => {
    const values = popValues(count);
    compilerAssert(operators[name], `Invalid operator $name`, { name });
    const operatorResult = operators[name](...values);
    vm.stack.push(operatorResult);
  },
  closure: ({ id }) => {
    compilerAssert(id in compilerState.global.functionDefinitions, "Not found in func $id", { id })
    const func = compilerState.global.functionDefinitions[id];
    const closure = new Closure(func, vm.scope);
    vm.stack.push(closure);
    if (func.name) {
      console.log("Closure", func.name.token.value)
      compilerAssert(!Object.hasOwn(vm.scope, func.name.token.value), "$name already in scope", { name: func.name, value: vm.scope[func.name.token.value] })
      vm.scope[func.name.token.value] = closure;
    }
  },
  tuple: ({ count }) => vm.stack.push({ _tuple: true, values: popValues(count) }),
  pushqs: () => compilerState.quoteStack.push([]),
  popqs: () => vm.stack.push(createStatements(vm.location, compilerState.quoteStack.pop())),
  appendq: () => {
    const value = vm.stack.pop();
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
      res = instr(current);
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

        return executeVmTask.of({ vm })
      })
    }
  }
  return Task.of('success')
};
export const executeVmTask = createTaskDef<{ vm: Vm }, void, string, never>(executeVmImpl);

const setScopeValueAndResolveEvents = (scope: Scope, name: string, value: unknown) => {
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
function topLevelFunctionDefinitionImpl(ctx, { funcDecl, scope }: TopLevelFuncDecArg) {
  const funcDef = addFunctionDefinition(funcDecl)

  compilerAssert(!Object.hasOwn(scope, funcDef.name!.token.value), "$name already in scope", { name: funcDef.name!.token.value, value: scope[funcDef.name!.token.value] })

  setScopeValueAndResolveEvents(scope, funcDef.name!.token.value, new Closure(funcDef, scope))
  return Task.of("Success")
}


const topLevelFunctionDefinitionTask = createTaskDef<TopLevelFuncDecArg, void, string, never>(topLevelFunctionDefinitionImpl)

export const runTopLevel = (stmts: ParseStatements, rootScope: Scope) => {
  const tasks: Task<unknown, unknown>[] = []
  
  stmts.exprs.forEach(expr => {
    if (expr.key === 'letconst') {
      console.log(expr)

      const out: BytecodeOut = {
        bytecode: { code: [], locations: [] },
        table: bytecodeDefault
      }
      writeBytecode(out, expr.value);
      out.bytecode.code.push({ type: "halt" });
      out.bytecode.locations.push(new SourceLocation(-1, -1));

      console.log(bytecodeToString(out.bytecode))

      const task = (
        executeBytecodeTask.of({ bytecode: out.bytecode, scope: rootScope })
        .chainFn((task, result) => {
          setScopeValueAndResolveEvents(rootScope, expr.name.token.value, result)
          return Task.of('success')
        })
      );
      tasks.push(task);
      return
    }
    if (expr.key === 'function') {

      tasks.push(topLevelFunctionDefinitionTask.of({ funcDecl: expr.functionDecl, scope: rootScope }));

      return

    }
    // compilerAssert(false, `Not supported at top level $key`, { key: expr.key })
  })

  return Task.concurrency(tasks);
}
