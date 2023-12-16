import { isParseVoid, BytecodeOut, FunctionDefinition, Type, Binding, LetAst, UserCallAst, CallAst, Ast, NumberAst, OperatorAst, SetAst, OrAst, AndAst, ListAst, IfAst, StatementsAst, Scope, createScope, Closure, ExternalFunction, compilerAssert, VoidType, IntType, FunctionPrototype, Vm, MetaInstructionTable, Token, expect, createStatements, DoubleType, FloatType, StringType, expectMap, bytecodeToString, ParseCall, ParseIdentifier, ParseAst, CompiledFunction, AstRoot, isAst, compilerState, createSubCompilerState, pushSubCompilerState, popSubCompilerState, addFunctionDefinition, ParseNil, createToken } from "./defs";

const pushBytecode = (out: BytecodeOut, token: Token, instr) => {
  out.bytecode.locations.push(token.location);
  out.bytecode.code.push(instr)
}

const writeBytecode = (out: BytecodeOut, expr: ParseAst) => {
  compilerAssert(expr.key, "$expr not found", { expr })
  const table = out.table
  if (!table[expr.key]) throw new Error(`Not found ${expr.key}`);
  table[expr.key](out, expr)
}
const writeAll = (out: BytecodeOut, exprs: ParseAst[]) => {
  exprs.forEach(expr => writeBytecode(out, expr))
}

const bytecodeDefault: MetaInstructionTable = {
  identifier: (out, ast) => pushBytecode(out, ast.token, { type: "binding", name: ast.token.value }), // prettier-ignore
  number: (out, ast) => pushBytecode(out, ast.token, { type: "push", value: Number(ast.token.value) }), // prettier-ignore
  name:   (out, ast) => pushBytecode(out, ast.token, { type: "name", name: ast.token.value }), // prettier-ignore
  string: (out, ast) => pushBytecode(out, ast.token, { type: "push", value: ast.token.value }), // prettier-ignore
  nil:    (out, ast) => pushBytecode(out, ast.token, { type: "push", value: null }), // prettier-ignore

  operator: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operator', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  let:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'let', name: ast.name })), // prettier-ignore
  set:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'set', name: ast.name })), // prettier-ignore
  letconst: (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'let', name: ast.name })), // prettier-ignore
  meta:     (out, ast) => (writeBytecode(out, ast.expr)),
  comptime: (out, ast) => (writeBytecode(out, ast.expr)),

  list: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'list', count: ast.exprs.length })),
  // binding: (out, ast) => pushBytecode(out, { type: "binding", name }), // prettier-ignore

  function: (out, ast) => {
    pushBytecode(out, ast.token, { type: "closure", id: addFunctionDefinition(ast.functionDecl).id }) // prettier-ignore
  },
  call:    (out, ast) => {
    writeAll(out, ast.args);
    if (ast.left instanceof ParseIdentifier) {
      pushBytecode(out, ast.token, { type: "call", name: ast.left.token.value, count: ast.args.length }); // prettier-ignore
      return;
    }
    compilerAssert(false, "Call with non-identifier not implemented yet")
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

  if: (out, ast) => {
    writeBytecode(out, ast.exprs[0]);
    const jump1 = { type: "jumpf", address: 0 };
    pushBytecode(out, ast.exprs[1].token, jump1);
    writeBytecode(out, ast.exprs[1]);
    const jump2 = { type: "jump", address: 0 };
    pushBytecode(out, ast.exprs[2].token, jump2);
    jump1.address = out.bytecode.code.length;
    writeBytecode(out, ast.exprs[2]);
    jump2.address = out.bytecode.code.length;
  },
};


const writeMeta = (out: BytecodeOut, expr: ParseAst) => {
  writeBytecode({ bytecode: out.bytecode, table: bytecodeDefault }, expr)
}

const bytecodeSecond: MetaInstructionTable = {
  // string: (out, ast) => pushBytecode(out, ast.token, { type: "stringast", token: ast.value }), // prettier-ignore
  identifier: (out, ast) => pushBytecode(out, ast.token, { type: "binding", name: ast.token.value }), // prettier-ignore
  number: (out, ast) => pushBytecode(out, ast.token, { type: "numberast", token: ast.token.value }), // prettier-ignore
  name:   (out, ast) => pushBytecode(out, ast.token, { type: "nameast", name: ast.token.value }), // prettier-ignore
  string: (out, ast) => pushBytecode(out, ast.token, { type: "string", name: ast.token.value }), // prettier-ignore
  // closure: (out, ast) => pushBytecode(out, { type: "closure", id }), // prettier-ignore

  // "callt*": (out, ast) => (writeAll(out, rest), pushBytecode(out, { type: "callt", args: rest.length })), // prettier-ignore
  // builtincall: (out, ast) => (writeAll(out, args), pushBytecode(out, { type: "builtin", name, args: args.length })), // prettier-ignore
  // tuple: (out, ast) => (writeAll(out, args), pushBytecode(out, { type: "tuple", args: args.length })), // prettier-ignore
  let:      (out, ast) => {
    compilerAssert(ast.value, "Not implemented yet");
    compilerAssert(ast.type, "Not implemented yet");
    (writeBytecode(out, ast.value), writeBytecode(out, ast.type), pushBytecode(out, ast.token, { type: 'letast', name: ast.name })) // prettier-ignore,
  },
  set:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'setast', name: ast.name })), // prettier-ignore,
  operator: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operatorast', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  meta:     (out, ast) => (writeMeta(out, ast.expr), pushBytecode(out, ast.token, { type: 'toast' })),
  comptime: (out, ast) => writeMeta(out, ast.expr),
  letconst: (out, ast) => (writeMeta(out, ast.value), pushBytecode(out, ast.token, { type: 'letlocal', name: ast.name.token.value })),

  function: (out, ast) => {
    compilerAssert(false, "Function not implemented yet")
    // pushBytecode(out, ast.token, { type: "closure", id: ast.id }), // prettier-ignore
  },

  list: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'listast', count: ast.exprs.length })),

  and: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "andast", count: ast.exprs.length })),
  or: (out, ast) =>  (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "orast", count: ast.exprs.length })),

  call: (out, ast) => {
    writeAll(out, ast.args);
    if (ast.left instanceof ParseIdentifier) {
      pushBytecode(out, ast.token, { type: "callast", name: ast.left.token.value, count: ast.args.length }); // prettier-ignore
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

  if: (out, ast) => {
    writeBytecode(out, ast.exprs[0]), writeBytecode(out, ast.exprs[1]), writeBytecode(out, ast.exprs[2])
    pushBytecode(out, ast.token, { type: "ifast" });
  }
};

const compileFunctionPrototype = (prototype: FunctionPrototype) => {
  if (prototype.bytecode) return prototype.bytecode;
  // const expanded = expandMacros(prototype.body);
  // compilerLog(`${prototype.name} expanded`, pretty(toString(expanded)));
  // prototype.bytecode = [];
  // prototype.bytecode.writeAll = (args) =>
  //   args.forEach((x) => writeBytecode(prototype.bytecode, x));
  prototype.bytecode = {
    code: [],
    locations: [],
  }
  const out: BytecodeOut = {
    bytecode: prototype.bytecode,
    table: prototype.instructionTable
  }
  // prototype.bytecode.write = (arg) => writeBytecode(prototype.bytecode, arg);
  writeBytecode(out, prototype.body);
  prototype.bytecode.code.push({ type: "halt" });
  prototype.bytecode.locations.push({ column: -1, line: -1 });

  console.log(`Compiled ${prototype.name}`)
  console.log(bytecodeToString(prototype.bytecode))
  console.log("")
  // compilerLog(prototype.name, bytecodeToString(prototype.bytecode)); // prettier-ignore
  return prototype.bytecode;
};

const executePrototype = (func: FunctionDefinition, prototype: FunctionPrototype, scope: Scope): unknown => {
  if (!scope) throw new Error("Expected scope");

  const newVm: Vm = {
    ip: 0,
    stack: [],
    scope,
  };
  pushSubCompilerState({ vm: newVm, func });
  compilerAssert(prototype.bytecode)

  vm = newVm;
  executeBytecode(prototype.bytecode.code, vm);
  popSubCompilerState();
  if (vm.stack.length !== 1) {
    console.log(vm.stack)
    throw new Error("Expected 1 value on stack at end of function. Got " + vm.stack.length);
  }

  const result = vm.stack.pop();
  vm = compilerState ? compilerState.vm : undefined!;
  return result;
};


const compileAndExecuteFunctionHeader = (func: FunctionDefinition, args: Ast[], parentScope: Scope) => {
  compilerAssert(args.length === func.args.length, 'Expected $x args got $y', { x: func.args.length, y: args.length })
  func.args.forEach((arg, i) => {
    // expectArg(arg[0], args[i].type, arg[1])
  })
  if (func.args.length === 0) return [];
  
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
  const compiledArgTypes = executePrototype(func, func.headerPrototype!, scope);
  compilerAssert(Array.isArray(compiledArgTypes), "Error")
  compiledArgTypes.forEach((type, i) => {
    if (type === null) return
    compilerAssert(type instanceof Type);
    compilerAssert(args[i].type === type, "Argument $name of type $value does not match $expected", { name: func.args[i][0].token, value: args[i].type, expected: type })
  })

  // console.log('compiled args', res)
  // func.typeArgs.forEach((typeArg, i) => {
  //   // scope[typeArg] = typeArgs[i];
  // });
  // if (func.args.length) {
  //   
  //   executePrototype(func, func.headerPrototype!, scope);
  // }
}

export const functionTemplateTypeCheckAndCompile = (
  func: FunctionDefinition,
  typeArgs: any[],
  args: Ast[],
  parentScope: Scope
) => {
  compileAndExecuteFunctionHeader(func, args, parentScope)

  if (!func.templatePrototype) 
    func.templatePrototype = { name: `${func.debugName} template bytecode`, body: func.body, instructionTable: bytecodeSecond }; // prettier-ignore
  compilerAssert(func.templatePrototype);

  const concreteTypes = []

  const templateScope = Object.create(parentScope);
  const argBindings: Binding[] = [];
  func.args.forEach((arg, i) => {
    const binding = new Binding(arg[0].token.value, VoidType);
    templateScope[arg[0].token.value] = binding;
    argBindings.push(binding);
  });
  func.typeArgs.forEach((typeArg, i) => {
    // templateScope[typeArg] = typeArgs[i];
  });

  compileFunctionPrototype(func.templatePrototype);
  const ast = executePrototype(func, func.templatePrototype, templateScope)
  console.log(`Compiled template ${func.debugName}`)
  
  compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

  const binding = new Binding(`${func.debugName} compiled`, null);
  const returnType = ast.type;
  const compiledFunction = new CompiledFunction(binding, func, returnType, concreteTypes, ast, argBindings);
  compilerState.global.compiledFunctions.set(binding, compiledFunction);
  return compiledFunction;
};

const functionCompileTimeCompile = (
  func: FunctionDefinition,
  typeArgs: any[],
  args: any[],
  parentScope: Scope
) => {
  if (args.length !== func.args.length) throw new Error(`Expected ${func.args.length} args got ${args.length}`); // prettier-ignore
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
  return executePrototype(func, func.compileTimePrototype, scope);
};


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
  return new LetAst(VoidType, binding, value);
}

const createCall = (vm: Vm, name: string, args: Ast[]) => {
  const func = expectMap(vm.scope, name, `$key not in scope`)
  if (func instanceof ExternalFunction) return new CallAst(IntType, func, args);
  if (func instanceof Closure) {
    const compiledFunction = functionTemplateTypeCheckAndCompile(func.func, [], args, func.scope); // prettier-ignore
    const binding = compiledFunction.binding;
    const returnType = compiledFunction.returnType;
    return new UserCallAst(returnType, binding, args)
  }
  compilerAssert(false, "Not supported value $func", { func })
}

const toAst = (value: unknown) => {
  if (typeof value === 'number') return new NumberAst(IntType, value);
  compilerAssert(false, "Not supported", { value })
}

const instructions = {
  push: ({ value }) => vm.stack.push(value),
  nil: () => vm.stack.push(null),
  // builtin: ({ name, args }) => {
  //   if (vm.stack.length < args) throw new Error("Not enough stack values");
  //   const values = popValues(args);
  //   if (builtins[name]) return vm.stack.push(builtins[name](...values));
  //   if (primitives[name]) return vm.stack.push(primitives[name](...values));
  //   throw new Error("No builtin " + name);
  // },
  operatorast: ({ name, count }) => vm.stack.push(new OperatorAst(IntType, name, popValues(count))),
  numberast:({ token }) =>  vm.stack.push(new NumberAst(IntType, Number(token))),
  letast: ({ name }) =>     vm.stack.push(letLocal(vm, name, popStack(), popStack())),
  setast: ({ name }) =>     vm.stack.push(new SetAst(VoidType, name, popStack())),
  orast: ({ count }) =>     vm.stack.push(new OrAst(IntType, popValues(count))),
  andast: ({ count }) =>    vm.stack.push(new AndAst(IntType, popValues(count))),
  listast: ({ count }) =>   vm.stack.push(new ListAst(IntType, popValues(count))),
  ifast: () =>              vm.stack.push(new IfAst(IntType, popStack(), popStack(), popStack())),
  callast: ({ name, count }) => vm.stack.push(createCall(vm, name, popValues(count))),
  toast: () => vm.stack.push(toAst(popStack())),

  binding: ({ name }) => vm.stack.push(expectMap(vm.scope, name, `No binding ${name}`)),
  
  pop: () => vm.stack.pop(),
  jumpf: ({ address }) => {
    if (!vm.stack.pop()) vm.ip = address;
  },
  letlocal: ({ name }) => {
    expect(!Object.hasOwn(vm.scope, name), `${name} already in scope`);
    vm.scope[name] = popStack();
  },
  setlocal: ({ name }) => {
    expect(Object.hasOwn(vm.scope, name), `${name} not existing in scope`);
    vm.scope[name] = popStack();
  },
  jump: ({ address }) => void (vm.ip = address),
  call: ({ name, count, tcount }) => {
    const values = popValues(count);
    const typeArgs = popValues(tcount || 0);
    const func = expectMap(vm.scope, name, "Expected $key in scope")
    if (func instanceof ExternalFunction) {
      const functionResult = func.func(...values);
      vm.stack.push(functionResult);
      return;
    }
    if (func instanceof Closure) {
      const functionResult = functionCompileTimeCompile(func.func, typeArgs, values, func.scope); // prettier-ignore
      vm.stack.push(functionResult);
      return;
    }
    compilerAssert(!(func instanceof FunctionDefinition), "$func is not handled", { func })

    compilerAssert(false, "$func is not a function", { func })
  },
  operator: ({ name, count }) => {
    const values = popValues(count);
    if (!operators[name]) throw new Error(`Invalid operator ${name}`);
    const operatorResult = operators[name](...values);
    vm.stack.push(operatorResult);
  },
  closure: ({ id }) => {
    compilerAssert(id in compilerState.global.functionDefinitions, "Not found in func $id", { id })
    const func = compilerState.global.functionDefinitions[id];
    const closure = new Closure(func, vm.scope);
    vm.stack.push(closure);
    if (func.name) {
      compilerAssert(!Object.hasOwn(vm.scope, func.name.token.value), "$name already in scope", { name: func.name, value: vm.scope[func.name.token.value] })
      vm.scope[func.name.token.value] = closure;
    }
  },
  tuple: ({ count }) => vm.stack.push({ _tuple: true, values: popValues(count) }),
  pushqs: () => compilerState.quoteStack.push([]),
  popqs: () => vm.stack.push(createStatements(compilerState.quoteStack.pop())),
  appendq: () => {
    const value = vm.stack.pop();
    compilerState.quoteStack[compilerState.quoteStack.length - 1].push(value);
    vm.stack.push(null); // needed for statements
  },
};

const executeBytecode = (bytecode: any[], vm: Vm) => {
  let current = bytecode[vm.ip];
  while (current.type !== "halt") {
    const startIp = vm.ip;
    const instr = instructions[current.type];
    compilerAssert(instr, "No instruction $type", { type: current.type, current })
    try {
      instr(current);
    } catch(ex) {
      console.log({ current, ip: vm.ip })
      throw ex;
    }
    if (vm.ip === startIp) vm.ip++;
    current = bytecode[vm.ip];
  }
};

// compileFunctionPrototype(prototype);

// const tokenizer = tokenizeWithLocation(input);
// const ast = parse(tokenizer);
// console.log(JSON.stringify(ast, null, 2))
// // console.log(ast)



// const rootScope: Scope = createScope({
//   int: IntType,
//   float: FloatType,
//   double: DoubleType,
//   void: VoidType,
//   string: StringType,
//   compfoo: { _function: (a, b) => 65 + a + b },
//   bar: 123,
//   print: new ExternalFunction('print', (...args) => {
//     console.log("print called", ...args);
//     return args[0];
//   }),
// });

// (ast as IrStatements).exprs.forEach(expr => {
//   if (expr.key === 'function') {

//     const func = functionDefs[expr.id]
//     console.log("Global func", func.name)
//     rootScope[func.name] = func;

//     return

//   }
//   throw new Error(`Not supported ${expr.key}`)
// })

// const func: FunctionDefinition = rootScope["main"];
// functionTemplateTypeCheckAndCompile(func, [], [], rootScope);
