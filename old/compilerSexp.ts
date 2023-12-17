
console.clear();

const sexpr2 = [
  "statements", //
  [
    "defn",
    "my_func",
    [],
    [],
    "int",
    [
      "statements",
      ["def", "foo", "int", ["+", "2", "3"]],
      ["set", "foo", ["+", "foo", "20"]],
      ["const", "myconst", ["+", "32", "64"]],
      ["const", "name", ["string", "hello"]],
      [
        "def",
        "asd",
        "int",
        ["meta", ["+", "3", ["call", "compfoo", "2", "myconst"]]],
      ],
      ["def", "bar", "int", ["meta", ["call", "my_func_2", "2"]]],
      ["def", "boop", "int", "3"],
      ["defn", "my_closure", [], [], "int", ["statements", "boop"]],
      ["def", "boop2", "int", ["meta", ["call", "my_closure"]]],

      ["if", "1", "3", "4"],
      ["const", "myast", ["quote", ["+", "3", "32"]]],
      // ["call", "thing", ["meta", "myast"]],
      // ["call", "thing", ["metaif", "69", "33", "44"]],
      ["call", "print", "foo"],
      ["def", "res", "int", ["call", "my_func_2", "foo"]],
      ["call", "print", ["or", ["==", "res", "25"], [">=", "2", "3"]]],
    ],
  ],
  [
    "defn",
    "my_func_2",
    [],
    [["a", "int"]],
    "int",
    [
      "statements",
      ["const", "foop", "98"],
      [
        "defn",
        "my_inner_func",
        [],
        [["bar", "int"]],
        "int",
        ["statements", ["+", "bar", "foop"]],
      ],
      [
        "defn",
        "my_type_arg_func",
        ["bamp"],
        [["bar", "int"]],
        "int",
        ["statements", ["+", "bar", "bamp"]],
      ],
      ["def", "foo", "int", ["+", "2", "3"]],
      ["set", "foo", ["+", "foo", "20"]],
      ["const", "myconst2", ["+", "32", "64"]],
      ["if", "1", "3", "4"],
      ["const", "myast2", ["quote", ["+", "3", "32"]]],
      ["call", "print", ["metaif", "69", "33", "44"]],
      // ["callt", "my_type_arg_func", ["foo"], ["4"]],
      // ["call", "my_inner_func", "4"],
      ["call", "print", "foo"],
    ],
  ],
];

const createAst = (ast, type, values) => ({ ast, type, values });

const VoidType = { _type: "void" };
const IntType = { _type: "int" };
const FloatType = { _type: "float" };
const DoubleType = { _type: "double" };
const StringType = { _type: "string" };
let compilerState: any = null;

const stmts = (list) => {
  expect(list.length > 0, "Expected statements");
  return createAst("statements", list[list.length - 1].type, list);
};

const rootScope = {
  int: IntType,
  float: FloatType,
  double: DoubleType,
  void: VoidType,
  string: StringType,
  compfoo: { _function: (a, b) => 65 + a + b },
  print: {
    _function: (...args) => {
      console.log("print called", ...args);
      return args[0];
    },
  },
};
// window.rootScope = rootScope;

const createBinding = (name, type) => ({ _binding: true, name, type });

const compiledFunctions = new Map();

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

const builtins = {
  "statements*": (...values) => createAst("statements", VoidType, values),
  "def*": (name, type, value) => {
    if (Object.hasOwn(vm.scope, name))
      throw new Error(`Already defined ${name}`);
    const binding = (vm.scope[name] = createBinding(name, type));
    return createAst("def", VoidType, [binding, type, value]);
  },
  "set*": (name, value) => {
    const binding = vm.scope[name];
    if (!binding) throw new Error(`Cant find ${name}`);
    if (binding.type !== value.type) throw new Error("Type check failed");
    console.log("set", binding, value);
    return createAst("set", VoidType, [binding, value]);
  },
  if: (expr, trueBody, falseBody) => {
    return createAst("if", IntType, [expr, trueBody, falseBody]);
  },
  scope: (body) => {
    throw new Error("Not implemented scope");
  },
  "call*": (name, ...args) => {
    console.log("Function call", name, args);
    const func = vm.scope[name];
    if (!func) {
      console.log(compilerState);
      throw new Error(`No symbol ${name}`);
    }
    if (func._function) return createAst("call", IntType, [name, args]);
    if (!func._userfunction && !func._closure)
      throw new Error(`${name} not a user function`);

    const funcDef = func._closure ? func.func : func;
    const parentScope = func._closure ? func.scope : rootScope;
    const compiledFunction = functionTemplateTypeCheckAndCompile(funcDef, [], args, parentScope); // prettier-ignore
    const binding = compiledFunction.binding;
    const returnType = compiledFunction.returnType;
    return createAst("usercall", returnType, [binding, args]);
  },
  "callt*": (name, typeArgsTuple, argsTuple) => {
    const typeArgs = typeArgsTuple.values;
    const args = argsTuple.values;
    console.log("Function call", name, args);
    const func = vm.scope[name];
    if (!func) {
      console.log(compilerState);
      throw new Error(`No symbol ${name}`);
    }
    if (func._function) return createAst("call", IntType, [name, args]);
    if (!func._userfunction && !func._closure)
      throw new Error(`${name} not a user function`);

    const funcDef = func._closure ? func.func : func;
    const parentScope = func._closure ? func.scope : rootScope;
    const compiledFunction = functionTemplateTypeCheckAndCompile(funcDef, typeArgs, args, parentScope); // prettier-ignore
    const binding = compiledFunction.binding;
    const returnType = compiledFunction.returnType;
    return createAst("usercall", returnType, [binding, args]);
  },
  operator: (symbol, a, b) => {
    return createAst("operator", IntType, [symbol, a, b]);
  },
  and: (a, b) => createAst("and", IntType, [a, b]),
  or: (a, b) => createAst("or", IntType, [a, b]),
  toAst: (expr) => {
    if (typeof expr === "number") return primitives.number(expr);
    if (expr && expr.ast) return expr;
    if (expr._binding) return createAst("binding", expr.type, [expr]);
    console.log({ expr });
    throw new Error("Not supported value");
  },
};
const primitives = {
  number: (value) => createAst("number", IntType, [value]),
  binding: (name) => {
    const value = vm.scope[name];
    if (!value) {
      console.log(vm);
      throw new Error(`Cant find ${name} in scope`);
    }
    if (value._binding) return createAst("binding", value.type, [value]);
    if (typeof value === "number") return primitives.number(value);
    if (value && value.ast) return value;
    throw new Error(`${value} is not a valid value`);
  },
  string: (value) => createAst("string", StringType, [value]),
};

const macros = {
  def: (name, type, value) => {
    if (compilerState.compType === "default") {
      return ["instr", "deflocal", ["instrdata", name], value];
    }
    return ["def*", ["primitive", "string", name], ["comptime", type], value];
  },
  set: (name, value) => {
    if (compilerState.compType === "default") {
      return ["instr", "setlocal", ["instrdata", name], value];
    }
    return ["set*", ["primitive", "string", name], value];
  },
  builtin: (name, ...args) => {
    // builtin is special because args are already expanded
    if (compilerState.compType === "secondOrder") {
      return ["primitive", "builtincall", `${name}`, ...args];
    }
    if (primitives[name]) return ["primitive", name, ...args];
    if (name === "operator") return ["primitive", "operator", ...args];
    if (bytecodeCalls[name]) return ["primitive", name, ...args];
    // if (builtins[name]) return ['primitive', ]

    console.log("builtin error", name, args);
    throw new Error("Not implemented " + name);
    return ["primitive", name, ...args];
  },
  metaif: (expr, trueBody, falseBody) => {
    const d = compilerState.compType === "default";
    const append = d ? (x) => x : (x) => ["instr", "appendquote", x];
    return ["metaAst", ["if",  expr,
      append(['quote', trueBody]),
      append(['quote', falseBody])]]; // prettier-ignore
  },
  appendQuote: (form) => {
    return ["builtincall", "appendQuote", form];
  },
  metaAst: (body) => {
    if (compilerState.compType === "default") return body;
    return ["meta", ["instr", 'metastatements*', body]]; // prettier-ignore
  },
  const: (name, value) => {
    return ["comptime",
      ["instr", "deflocal", ["instrdata", name], value]] // prettier-ignore
  },
  functionref: (name) => {
    if (compilerState.compType === "default") return name;
    return ["primitive", "string", name];
  },
  call: (name, ...args) => {
    return ["call*", ["functionref", name], ...args];
  },
  callt: (name, typeArgs, args) => {
    return [
      "callt*",
      ["functionref", name],
      ["instr", "tuple", ...typeArgs],
      ["instr", "tuple", ...args],
    ];
  },
  statements: (...rest) => {
    if (compilerState.compType === "default") return ["statements*", ...rest];
    return ["metastatements", ...rest];
  },
  quote: (form) => {
    let prevCompType = compilerState.compType;
    compilerState.compType = "secondOrder";
    const expanded = expandMacros(form);
    compilerState.compType = prevCompType;
    return expanded;
  },
  comptime: (form) => {
    let prevCompType = compilerState.compType;
    compilerState.compType = "default";
    const expanded = expandMacros(form);
    compilerState.compType = prevCompType;
    return expanded;
  },
  meta: (form) => {
    if (compilerState.compType === "default")
      throw new Error("Meta meta not implemented yet");
    return ["builtincall", "toAst", ["comptime", form]];
  },
  compileArgs: (expr) => {
    const stmts = expr.map(([name, type]) => {
      return ["call", "__expectArg", ["string", name], name, type]; // prettier-ignore
    });
    return ["statements", ...stmts, "1"];
  },
  defn: (name, typeArgs, args, returnType, body) => {
    const closure = { _userfunction: true, closure: true, name, typeArgs, args, returnType, body }; // prettier-ignore
    const id = functionDefs.push(closure) - 1;

    console.log("Defining closure", name, compilerState.compType);
    return ["instr", "closure", ["instrdata", id]]; // prettier-ignore
  },
  metastatements: (...rest) => {
    const statements = rest.map((x) => {
      const stmt = expandMacros(x);
      // These need test cases. are they possible? how do they fit together?
      if (stmt[1] === 'instr' && stmt[2] === 'statements') throw new Error("Not implemented yet") // prettier-ignore
      if (stmt[1] === 'instr' && stmt[2] === 'metastatements*') throw new Error("Not implemented yet") // prettier-ignore
      if (stmt[1] === 'statements*') throw new Error("Not implemented yet") // prettier-ignore
      if (stmt[1] === "builtincall") return ["instr", "appendquote", stmt];
      return stmt;
    });
    return ["instr", "metastatements*", ...statements];
  },
};
const expandMacros = (form) => {
  if (typeof form === "number")
    return expandMacros(["builtin", "number", Number(form)]);
  if (typeof form === "string") {
    if (Number.isFinite(Number(form)))
      return expandMacros(["builtin", "number", Number(form)]);
    return expandMacros(["builtin", "binding", form]);
  }
  if (!Array.isArray(form)) {
    console.log(form);
    throw new Error("Unexpected");
  }
  const [name, ...rest] = form;

  if (name === "primitive") return form;
  if (name === "builtincall")
    return ["primitive", "builtincall", rest[0], ...rest.slice(1).map(expandMacros)]; // prettier-ignore
  if (name === "instr") {
    const data = (x) => (x[0] === "instrdata" ? x[1] : expandMacros(x));
    const args = rest.slice(1).map(data);
    return ["primitive", rest[0], ...args];
  }

  if (macros[name]) return expandMacros(macros[name](...rest));
  if (primitives[name]) return ["primitive", name, ...rest];
  if (builtins[name])
    return expandMacros(["builtin", name, ...rest.map(expandMacros)]);
  if (operators[name]) 
    return expandMacros(["builtin", "operator", name, ...rest.map(expandMacros)]); // prettier-ignore

  // return form;
  console.log("form", form);
  throw new Error(`Not found '${name}'`);
};
const expandTopLevel = (topLevel, scope) => {
  if (!Array.isArray(topLevel)) throw new Error();
  if (topLevel[0] !== "statements") throw new Error();
  topLevel.slice(1).forEach((statement) => {
    if (!Array.isArray(statement)) throw new Error();
    if (statement[0] === "defn") {
      const [_, name, typeArgs, args, returnType, body] = statement;
      if (scope[name]) throw new Error(`Symbol ${name} already exists`);
      scope[name] = { _userfunction: true, name, typeArgs, args, returnType, body }; // prettier-ignore
      return;
    }
    throw new Error(`Unsupported ${statement[0]}`);
  });
};
const compileFunctionPrototype = (func, prototype) => {
  if (prototype.bytecode) return prototype.bytecode;
  const expanded = expandMacros(prototype.body);
  compilerLog(`${prototype.name} expanded`, pretty(toString(expanded)));
  prototype.bytecode = [];
  prototype.bytecode.writeAll = (args) =>
    args.forEach((x) => writeBytecode(prototype.bytecode, x));
  prototype.bytecode.write = (arg) => writeBytecode(prototype.bytecode, arg);
  writeBytecode(prototype.bytecode, expanded);
  prototype.bytecode.push({ type: "halt" });
  compilerLog(prototype.name, bytecodeToString(prototype.bytecode)); // prettier-ignore
  return prototype.bytecode;
};
const compileAndExecutePrototype = (func, prototype, scope) => {
  if (!scope) throw new Error("Expected scope");
  const newVm = {
    ip: 0,
    stack: [],
    scope,
  };
  const newCompilerState = {
    vm: newVm,
    func,
    compType: prototype.compType,
    quoteStack: [],
  };
  const prevCompilerState = compilerState;
  compilerState = newCompilerState;

  compileFunctionPrototype(func, prototype);

  vm = newVm;
  executeBytecode(prototype.bytecode, vm);
  compilerState = prevCompilerState;
  if (vm.stack.length !== 1)
    throw new Error("Expected 1 value on stack got " + vm.stack.length);

  const result = vm.stack.pop();
  vm = compilerState ? compilerState.vm : undefined;
  return result;
};

const functionTemplateTypeCheckAndCompile = (
  func,
  typeArgs,
  args,
  parentScope = undefined
) => {
  if (!func.headerPrototype) {
    const body = ["compileArgs", func.args];
    func.headerPrototype = { compType: 'default', name: `${func.name} header`, body }; // prettier-ignore
  }
  const concreteTypes = {};
  const __expectArg = (name, value, expected) => {
    if (value !== expected) throw new Error(`Argument ${name} of type ${value} does not match ${expected}`) // prettier-ignore
    concreteTypes[name] = expected;
  };
  const scope = Object.create(parentScope || rootScope);
  Object.assign(scope, { __expectArg: { _function: __expectArg } });
  if (args.length !== func.args.length) throw new Error(`Expected ${func.args.length} args got ${args.length}`); // prettier-ignore
  args.forEach((arg, i) => {
    scope[func.args[i][0]] = arg.type;
  });
  func.typeArgs.forEach((typeArg, i) => {
    scope[typeArg] = typeArgs[i];
  });
  if (func.args.length)
    compileAndExecutePrototype(func, func.headerPrototype, scope);
  if (!func.templatePrototype) 
    func.templatePrototype = { compType: 'secondOrder', name: `${func.name} template bytecode`, body: func.body }; // prettier-ignore
  const templateScope = Object.create(parentScope || rootScope);
  const argBindings = [];
  func.args.forEach((arg, i) => {
    const binding = createBinding(arg[0], concreteTypes[arg[0]]);
    templateScope[arg[0]] = binding;
    argBindings.push(binding);
  });
  func.typeArgs.forEach((typeArg, i) => {
    templateScope[typeArg] = typeArgs[i];
  });
  const ast = compileAndExecutePrototype(func, func.templatePrototype, templateScope); // prettier-ignore
  compilerLog(`${func.name} ast`, JSON.stringify(ast, null, 2));

  const binding = createBinding(`${func.name} compiled`);
  const returnType = ast.type;
  const compiledFunction = {
    binding,
    func,
    returnType,
    concreteTypes,
    ast,
    argBindings,
  };
  compiledFunctions.set(binding, compiledFunction);
  return compiledFunction;
};

const functionCompileTimeTypeCheckAndCompile = (
  func,
  typeArgs,
  args,
  parentScope = undefined
) => {
  if (func._closure) { parentScope = func.scope; func = func.func; } // prettier-ignore
  if (args.length !== func.args.length) throw new Error(`Expected ${func.args.length} args got ${args.length}`); // prettier-ignore
  if (!func.compileTimePrototype) 
    func.compileTimePrototype = { compType: 'default', name: `${func.name} comptime bytecode`, body: func.body }; // prettier-ignore

  const scope = Object.create(parentScope || rootScope);
  args.forEach((arg, i) => {
    scope[func.args[i][0]] = arg;
  });
  func.typeArgs.forEach((typeArg, i) => {
    scope[typeArg] = typeArgs[i];
  });
  const result = compileAndExecutePrototype(func, func.compileTimePrototype, scope); // prettier-ignore
  return result;
};

const bytecodeCalls = {
  string: (out, value) => out.push({ type: "push", value }), // prettier-ignore
  number: (out, value) => out.push({ type: "push", value }), // prettier-ignore
  binding: (out, name) => out.push({ type: "binding", name }), // prettier-ignore
  closure: (out, id) => out.push({ type: "closure", id }), // prettier-ignore

  "call*": (out, ...rest) => (out.writeAll(rest), out.push({ type: "call", args: rest.length })), // prettier-ignore
  "callt*": (out, ...rest) => (out.writeAll(rest), out.push({ type: "callt", args: rest.length })), // prettier-ignore
  builtincall: (out, name, ...args) => (out.writeAll(args), out.push({ type: "builtin", name, args: args.length })), // prettier-ignore
  tuple: (out, ...args) => (out.writeAll(args), out.push({ type: "tuple", args: args.length })), // prettier-ignore
  appendquote: (out, quote) => (out.write(quote), out.push({ type: 'appendq'})), // prettier-ignore
  deflocal: (out, name, value) => (out.write(value), out.push({ type: 'deflocal', name })), // prettier-ignore
  setlocal: (out, name, value) => (out.write(value), out.push({ type: 'setlocal', name })), // prettier-ignore
  operator: (out, name, ...args) => (out.writeAll(args), out.push({ type: 'operator', name, args: args.length })), // prettier-ignore

  and: (out, a, b) => {
    out.write(a);
  },
  or: (out, a, b) => {},

  "metastatements*": (out, ...stmts) => {
    out.push({ type: "pushqs" });
    stmts.forEach((stmt, i) => {
      writeBytecode(out, stmt);
      out.push({ type: "pop" }); // Even pop the final value
    });
    out.push({ type: "popqs" });
  },

  "statements*": (out, ...stmts) => {
    stmts.forEach((x, i) => {
      writeBytecode(out, x);
      if (i !== stmts.length - 1) out.push({ type: "pop" });
    });
  },

  if: (out, expr, trueBody, falseBody) => {
    writeBytecode(out, expr);
    const jump1 = { type: "jumpf", address: 0 };
    out.push(jump1);
    writeBytecode(out, trueBody);
    const jump2 = { type: "jump", address: 0 };
    out.push(jump2);
    jump1.address = out.length;
    writeBytecode(out, falseBody);
    jump2.address = out.length;
  },
};
const functionDefs = [];
const writeBytecode = (out, form) => {
  if (typeof form === "string") return bytecodeCalls.string(out, form);
  if (typeof form === "number") return bytecodeCalls.number(out, form);
  if (!Array.isArray(form)) throw new Error("Unexpected");
  const [_, name, ...rest] = form;
  if (!bytecodeCalls[name]) throw new Error(`Bytecode instr not found ${name}`);
  bytecodeCalls[name](out, ...rest);
};

export const toString = (arg) => {
  if (!arg) return String(arg);
  // if (typeof arg == "object" && META in arg) return `<${arg.type ? toString(arg.type) : "meta"}>`;
  if (Array.isArray(arg)) {
    return `[${arg.map(toString)}]`;
  }

  if (typeof arg === "object" && "customToString" in arg)
    return arg.customToString({ toString });
  // if (arg instanceof Binding) return arg.toString();
  if (typeof arg === "function") return arg.name;
  if (typeof arg === "object" && typeof arg.value === "string")
    return arg.value; // assume token
  if (typeof arg === "object") return JSON.stringify(arg);
  return String(arg);
};
export const pretty = (str) => {
  let indent = -1;
  const stack = [false];
  return toString(str).replace(/[[\]]|,\s*/g, (x) => {
    if (x === "[") {
      const s = stack[stack.length - 1];
      stack[stack.length - 1] = true;
      stack.push(false);
      indent++;
      return `${s ? "" : "\n" + "  ".repeat(indent)}[`;
    } else if (x === "]") {
      stack.pop();
      indent--;
      return "]";
    } else {
      if (!stack[stack.length - 1]) return ",";
      return `,\n${"  ".repeat(indent + 1)}`;
    }
  });
};

let vm: any = undefined;

const popValues = (num) => {
  if (vm.stack.length < num) throw new Error(`Expected ${num} values on stack`);
  return Array.from(new Array(num)).map(() => vm.stack.pop()).reverse() // prettier-ignore
};
const popStack = () => {
  if (vm.stack.length === 0) throw new Error(`Expected 1 value on stack`);
  return vm.stack.pop();
};
const expectMap = (object, key, message) => {
  if (!object[key]) throw new Error(message);
  return object[key];
};
const expect = (expected, message) => {
  if (!expected) throw new Error(message);
  return expected;
};

const instructions = {
  push: ({ value }) => vm.stack.push(value),
  nil: () => vm.stack.push(null),
  builtin: ({ name, args }) => {
    if (vm.stack.length < args) throw new Error("Not enough stack values");
    const values = popValues(args);
    if (builtins[name]) return vm.stack.push(builtins[name](...values));
    if (primitives[name]) return vm.stack.push(primitives[name](...values));
    throw new Error("No builtin " + name);
  },
  pop: () => vm.stack.pop(),
  binding: ({ name }) => {
    vm.stack.push(expectMap(vm.scope, name, `No binding ${name}`));
  },
  jumpf: ({ address }) => {
    if (!vm.stack.pop()) vm.ip = address;
  },
  deflocal: ({ name }) => {
    expect(!Object.hasOwn(vm.scope, name), `${name} already in scope`);
    vm.scope[name] = popStack();
  },
  setlocal: ({ name }) => {
    expect(Object.hasOwn(vm.scope, name), `${name} not existing in scope`);
    vm.scope[name] = popStack();
  },
  jump: ({ address }) => void (vm.ip = address),
  call: ({ args, targs }) => {
    const values = popValues(args);
    const typeArgs = popValues(targs || 0);
    const func = values.shift();
    if (func._function) {
      const functionResult = func._function(...values);
      vm.stack.push(functionResult);
      return;
    }
    if (func._userfunction || func._closure) {
      const functionResult = functionCompileTimeTypeCheckAndCompile(func, typeArgs, values); // prettier-ignore
      vm.stack.push(functionResult);
      return;
    }
    throw new Error("Value is not a function");
  },
  callt: ({ args }) => {
    const values = popValues(args);
    const [func, typeArgs, params] = values;
    if (!typeArgs._tuple) throw new Error("Expected tuple");
    if (!params._tuple) throw new Error("Expected tuple");

    if (func._function)
      throw new Error("Dynamic function does not take type arguments");

    if (func._userfunction || func._closure) {
      const functionResult = functionCompileTimeTypeCheckAndCompile(func, typeArgs.values, params.values); // prettier-ignore
      vm.stack.push(functionResult);
      return;
    }
    throw new Error("Value is not a function");
  },
  operator: ({ name, args }) => {
    const values = popValues(args);
    if (!operators[name]) throw new Error(`Invalid operator ${name}`);
    const operatorResult = operators[name](...values);
    vm.stack.push(operatorResult);
  },
  closure: ({ id }) => {
    const func = functionDefs[id];
    console.log("func", func);

    if (Object.hasOwn(vm.scope, func.name))
      throw new Error(`${name} already in scope`);
    console.log("Create closure", func, vm.scope);
    vm.scope[func.name] = { _closure: true, func, scope: vm.scope };
  },
  tuple: ({ args }) => vm.stack.push({ _tuple: true, values: popValues(args) }),
  pushqs: () => compilerState.quoteStack.push([]),
  popqs: () => vm.stack.push(stmts(compilerState.quoteStack.pop())),
  appendq: () => {
    const value = vm.stack.pop();
    compilerState.quoteStack[compilerState.quoteStack.length - 1].push(value);
    vm.stack.push(null); // needed for statements
  },
};

const executeBytecode = (bytecode, vm) => {
  let current = bytecode[vm.ip];
  while (current.type !== "halt") {
    const startIp = vm.ip;
    const instr = instructions[current.type];
    if (!instr) throw new Error("No instruction " + current.type);
    instr(current);
    if (vm.ip === startIp) vm.ip++;
    current = bytecode[vm.ip];
  }
};

let log = [];
const compilerLog = (title, details) => {
  log.push(`<details>
<summary>${title}</summary>
${details}
</details>`);
  updateView();
};
// updateView();
// compilerState.compType = "default";

compilerLog("Initial expr", pretty(toString(sexpr2)));
expandTopLevel(sexpr2, rootScope);
const func = rootScope["my_func"];
functionTemplateTypeCheckAndCompile(func, [], []);

compilerLog(
  "Compiled functions",
  [...compiledFunctions.values()].map((x) => `${x.binding.name}`).join("\n")
);

const bytecodeWriter = {
  functions: [],
};

const OpCodes = {
  Nil: 0,
  True: 1,
  False: 2,
  And: 3,
  Or: 4,
  Pop: 5,
  Print: 6,
  StringFormat: 7,
  Jump: 8,
  JumpIfFalse: 9,
  JumpIfFalsePop: 10,
  Loop: 11,
  Call: 12,
  Return: 13,
  ConstantV: 14,
  ConstantF: 15,
  ConstantD: 16,
  ConstantI: 17,
  GetLocalV: 18,
  GetLocalF: 19,
  GetLocalD: 20,
  GetLocalI: 21,
  SetLocalV: 22,
  SetLocalF: 23,
  SetLocalD: 24,
  SetLocalI: 25,
  EqualV: 26,
  EqualF: 27,
  EqualD: 28,
  EqualI: 29,
  GreaterV: 30,
  GreaterF: 31,
  GreaterD: 32,
  GreaterI: 33,
  LessV: 34,
  LessF: 35,
  LessD: 36,
  LessI: 37,
  AddV: 38,
  AddF: 39,
  AddD: 40,
  AddI: 41,
  SubtractV: 42,
  SubtractF: 43,
  SubtractD: 44,
  SubtractI: 45,
  MultiplyV: 46,
  MultiplyF: 47,
  MultiplyD: 48,
  MultiplyI: 49,
  DivideV: 50,
  DivideF: 51,
  DivideD: 52,
  DivideI: 53,
  NotV: 54,
  NotF: 55,
  NotD: 56,
  NotI: 57,
  NegateV: 58,
  NegateF: 59,
  NegateD: 60,
  NegateI: 61,
  ToStringV: 62,
  ToStringF: 63,
  ToStringD: 64,
  ToStringI: 65,
};
const operatorMap = {
  "+": "Add",
  "-": "Subtract",
  "*": "Multiply",
  "/": "Divide",
  "==": "Equal",
  ">": "Greater",
  "<": "Less",
};

const writeFinalBytecodeFunctions = (functions) => {
  console.log(functions);

  functions.forEach((func) => {
    const locals = new Map();
    let nextLocalSlot = 0;

    const funcBytecode = [];

    func.func.args.forEach(([name], i) => {
      locals.set(func.argBindings[i], locals.size);
    });
    const constants = new Map();
    const constantSlots = [];
    let nextConstantSlot = 0;

    console.log(func);
    const writeBytes = (...values) => {
      values.forEach((x) => expect(x < 2 ** 8, `Expected ${x} < 256`));
      funcBytecode.push(...values);
    };
    const writeJump = (type) => {
      writeBytes(type, 0, 0);
      const jump = funcBytecode.length;
      return () => writeLittleEndian16At(funcBytecode, jump - 2, funcBytecode.length - jump); // prettier-ignore
    };
    function writeLittleEndian16At(arr, offset, number) {
      expect(number < 2 ** 16);
      arr[offset] = number & 0xff; // Write the least significant byte
      arr[offset + 1] = (number >> 8) & 0xff; // Write the most significant byte
    }
    function writeLittleEndian32At(arr, offset, number) {
      arr[offset] = number & 0xff; // Least significant byte
      arr[offset + 1] = (number >> 8) & 0xff;
      arr[offset + 2] = (number >> 16) & 0xff;
      arr[offset + 3] = (number >> 24) & 0xff; // Most significant byte
    }
    const arrayBuffer = new Uint32Array(2);
    function writeDoubleLittleEndian(arr, offset, number) {
      let dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength); // prettier-ignore
      dataView.setFloat64(0, number, true);
      arr[offset + 0] = arrayBuffer[0];
      arr[offset + 1] = arrayBuffer[1];
      console.log(dataView, arrayBuffer);
    }
    function writeUint32LittleEndian(arr, offset, number) {
      let dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength); // prettier-ignore
      dataView.setUint32(0, number, true);
      arr[offset] = arrayBuffer[0];
      console.log(dataView, arrayBuffer);
    }
    const writeTypeAt = (arr, offset, type, value) => {
      expect(type === IntType);
      writeUint32LittleEndian(arr, offset, value);
    };
    const writeOperator = (op, type) => {
      if (op === ">=") {
        writeOperator("<", type);
        writeBytes(OpCodes.NotI);
        return;
      } else if (op === "<=") {
        writeOperator(">", type);
        writeBytes(OpCodes.NotI);
        return;
      }
      if (op === "or") {
        expect(type === IntType);
        writeBytes(OpCodes.Or);
      } else if (op === "and") {
        expect(type === IntType);
        writeBytes(OpCodes.And);
        return;
      }
      const s = type === IntType ? 'I' : type === FloatType ? 'F' : type === DoubleType ? 'D' : null; // prettier-ignore
      expect(operatorMap[op] !== undefined);
      const bytecode = OpCodes[`${operatorMap[op]}${s}`];
      expect(bytecode !== undefined);
      writeBytes(bytecode);
    };
    const slotSize = (type) => {
      expect(type === IntType);
      return 1;
    };
    const writeExpr = (expr) => {
      const asts = {
        statements: (ast) =>
          ast.values.forEach((expr, i) => {
            writeExpr(expr);
            if (i !== ast.values.length - 1 && expr.type !== VoidType)
              writeBytes(OpCodes.Pop, slotSize(expr.type));
          }),
        binding: (ast) => {
          let index = locals.get(ast.values[0]);
          expect(index !== undefined, "Expected binding");
          expect(ast.values[0].type === IntType);
          // if (index === undefined) index = 69; // closure variables
          // expect(index !== undefined, `Expected local ${ast.values[0].name}`);
          writeBytes(OpCodes.GetLocalI, index);
          console.log(ast);
        },
        def: (ast) => {
          const [binding, type, value] = ast.values;
          locals.set(binding, nextLocalSlot);
          nextLocalSlot += slotSize(type);
          writeExpr(value);
        },
        set: (ast) => {
          const [binding, value] = ast.values;
          if (binding.type !== IntType) throw new Error(`Not implemented type ${type._type}`); // prettier-ignore
          const index = locals.get(binding);
          expect(index !== undefined);
          writeExpr(value);
          writeBytes(OpCodes.SetLocalI, index);
        },
        number: (ast) => {
          const [value] = ast.values;
          let index = constants.get(value);
          if (index === undefined) {
            index = nextConstantSlot;
            nextConstantSlot += slotSize(ast.type); // double is 2 slots. TODO: Fix this when other types are in constants
            constants.set(value, index);
            writeTypeAt(constantSlots, constantSlots.length, ast.type, value);
          }
          writeBytes(OpCodes.ConstantI, index);
        },
        if: (ast) => {
          const [expr, trueAst, falseAst] = ast.values;
          writeExpr(expr);
          const patch1 = writeJump(OpCodes.JumpIfFalse);
          writeExpr(trueAst);
          const patch2 = writeJump(OpCodes.Jump);
          patch1();
          writeExpr(falseAst);
          patch2();
        },
        and: (ast) => {
          const [a, b] = ast.values;
          writeExpr(a);
          const patch = writeJump(OpCodes.JumpIfFalse);
          writeBytes(OpCodes.Pop, slotSize(a.type));
          writeExpr(b);
          patch();
        },
        or: (ast) => {
          const [a, b] = ast.values;
          writeExpr(a);
          const patch1 = writeJump(OpCodes.JumpIfFalse);
          const patch2 = writeJump(OpCodes.Jump);
          patch1();
          writeBytes(OpCodes.Pop, slotSize(a.type));
          writeExpr(b);
          patch2();
        },
        call: (ast) => {
          const [name, params] = ast.values;
          console.log(ast);
          if (name === "print") {
            expect(params.length === 1);
            params.forEach(writeExpr);
            if (params[0].type === IntType) writeBytes(OpCodes.ToStringI);
            else expect(false, `Unsupported ${params[0].type._type}`);
            writeBytes(OpCodes.Print, 1);
            return;
          }
          expect(false, "Not supported");
          // TODO: func name
          // params.forEach(writeExpr);
          // writeBytes(OpCodes.Call, params.length);
        },
        usercall: (ast) => {
          const [binding, params] = ast.values;
          const index = functions.findIndex((x) => x.binding === binding);
          expect(index !== undefined, "Expected function");
          console.log(ast);
          // TODO: func name
          params.forEach(writeExpr);
          writeBytes(OpCodes.Call, index, params.length);
        },
        operator: (ast) => {
          writeExpr(ast.values[1]);
          writeExpr(ast.values[2]);
          console.log({ operator: ast });
          // writeBytes(operatorToBytecode(ast.values[0], ast.values[1].type));
          writeOperator(ast.values[0], ast.values[1].type);
        },
      };
      if (!asts[expr.ast]) console.log(expr);
      if (!asts[expr.ast]) throw new Error(`Not implemented ${expr.ast}`);
      asts[expr.ast](expr);
    };
    writeExpr(func.ast);
    writeBytes(OpCodes.Return);
    const argSlots = Object.values(func.concreteTypes).reduce((acc, x) => acc + slotSize(x), 0); // prettier-ignore
    const returnSlots = slotSize(func.returnType);
    console.log({ constants });
    bytecodeWriter.functions.push({
      argSlots,
      returnSlots,
      constants: constantSlots,
      bytecode: funcBytecode,
    });
  });
};

writeFinalBytecodeFunctions([...compiledFunctions.values()].slice(0, 3));

console.log(JSON.stringify(bytecodeWriter, null, 2));
// const finalBytecode = [];
// finalBytecode.push(
//   bytecodeWriter.functionLengths.length,
//   ...bytecodeWriter.functionLengths
// );
// bytecodeWriter.functions.forEach((f) => finalBytecode.push(...f.bytecode));
// console.log(finalBytecode);

// writeBytecode(func.templateBytecode, res);

// updateView();
// executeBytecode(bytecode, vm);
// updateView();

// if (vm.stack.length !== 1)
//   throw new Error("Expected 1 value on stack got " + vm.stack.length);

// ast = vm.stack.pop();
// updateView();
// console.log("result", vm.stack);
// console.log("constants", vm.constants);

function bytecodeToString(bytecode) {
  const instr = (instr) => {
    const { type, ...args } = instr;
    const values = Object.entries(args)
      .map(([k, v]) => `${k}: ${v}`)
      .join(", ");
    return `${type.padStart("operator".length, " ")}  ${values}`;
  };
  return bytecode
    .map((x, i) => `${String(i).padStart(3, " ")}  ${instr(x)}`)
    .join("\n");
}

function updateView() {
  // document.getElementById("app").innerHTML = `<pre>${log.join("\n")}</pre>`;
  //   document.getElementById("app").innerHTML = `

  // <pre>

  // ${pretty(toString(sexpr))}

  // <details>
  // <summary>Expanded form</summary>
  // ${pretty(toString(res))}

  // </details>

  // <details>
  // <summary>Ast</summary>
  // ${JSON.stringify(ast, null, 2)}
  // </details>
  // </pre>
  // `;
}
