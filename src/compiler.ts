type NodeType = 'Number' | 'Symbol' | 'String' | 'Parens' | 'Invalid';

interface SourceLocation {
  line: number;
  column: number;
}
type Token = { value: string, location: SourceLocation }



class IrStatements { key = 'statements' as const; constructor(public token: Token, public exprs: IrAst[]) {} }
class IrLet {        key = 'let' as const;        constructor(public token: Token, public name: string, public type: IrAst, public value: IrAst) {} }
class IrSet {        key = 'set' as const;        constructor(public token: Token, public name: string, public value: IrAst) {} }
class IrOperator {   key = 'operator' as const;   constructor(public token: Token, public exprs: IrAst[]) {} }
class IrName {       key = 'name' as const;       constructor(public token: Token) {} }
class IrSymbol {     key = 'symbol' as const;     constructor(public token: Token) {} }
class IrNumber {     key = 'number' as const;     constructor(public token: Token) {} }
class IrMeta {       key = 'meta' as const;       constructor(public token: Token, public expr: IrAst) {} }
class IrLetConst {   key = 'letconst' as const;   constructor(public token: Token, public name: string, public value: IrAst) {} }
class IrCall {       key = 'call' as const;       constructor(public token: Token, public exprs: IrAst[]) {} }
class IrList {       key = 'list' as const;       constructor(public token: Token, public exprs: IrAst[]) {} }
class IrOr {         key = 'or' as const;         constructor(public token: Token, public exprs: IrAst[]) {} }
class IrAnd {        key = 'and' as const;        constructor(public token: Token, public exprs: IrAst[]) {} }
class IrIf {         key = 'if' as const;         constructor(public token: Token, public exprs: IrAst[]) {} }
class IrDefn {       key = 'defn' as const;       constructor(public token: Token, public id: number) {} }
type IrAst = IrStatements | IrLet | IrSet | IrOperator | IrName | IrSymbol | IrNumber | IrMeta | IrLetConst | IrCall | IrList | IrOr | IrAnd | IrIf | IrDefn

let allTokens: Token[] = []
let tokenId = 0
function parse(tokenizer: Generator<Token, void, unknown>): IrAst {

  function advance() {
    previous = currentToken
    const res = tokenizer.next()
    if (res.done) {
      currentToken = undefined!;
      return
    }
    currentToken = res.value as any;

    allTokens.push(currentToken)
    tokenId ++
    return currentToken
  }
  
  let previous: Token;
  let currentToken: Token
  advance()

  const expect = (str: string) => {
    if (currentToken?.value === str) return advance()
    throw new Error(`Expected ${str} got ${currentToken?.value}`)
  }

  const match = (str: string) => {
    if (currentToken.value === str) return advance()
    return false;
  }

  const parseExprList = () => {
    const exprs: IrAst[] = []
    while (currentToken && currentToken.value !== ')' && currentToken.value !== ']') {
      exprs.push(parseSExpr());
    }
    return exprs;
  }

  const expectTrailingParen = <T>(expr: T) => (expect(")"), expr);
  const expectTrailingSquare = <T>(expr: T) => (expect("]"), expr);

  const parseList = () => expectTrailingSquare(new IrList(previous, parseExprList()))
  const parseDefn = () => {
    const id = addFunctionDefinition({ 
      name: parseName(),
      typeArgs: (expect('['), parseList().exprs), 
      args: (expect('['), parseList().exprs),
      return: parseSExpr(), body: parseSExpr()
    })
    return expectTrailingParen(new IrDefn(previous, id))
  }
  const isAlpha = (char) => char.toLowerCase() >= 'a' && char.toLowerCase() <= 'z'
  const isNumber = (value: string) => !isNaN(Number(value))

  function parseSExpr(): IrAst {

    if (!currentToken) throw new Error()
    const { value: token, location } = currentToken;

    if (token === ')') throw new Error("Expected expr got )")
    
    if (match('(')) {
      if (!currentToken) throw new Error("")
      
      if (match('statements'))    return expectTrailingParen(new IrStatements(previous, parseExprList()))
      else if (match('let'))      return expectTrailingParen(new IrLet(previous, parseName(), new IrMeta(previous, parseSExpr()), parseSExpr()))
      else if (match('set'))      return expectTrailingParen(new IrSet(previous, parseName(), parseSExpr()))
      else if (match('+'))        return expectTrailingParen(new IrOperator(previous, parseExprList()))
      else if (match('*'))        return expectTrailingParen(new IrOperator(previous, parseExprList()))
      else if (match('/'))        return expectTrailingParen(new IrOperator(previous, parseExprList()))
      else if (match('-'))        return expectTrailingParen(new IrOperator(previous, parseExprList()))
      else if (match('or'))       return expectTrailingParen(new IrOr(previous, parseExprList()))
      else if (match('and'))      return expectTrailingParen(new IrAnd(previous, parseExprList()))
      else if (match('=='))       return expectTrailingParen(new IrOperator(previous, parseExprList()))
      else if (match('>='))       return expectTrailingParen(new IrOperator(previous, parseExprList()))
      else if (match('if'))       return expectTrailingParen(new IrIf(previous, parseExprList()))
      else if (match('letconst')) return expectTrailingParen(new IrLetConst(previous, parseName(), parseSExpr()))
      else if (match('meta'))     return expectTrailingParen(new IrMeta(previous, parseSExpr()))
      else if (match('call'))     return expectTrailingParen(new IrCall(previous, parseExprList()))
      else if (match('defn'))     return parseDefn()
      else throw new Error(`Not found ${currentToken.value}`)
    }
    else if (match('['))        return expectTrailingSquare(new IrList(previous, parseExprList()))
    else if (isNumber(token))   return (advance(), new IrNumber(previous))
    else if (isAlpha(token[0])) return (advance(), new IrSymbol(previous))
    throw new Error(`Unexpected ${token}`)
  }
  const parseName = () => {
    const current = currentToken;
    if (!current) throw new Error()
    if (current.value === '(' || isNumber(current.value)) throw new Error();
    
    advance();
    return current.value
  }

  return parseSExpr();
}

function* tokenizeWithLocation(input: string): Generator<Token, void, unknown> {
  let currentToken = '';
  let line = 1;
  let column = 1;

  for (let ch of input) {
    if (ch === '\n') {
      if (currentToken) {
        yield { value: currentToken, location: { line, column: column - currentToken.length } };
        currentToken = ''
      }
      line++;
      column = 1;
      continue;
    }

    if (ch === '(' || ch === ')' || ch === '[' || ch === ']') {
      if (currentToken) {
        yield { value: currentToken, location: { line, column: column - currentToken.length } };
        currentToken = '';
      }
      yield { value: ch, location: { line, column } };
    } else if (ch === ' ') {
      if (currentToken) {
        yield { value: currentToken, location: { line, column: column - currentToken.length } };
        currentToken = '';
      }
    } else {
      currentToken += ch;
    }
    column++;
  }

  if (currentToken) {
    yield { value: currentToken, location: { line, column: column - currentToken.length } };
  }
}


interface BytecodeOut {
  bytecode: {
    code: any[]
    locations: SourceLocation[]
  }
  table: any
}

const pushBytecode = (out: BytecodeOut, token: Token, instr) => {
  out.bytecode.locations.push(token.location);
  out.bytecode.code.push(instr)
}

const writeBytecode = (out: BytecodeOut, expr: IrAst) => {
  const table = out.table
  if (!table[expr.key]) throw new Error(`Not found ${expr.key}`);
  table[expr.key](out, expr)
}
const writeAll = (out: BytecodeOut, exprs) => {
  exprs.forEach(expr => writeBytecode(out, expr))
}

type FunctionDef = {
  name: string
  typeArgs: IrAst[]
  args: IrAst[]
  return: IrAst
  body: IrAst

  headerPrototype?: FunctionPrototype | undefined
  templatePrototype?: FunctionPrototype | undefined
  compileTimePrototype?: FunctionPrototype | undefined
}
const functionDefs: FunctionDef[] = []
const addFunctionDefinition = (defn: FunctionDef) => {
  functionDefs.push(defn)
  return functionDefs.length - 1
}

type MetaInstructionTable = {
    [E in IrAst as E['key']]: (out: BytecodeOut, ast: E) => void;
}

const bytecodeDefault: MetaInstructionTable = {
  symbol: (out, ast) => pushBytecode(out, ast.token, { type: "binding", name: ast.token.value }), // prettier-ignore
  number: (out, ast) => pushBytecode(out, ast.token, { type: "push", value: Number(ast.token.value) }), // prettier-ignore
  name:   (out, ast) => pushBytecode(out, ast.token, { type: "name", name: ast.token.value }), // prettier-ignore

  operator: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operator', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  let:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'let', name: ast.name })), // prettier-ignore
  set:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'set', name: ast.name })), // prettier-ignore
  letconst: (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'let', name: ast.name })), // prettier-ignore
  meta:     (out, ast) => (writeBytecode(out, ast.expr)),

  list: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'list', count: ast.exprs.length })),
  // binding: (out, ast) => pushBytecode(out, { type: "binding", name }), // prettier-ignore
  defn: (out, ast) => pushBytecode(out, ast.token, { type: "closure", id: ast.id }), // prettier-ignore
  call: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "call", count: ast.exprs.length })), // prettier-ignore

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


const writeMeta = (out: BytecodeOut, expr: IrAst) => {
  writeBytecode({ bytecode: out.bytecode, table: bytecodeDefault }, expr)
}

const bytecodeSecond: MetaInstructionTable = {
  // string: (out, ast) => pushBytecode(out, ast.token, { type: "stringast", token: ast.value }), // prettier-ignore
  number: (out, ast) => pushBytecode(out, ast.token, { type: "numberast", token: ast.token.value }), // prettier-ignore
  name:   (out, ast) => pushBytecode(out, ast.token, { type: "nameast", name: ast.token.value }), // prettier-ignore
  symbol: (out, ast) => pushBytecode(out, ast.token, { type: "binding", name: ast.token.value }), // prettier-ignore
  // closure: (out, ast) => pushBytecode(out, { type: "closure", id }), // prettier-ignore

  // "callt*": (out, ast) => (writeAll(out, rest), pushBytecode(out, { type: "callt", args: rest.length })), // prettier-ignore
  // builtincall: (out, ast) => (writeAll(out, args), pushBytecode(out, { type: "builtin", name, args: args.length })), // prettier-ignore
  // tuple: (out, ast) => (writeAll(out, args), pushBytecode(out, { type: "tuple", args: args.length })), // prettier-ignore
  call:     (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "callast", count: ast.exprs.length })), // prettier-ignore
  let:      (out, ast) => (writeBytecode(out, ast.value), writeBytecode(out, ast.type), pushBytecode(out, ast.token, { type: 'letast', name: ast.name })), // prettier-ignore,
  set:      (out, ast) => (writeBytecode(out, ast.value), pushBytecode(out, ast.token, { type: 'setast', name: ast.name })), // prettier-ignore,
  operator: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operatorast', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  meta:     (out, ast) => writeMeta(out, ast.expr),
  letconst: (out, ast) => (writeMeta(out, ast.value), pushBytecode(out, ast.token, { type: 'letlocal', name: ast.name })),

  defn: (out, ast) => pushBytecode(out, ast.token, { type: "closure", id: ast.id }), // prettier-ignore

  list: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'listast', count: ast.exprs.length })),

  and: (out, ast) => (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "andast", count: ast.exprs.length })),
  or: (out, ast) =>  (writeAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "orast", count: ast.exprs.length })),

  statements: (out, ast) => {
    const isMeta = (ast) => new Set(['letconst', 'defn']).has(ast.key); // dont like this hacks
    pushBytecode(out, ast.token, { type: "pushqs" });
    ast.exprs.forEach((stmt, i) => {
      writeBytecode(out, stmt);
      if (!isMeta(stmt)) pushBytecode(out, ast.token, { type: "appendq" });
      pushBytecode(out, ast.token, { type: "pop" }); // Even pop the final value
    });
    pushBytecode(out, ast.token, { type: "popqs" });
  },

  if: (out, ast) => {
    writeBytecode(out, ast.exprs[0]), writeBytecode(out, ast.exprs[1]), writeBytecode(out, ast.exprs[2])
    pushBytecode(out, ast.token, { type: "ifast" });
  }
};

const printLocation = (loc: SourceLocation) => {
  return `${loc.line}:${loc.column}`
}

const createBinding = (name: string, type): Binding => ({ _binding: true, name, type });

const compiledFunctions = new Map();


type BytecodeGen = {
  code: any[]
  locations: SourceLocation[]
}
type FunctionPrototype = {
  name: string
  bytecode?: BytecodeGen | undefined
  // function: FunctionDef,
  body: IrAst
  instructionTable: any
}

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

  console.log(bytecodeToString(prototype.bytecode))
  // compilerLog(prototype.name, bytecodeToString(prototype.bytecode)); // prettier-ignore
  return prototype.bytecode;
};

type Vm = {
  ip: number,
  stack: unknown[],
  scope: Scope
}
type CompilerState = {
  vm: Vm,
  func: FunctionDef,
  quoteStack: IrAst[][]
}

let compilerState: CompilerState
const executePrototype = (func: FunctionDef, prototype: FunctionPrototype, scope: Scope) => {
  if (!scope) throw new Error("Expected scope");

  const newVm: Vm = {
    ip: 0,
    stack: [],
    scope,
  };
  const newCompilerState: CompilerState = {
    vm: newVm,
    func,
    quoteStack: [],
  };
  const prevCompilerState = compilerState;
  compilerState = newCompilerState;

  vm = newVm;
  executeBytecode(prototype.bytecode.code, vm);
  compilerState = prevCompilerState;
  if (vm.stack.length !== 1) {
    console.log(vm.stack)
    throw new Error("Expected 1 value on stack at end of function. Got " + vm.stack.length);
  }

  const result = vm.stack.pop();
  vm = compilerState ? compilerState.vm : undefined;
  return result;
};

type Binding = {
  _binding: true
  name: string
  type: Type
}
const functionTemplateTypeCheckAndCompile = (
  func: FunctionDef,
  typeArgs: any[],
  args: any[],
  parentScope: Scope
) => {
  if (!func.headerPrototype) {
    // const body = ["compileArgs", func.args];
    const body = null // TODO
    func.headerPrototype = { name: `${func.name} header`, body, instructionTable: bytecodeDefault }; // prettier-ignore
  }
  const concreteTypes = {};
  const __expectArg = (name, value, expected) => {
    if (value !== expected) throw new Error(`Argument ${name} of type ${value} does not match ${expected}`) // prettier-ignore
    concreteTypes[name] = expected;
  };
  const scope = Object.create(parentScope);
  Object.assign(scope, { __expectArg: { _function: __expectArg } });
  if (args.length !== func.args.length) throw new Error(`Expected ${func.args.length} args got ${args.length}`); // prettier-ignore
  args.forEach((arg, i) => {
    scope[func.args[i][0]] = arg.type;
  });
  func.typeArgs.forEach((typeArg, i) => {
    // scope[typeArg] = typeArgs[i];
  });
  if (func.args.length) {
    compileFunctionPrototype(func.headerPrototype)
    executePrototype(func, func.headerPrototype!, scope);
  }
  if (!func.templatePrototype) 
    func.templatePrototype = { name: `${func.name} template bytecode`, body: func.body, instructionTable: bytecodeSecond }; // prettier-ignore
  const templateScope = Object.create(parentScope);
  const argBindings: Binding[] = [];
  func.args.forEach((arg, i) => {
    const binding = createBinding(arg[0], concreteTypes[arg[0]]);
    templateScope[arg[0]] = binding;
    argBindings.push(binding);
  });
  func.typeArgs.forEach((typeArg, i) => {
    // templateScope[typeArg] = typeArgs[i];
  });

  compileFunctionPrototype(func.templatePrototype);
  const ast = executePrototype(func, func.templatePrototype, rootScope)

  console.log(JSON.stringify(ast, null, 2))
  
  // const ast = compileAndExecutePrototype(func, func.templatePrototype, templateScope); // prettier-ignore
  // compilerLog(`${func.name} ast`, JSON.stringify(ast, null, 2));

  const binding = createBinding(`${func.name} compiled`, null);
  // const returnType = ast.type;
  const compiledFunction = {
    binding,
    func,
    returnType: null,
    concreteTypes,
    ast,
    argBindings,
  };
  compiledFunctions.set(binding, compiledFunction);
  return compiledFunction;
};

const functionCompileTimeCompile = (
  func: FunctionDef,
  typeArgs: any[],
  args: any[],
  parentScope: Scope
) => {
  if (func._closure) { parentScope = func.scope; func = func.func; } // prettier-ignore
  if (args.length !== func.args.length) throw new Error(`Expected ${func.args.length} args got ${args.length}`); // prettier-ignore
  if (!func.compileTimePrototype) 
    func.compileTimePrototype = { name: `${func.name} comptime bytecode`, body: func.body, instructionTable: bytecodeDefault }; // prettier-ignore

  const scope = Object.create(parentScope);
  args.forEach((arg, i) => {
    scope[func.args[i][0]] = arg;
  });
  func.typeArgs.forEach((typeArg, i) => {
    scope[typeArg] = typeArgs[i];
  });
  compileFunctionPrototype(func.compileTimePrototype)
  return executePrototype(func, func.compileTimePrototype, scope);
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

const createAst = (ast: string, type: Type, values: any[]) => ({ ast, type, values });


const VoidType = { _type: "void" };
const IntType = { _type: "int" };
const FloatType = { _type: "float" };
const DoubleType = { _type: "double" };
const StringType = { _type: "string" };

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
  operatorast: ({ name, count }) => vm.stack.push(createAst('operator', IntType, [name, popValues(count)])),
  numberast:({ token }) =>  vm.stack.push(createAst('number', IntType, [Number(token)])),
  letast: ({ name }) =>     vm.stack.push(createAst('let', VoidType, [name, popStack(), popStack()])),
  setast: ({ name }) =>     vm.stack.push(createAst('set', VoidType, [name, popStack()])),
  orast: ({ count }) =>     vm.stack.push(createAst('or', IntType, popValues(count))),
  andast: ({ count }) =>    vm.stack.push(createAst('and', IntType, popValues(count))),
  callast: ({ count }) =>   vm.stack.push(createAst('call', IntType, popValues(count))),
  listast: ({ count }) =>   vm.stack.push(createAst('list', IntType, popValues(count))),
  ifast: () =>              vm.stack.push(createAst('if', IntType, popValues(3))),

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
  call: ({ count, tcount }) => {
    const values = popValues(count);
    const typeArgs = popValues(tcount || 0);
    const func = values.shift();
    if (func._function) {
      const functionResult = func._function(...values);
      vm.stack.push(functionResult);
      return;
    }
    if (func._userfunction || func._closure) {
      const functionResult = functionCompileTimeCompile(func, typeArgs, values, rootScope); // prettier-ignore
      vm.stack.push(functionResult);
      return;
    }
    throw new Error("Value is not a function");
  },
  callt: ({ count }) => {
    const values = popValues(count);
    const [func, typeArgs, params] = values;
    if (!typeArgs._tuple) throw new Error("Expected tuple");
    if (!params._tuple) throw new Error("Expected tuple");

    if (func._function)
      throw new Error("Dynamic function does not take type arguments");

    if (func._userfunction || func._closure) {
      const functionResult = functionCompileTimeCompile(func, typeArgs.values, params.values, rootScope); // prettier-ignore
      vm.stack.push(functionResult);
      return;
    }
    throw new Error("Value is not a function");
  },
  operator: ({ name, count }) => {
    const values = popValues(count);
    if (!operators[name]) throw new Error(`Invalid operator ${name}`);
    const operatorResult = operators[name](...values);
    vm.stack.push(operatorResult);
  },
  closure: ({ id }) => {
    const func = functionDefs[id];
    console.log("func", func.name);

    if (Object.hasOwn(vm.scope, func.name))
      throw new Error(`${func.name} already in scope`);
    // console.log("Create closure", func, vm.scope);
    vm.scope[func.name] = { _closure: true, func, scope: vm.scope };
  },
  tuple: ({ count }) => vm.stack.push({ _tuple: true, values: popValues(count) }),
  pushqs: () => compilerState.quoteStack.push([]),
  popqs: () => vm.stack.push(makeStmts(compilerState.quoteStack.pop())),
  appendq: () => {
    const value = vm.stack.pop();
    compilerState.quoteStack[compilerState.quoteStack.length - 1].push(value);
    vm.stack.push(null); // needed for statements
  },
};


const makeStmts = (list: any[]) => {
  expect(list.length > 0, "Expected statements");
  return createAst("statements", list[list.length - 1].type, list);
};

const executeBytecode = (bytecode: any[], vm: Vm) => {
  let current = bytecode[vm.ip];
  while (current.type !== "halt") {
    const startIp = vm.ip;
    const instr = instructions[current.type];
    if (!instr) {
      console.log(current)
      throw new Error("No instruction " + current.type);
    }
    instr(current);
    if (vm.ip === startIp) vm.ip++;
    current = bytecode[vm.ip];
  }
};

// compileFunctionPrototype(prototype);

const input = `
  (statements
    (defn main [] [] int (statements
      (let x int (+ 3 2))
      (letconst foo (+ 32 42))

      (defn bam [] [a int b int] int (statements
        (call print (+ a b))))

      (let f int (meta (+ bar 3)))
      (set f (* 32 2))
      (call print (or (== 3 25) (>= 2 3)))
      (let foo int [])
      (let x int (if (== 2 3) 8 9))
      ))
      
      
    )`;
const tokenizer = tokenizeWithLocation(input);
const ast = parse(tokenizer);
console.log(JSON.stringify(ast, null, 2))


const rootScope = {
  int: IntType,
  float: FloatType,
  double: DoubleType,
  void: VoidType,
  string: StringType,
  compfoo: { _function: (a, b) => 65 + a + b },
  bar: 123,
  print: {
    _funcname: 'print',
    _function: (...args) => {
      console.log("print called", ...args);
      return args[0];
    },
  },
};

ast.exprs.forEach(expr => {
  if (expr.key === 'defn') {

    const func = functionDefs[expr.id]
    console.log("Global func", func.name)
    rootScope[func.name] = func;


  //   const outSecondOrder: BytecodeOut = {
  //     bytecode: {
  //       code: [] as any[],
  //       locations: [] as any[]
  //     },
  //     table: bytecodeSecond
  //   }

  //   writeBytecode(outSecondOrder, functionDefs[expr.id].body);
  //   console.log(bytecodeToString(outSecondOrder.bytecode))

  //   const outDefault: BytecodeOut = {
  //     bytecode: {
  //       code: [] as any[],
  //       locations: [] as any[]
  //     },
  //     table: bytecodeDefault
  //   }

  //   console.log("\n")
  //   writeBytecode(outDefault, functionDefs[expr.id].body);
  //   console.log(bytecodeToString(outDefault.bytecode))
    return

  }
  throw new Error(`Not supported ${expr.key}`)
})

const func: FunctionDef = rootScope["main"];
functionTemplateTypeCheckAndCompile(func, [], [], rootScope);



function bytecodeToString(bytecodeGen) {
  const {locations, code} = bytecodeGen
  const instr = (instr) => {
    const { type, ...args } = instr;
    const values = Object.entries(args)
      .map(([k, v]) => `${k}: ${v}`)
      .join(", ");
    return `${type.padStart("operatorast".length, " ")}  ${values}`;
  };
  return code
    .map((x, i) => `${String(`${locations[i].line}:`).padStart(5, " ")}${String(`${locations[i].column}`).padEnd(3, " ")} ${String(i).padStart(3, " ")}  ${instr(x)}`)
    .join("\n");
}
