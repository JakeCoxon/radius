import { Event } from "./tasks";

export function compilerAssert(expected: unknown, message: string="", info: object={}): asserts expected {
  if (expected) return;
  let out = message.replace(/\$([a-z]+)/g, (match, capture) => { 
    const obj = info[capture]
    if (obj === undefined || obj === null) return makeColor(`null`)
    if (obj[Bun.inspect.custom]) return Bun.inspect(obj, { depth: 0, colors: true });
    if (typeof obj !== 'object') return Bun.inspect(obj, { depth: 0, colors: true });
    if (obj.constructor) return makeColor(`[${obj.constructor.name}]`)
    return Bun.inspect(obj)
  }); 
  console.log(Bun.inspect(info, { depth: 2, colors: true }))
  throw new Error(out)
}

const makeColor = (x) => {
  return Bun.inspect({
    [Bun.inspect.custom](depth, options, inspect) {
      return options.stylize(x, 'special');
    }
  }, { colors: true })
}

export class SourceLocation {
  constructor(public line: number, public column:number) {}
  [Bun.inspect.custom](depth, options, inspect) {
    return options.stylize(`[SourceLocation ${this.line}:${this.column}]`, 'special');
  }
}

export type Token = { value: string, type: string, location: SourceLocation }

const TokenRoot = {
  [Bun.inspect.custom](depth, options, inspect) {
    return options.stylize(`[Token ${this.value}]`, 'string');
  }
}
export const createToken = (value: any, type = "NONE"): Token => Object.assign(Object.create(TokenRoot), { value, type, location: new SourceLocation(0, 0) });

export type ArgumentTypePair = [ParseIdentifier, ParseNode | null];

// These are reference types that id will be filled in later.
export type ParserFunctionDecl = {
  id: number | undefined, debugName: string,
  token: Token, functionMetaName: ParseIdentifier | null,
  name: ParseIdentifier | null, typeArgs: ParseNode[], args: ArgumentTypePair[], 
  returnType: ParseNode | null, body: ParseNode | null, keywords: ParseNode[] }
  
export type ParserClassDecl = {
  id: number | undefined, debugName: string,
  token: Token, metaType: ParseIdentifier | null,
  name: ParseIdentifier | null, typeArgs: ParseNode[], 
  body: ParseNode | null, keywords: ParseNode[] }

class ParseNodeType {
  key: unknown;
  [Bun.inspect.custom](depth, options, inspect) {
    if (depth <= 0) return options.stylize(`[${this.constructor.name}]`, 'special');
    const newOptions = Object.assign({}, options, {
      ast: true,
      depth: options.depth === null ? null : options.depth - 1,
    });

    const props = {...this}
    delete props.key;
    return `${options.stylize(this.constructor.name, 'special')} ${inspect(props, newOptions)}`
  }
}

export class ParseIdentifier extends ParseNodeType { key = 'identifier' as const; constructor(public token: Token) { super();} }
export class ParseSymbol extends ParseNodeType {     key = 'symbol' as const;     constructor(public token: Token) { super();} }
export class ParseNil extends ParseNodeType {        key = 'nil' as const;        constructor(public token: Token) { super();} }
export class ParseNumber extends ParseNodeType {     key = 'number' as const;     constructor(public token: Token) { super();} }
export class ParseString extends ParseNodeType {     key = 'string' as const;     constructor(public token: Token) { super();} }
export class ParseBoolean extends ParseNodeType {    key = 'boolean' as const;    constructor(public token: Token) { super();} }

export class ParseStatements extends ParseNodeType { key = 'statements' as const; constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseLet extends ParseNodeType {        key = 'let' as const;        constructor(public token: Token, public name: ParseIdentifier, public type: ParseNode | null, public value: ParseNode | null) { super();} }
export class ParseSet extends ParseNodeType {        key = 'set' as const;        constructor(public token: Token, public name: ParseNode, public value: ParseNode) { super();} }
export class ParseOperator extends ParseNodeType {   key = 'operator' as const;   constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseNote extends ParseNodeType {       key = 'note' as const;       constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseMeta extends ParseNodeType {       key = 'meta' as const;       constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseMetaIf extends ParseNodeType {     key = 'metaif' as const;     constructor(public token: Token, public expr: ParseIf) { super();} }
export class ParseMetaFor extends ParseNodeType {    key = 'metafor' as const;    constructor(public token: Token, public expr: ParseFor) { super();} }
export class ParseCompTime extends ParseNodeType {   key = 'comptime' as const;   constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseCall extends ParseNodeType {       key = 'call' as const;       constructor(public token: Token, public left: ParseNode, public args: ParseNode[], public typeArgs: ParseNode[]) { super();} }
export class ParseList extends ParseNodeType {       key = 'list' as const;       constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseListComp extends ParseNodeType {   key = 'listcomp' as const;   constructor(public token: Token, public exprs: ParseNode[], public mapping: ParseNode[], public reduce: ParseNode | null) { super();} }
export class ParseOr extends ParseNodeType {         key = 'or' as const;         constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseAnd extends ParseNodeType {        key = 'and' as const;        constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseElse extends ParseNodeType {       key = 'else' as const;       constructor(public token: Token, public body: ParseNode) { super();} }
export class ParseIf extends ParseNodeType {         key = 'if' as const;         constructor(public token: Token, public condition: ParseNode, public trueBody: ParseNode, public falseBody: ParseIf | ParseElse | null) { super();} }
export class ParseLetConst extends ParseNodeType {   key = 'letconst' as const;   constructor(public token: Token, public name: ParseIdentifier, public value: ParseNode) { super();} }
export class ParseFunction extends ParseNodeType {   key = 'function' as const;   constructor(public token: Token, public functionDecl: ParserFunctionDecl) { super();} }
export class ParseClass extends ParseNodeType {      key = 'class' as const;      constructor(public token: Token, public classDecl: ParserClassDecl) { super();} }
export class ParseReturn extends ParseNodeType {     key = 'return' as const;     constructor(public token: Token, public expr: ParseNode | null) { super();} }
export class ParseBreak extends ParseNodeType {      key = 'break' as const;      constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseContinue extends ParseNodeType {   key = 'continue' as const;   constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseFor extends ParseNodeType {        key = 'for' as const;        constructor(public token: Token, public identifier: ParseIdentifier, public expr: ParseNode, public body: ParseNode) { super();} }
export class ParseCast extends ParseNodeType {       key = 'cast' as const;       constructor(public token: Token, public expr: ParseNode, public as: ParseNode) { super();} }
export class ParseOpEq extends ParseNodeType {       key = 'opeq' as const;       constructor(public token: Token, public left: ParseNode, public right: ParseNode) { super();} }
export class ParseWhile extends ParseNodeType {      key = 'while' as const;      constructor(public token: Token, public condition: ParseNode, public body: ParseNode) { super();} }
export class ParseWhileExpr extends ParseNodeType {  key = 'whileexpr' as const;  constructor(public token: Token, public condition: ParseNode, public body: ParseNode) { super();} }
export class ParseForExpr extends ParseNodeType {    key = 'forexpr' as const;    constructor(public token: Token, public identifier: ParseIdentifier, public expr: ParseNode, public body: ParseNode) { super();} }
export class ParseNot extends ParseNodeType {        key = 'not' as const;        constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseField extends ParseNodeType {      key = 'field' as const;      constructor(public token: Token, public expr: ParseNode, public field: ParseIdentifier) { super();} }
export class ParseExpand extends ParseNodeType {     key = 'expand' as const;     constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseDict extends ParseNodeType {       key = 'dict' as const;       constructor(public token: Token, public pairs: [ParseNode, ParseNode][]) { super();} }
export class ParsePostCall extends ParseNodeType {   key = 'postcall' as const;   constructor(public token: Token, public expr: ParseNode, public arg: ParseNode) { super();} }
export class ParseSlice extends ParseNodeType {      key = 'slice' as const;      constructor(public token: Token, public expr: ParseNode, public a: ParseNode | null, public b: ParseNode | null, public c: ParseNode | null, public isStatic: boolean) { super();} }
export class ParseSubscript extends ParseNodeType {  key = 'subscript' as const;  constructor(public token: Token, public expr: ParseNode, public subscript: ParseNode, public isStatic: boolean) { super();} }
export class ParseTuple extends ParseNodeType {      key = 'tuple' as const;      constructor(public token: Token, public exprs: ParseNode[]) { super();} }

export type ParseNode = ParseStatements | ParseLet | ParseSet | ParseOperator | ParseIdentifier | 
  ParseNumber | ParseMeta | ParseCompTime | ParseLetConst | ParseCall | ParseList | ParseOr | ParseAnd | 
  ParseIf | ParseFunction | ParseString | ParseReturn | ParseBreak | ParseContinue | ParseFor | ParseCast |
  ParseOpEq | ParseWhile | ParseWhileExpr | ParseForExpr | ParseNot | ParseField | ParseExpand | ParseListComp |
  ParseDict | ParsePostCall | ParseSymbol | ParseNote | ParseSlice | ParseSubscript | ParseTuple | ParseClass |
  ParseNil | ParseBoolean | ParseElse | ParseMetaIf | ParseMetaFor

// Void types mean that in secondOrder compilation, the AST doesn't return an AST
export const isParseVoid = (ast: ParseNode) => ast.key == 'letconst' || ast.key === 'function' || ast.key === 'class' || ast.key === 'comptime';
export const isParseNode = (ast: unknown): ast is ParseNode => ast instanceof ParseNodeType

export type BytecodeInstr = 
  { type: 'binding', name: string } |
  { type: 'push', value: unknown } |
  { type: 'operator', name: string, count: number } |
  { type: 'letlocal', name: string, t: boolean, v: boolean } |
  { type: 'setlocal', name: string } |
  { type: 'list', count: number } |
  { type: 'tuple', count: number } |
  { type: 'closure', id: number } |
  { type: 'call', name: string, count: number, tcount: number } |
  { type: 'return', r: boolean } |
  { type: 'pop' } |
  { type: 'nil' } |
  { type: 'bindingast', name: string } |
  { type: 'numberast', value: number } |
  { type: 'stringast', value: string } |
  { type: 'boolast', value: boolean } |
  { type: 'setast', name: string } |
  { type: 'operatorast', name: string, count: number } |
  { type: 'toast' } |
  { type: 'whileast' } |
  { type: 'returnast', r: boolean } |
  { type: 'listast', count: number } |
  { type: 'andast', count: number } |
  { type: 'orast', count: number } |
  { type: 'ifast', f: boolean } |
  { type: 'letast', name: string, t: boolean, v: boolean } |
  { type: 'callast', name: string, count: number, tcount: number } |
  { type: 'pushqs' } |
  { type: 'popqs' } |
  { type: 'appendq' } |
  { type: 'jump', address: number } |
  { type: 'jumpf', address: number } |
  { type: 'halt' }


export type InstructionMapping = {
  [T in BytecodeInstr as T['type']]: (vm: Vm, instr: T) => void;
}


export type BytecodeGen = {
  code: BytecodeInstr[]
  locations: SourceLocation[]
}
export type FunctionPrototype = {
  name: string
  bytecode?: BytecodeGen | undefined
  // function: FunctionDef,
  body: ParseNode
  instructionTable: MetaInstructionTable
}

export const hashValues = (values: unknown[]) => {
  return values.map(value => {
    if (value instanceof Type) return `$${value.typeName}`
    if (typeof value === 'number') return value
    compilerAssert(false, "Cannot hash value", { value })
  }).join("__")
}

export interface BytecodeOut {
  bytecode: {
    code: BytecodeInstr[]
    locations: SourceLocation[]
  }
  table: MetaInstructionTable
  globalCompilerState: GlobalCompilerState // Not nice
}

export type MetaInstructionTable = {
  [E in ParseNode as E['key']]: (out: BytecodeOut, ast: E) => void;
}

export class CompiledFunction {
  constructor(
    public binding: Binding,
    public functionDefinition: FunctionDefinition,
    public returnType: Type,
    public concreteTypes: Type[],
    public body: Ast,
    public argBindings: Binding[],
    public typeParameters: unknown[],
    public typeParamHash: unknown) {}
}

export class FunctionDefinition {
  headerPrototype?: FunctionPrototype | undefined
  templatePrototype?: FunctionPrototype | undefined
  compileTimePrototype?: FunctionPrototype | undefined

  compiledFunctions: CompiledFunction[] = []

  constructor(
    public id: number,
    public debugName: string,
    public name: ParseIdentifier | null,
    public typeArgs: ParseNode[],
    public args: ArgumentTypePair[],
    public returnType: ParseNode | null,
    public body: ParseNode | null) {}

  [Bun.inspect.custom](depth, options, inspect) {
    if (options.ast) return options.stylize(`[FunctionDefinition ${this.name}]`, 'special');
    return {...this}
  }
}


export class AstRoot {
  [Bun.inspect.custom](depth, options, inspect) {
    if (depth <= 0) return options.stylize(`[${this.constructor.name}]`, 'special');
    const newOptions = Object.assign({}, options, {
      ast: true,
      depth: options.depth === null ? null : options.depth - 1,
    });
    return inspect({ast: this.constructor.name, ...this}, newOptions)
  }
}
export class NumberAst extends AstRoot {     constructor(public type: Type, public location: SourceLocation, public value: number) { super() } }
export class StringAst extends AstRoot {     constructor(public type: Type, public location: SourceLocation, public value: string) { super() } }
export class BindingAst extends AstRoot {    constructor(public type: Type, public location: SourceLocation, public binding: Binding) { super() } }
export class BoolAst extends AstRoot {       constructor(public type: Type, public location: SourceLocation, public value: boolean) { super() } }
export class LetAst extends AstRoot {        constructor(public type: Type, public location: SourceLocation, public binding: Binding, public value: Ast | null) { super() } }
export class SetAst extends AstRoot {        constructor(public type: Type, public location: SourceLocation, public binding: Binding, public value: Ast) { super() } }
export class OperatorAst extends AstRoot {   constructor(public type: Type, public location: SourceLocation, public operator: string, public args: Ast[]) { super() } }
export class IfAst extends AstRoot {         constructor(public type: Type, public location: SourceLocation, public expr: Ast, public trueBody: Ast, public falseBody: Ast | null) { super() } }
export class ListAst extends AstRoot {       constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class CallAst extends AstRoot {       constructor(public type: Type, public location: SourceLocation, public func: ExternalFunction, public args: Ast[]) { super() } }
export class UserCallAst extends AstRoot {   constructor(public type: Type, public location: SourceLocation, public binding: Binding, public args: Ast[]) { super() } }
export class AndAst extends AstRoot {        constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class OrAst extends AstRoot {         constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class StatementsAst extends AstRoot { constructor(public type: Type, public location: SourceLocation, public statements: Ast[]) { super() } }
export class WhileAst extends AstRoot {      constructor(public type: Type, public location: SourceLocation, public condition: Ast, public body: Ast) { super() } }
export class ReturnAst extends AstRoot {     constructor(public type: Type, public location: SourceLocation, public expr: Ast | null) { super() } }

export type Ast = NumberAst | LetAst | SetAst | OperatorAst | IfAst | ListAst | CallAst | AndAst | OrAst | StatementsAst | WhileAst | ReturnAst;
export const isAst = (value: unknown): value is Ast => value instanceof AstRoot;

export class Tuple {
  constructor(public values: unknown[]) {}
  [Bun.inspect.custom](depth, options, inspect) {
    return options.stylize(`[Tuple ...]`, 'special');
  }
}

export class Binding {
  constructor(public name: string, public type: Type) {}
  [Bun.inspect.custom](depth, options, inspect) {
    return options.stylize(`[Binding ${this.name} ${inspect(this.type)}]`, 'special');
  }
}

export class Type {
  constructor(public typeName: string) {}
  [Bun.inspect.custom](depth, options, inspect) {
    return options.stylize(`[Type ${this.typeName}]`, 'special');
  }
}
export class Closure {
  constructor(public func: FunctionDefinition, public scope: Scope) {}
}

export const ScopeEventsSymbol = Symbol('ScopeEventsSymbol')
export type Scope = object & {
  _scope: true,
  [ScopeEventsSymbol]: {[key:string]:Event<unknown, never>}
}
export const createScope = (obj: object) => obj as Scope;

export class ExternalFunction {
  constructor(public name: string, public func: Function) {}
  [Bun.inspect.custom](depth, options, inspect) {
    return options.stylize(`[ExternalFunction ${this.name}]`, 'special');
  }
}

export const VoidType = new Type("void")
export const IntType = new Type("int")
export const BoolType = new Type("bool")
export const FloatType = new Type("float")
export const DoubleType = new Type("double")
export const StringType = new Type("string")
export const FunctionType = new Type("function")

export const expectMap = (object: object, key: string, message: string, info: object = {}) => {
  compilerAssert(object[key] !== undefined, message, { object, key, ...info }); 
  return object[key];
};
export const expect = (expected: unknown, message: string, info: object = {}) => {
  compilerAssert(expected !== undefined, message, info); 
  return expected;
};
export const expectAll = <T>(fn: (x: unknown) => x is T, expected: unknown[], info: object = {}) => {
  compilerAssert(expected.every(fn), "Expected something got $expected", { expected, ...info }); 
  return expected;
};
export const expectAst = <T>(expected: unknown, info: object = {}) => {
  compilerAssert(isAst(expected), "Expected AST got $expected", { expected, ...info }); 
  return expected;
};
export const expectType = <T>(expected: unknown, info: object = {}) => {
  compilerAssert(expected instanceof Type, "Expected Type got $expected", { expected, ...info }); 
  return expected;
};
export const expectAsts = <T>(expected: unknown[], info: object = {}) => {
  compilerAssert(expected.every(isAst), "Expected ASTs got $expected", { expected, ...info }); 
  return expected;
};
export const createStatements = (location: SourceLocation, list: Ast[]) => {
  expect(list.length > 0, "Expected statements");
  return new StatementsAst(list[list.length - 1].type, location, list);
};

export type Vm = {
  ip: number,
  stack: unknown[],
  scope: Scope,
  location: SourceLocation,
  bytecode: BytecodeGen,
  context: TaskContext
}
export type SubCompilerState = {
  vm: Vm,
  func: FunctionDefinition,
  quoteStack: Ast[][]
  prevCompilerState: SubCompilerState | undefined
}

export type GlobalCompilerState = {
  compiledFunctions: Map<Binding, CompiledFunction>;
  functionDefinitions: FunctionDefinition[],

  subCompilerState: SubCompilerState | undefined
}

export interface TaskContext {
  subCompilerState: SubCompilerState
  globalCompiler: GlobalCompilerState
}

export const pushSubCompilerState = (ctx: TaskContext, obj: { vm: Vm, func: FunctionDefinition }) => {
  const state: SubCompilerState = {
    func: obj.func,
    vm: obj.vm,
    quoteStack: [],
    prevCompilerState: ctx.subCompilerState
  }
  ctx.subCompilerState = state;
  // compilerState = state;
  // globalCompiler.subCompilerState = state;
}

export const addFunctionDefinition = (compilerState: GlobalCompilerState, decl: ParserFunctionDecl) => {
  if (decl.id !== undefined) return compilerState.functionDefinitions[decl.id];

  decl.id = compilerState.functionDefinitions.length;
  const funcDef = new FunctionDefinition(
    decl.id, decl.debugName,
    decl.name, decl.typeArgs, decl.args,
    decl.returnType, decl.body)

  compilerState.functionDefinitions.push(funcDef);
  return funcDef;
}

export function bytecodeToString(bytecodeGen: BytecodeGen) {
  const { locations, code } = bytecodeGen
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
