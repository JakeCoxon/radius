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

export type ArgumentTypePair = [ParseIdentifier, ParseAst | null];

// These are reference types that id will be filled in later.
export type ParserFunctionDecl = {
  id: number | undefined, debugName: string,
  token: Token, functionMetaName: ParseIdentifier | null,
  name: ParseIdentifier | null, typeArgs: ParseAst[], args: ArgumentTypePair[], 
  returnType: ParseAst | null, body: ParseAst | null, keywords: ParseAst[] }
  
export type ParserClassDecl = {
  id: number | undefined, debugName: string,
  token: Token, metaType: ParseIdentifier | null,
  name: ParseIdentifier | null, typeArgs: ParseAst[], 
  body: ParseAst | null, keywords: ParseAst[] }

class ParseTreeType {
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

export class ParseName extends ParseTreeType {       key = 'name' as const;       constructor(public token: Token) { super();} }
export class ParseIdentifier extends ParseTreeType { key = 'identifier' as const; constructor(public token: Token) { super();} }
export class ParseSymbol extends ParseTreeType {     key = 'symbol' as const;     constructor(public token: Token) { super();} }
export class ParseNil extends ParseTreeType {        key = 'nil' as const;        constructor(public token: Token) { super();} }
export class ParseNumber extends ParseTreeType {     key = 'number' as const;     constructor(public token: Token) { super();} }
export class ParseString extends ParseTreeType {     key = 'string' as const;     constructor(public token: Token) { super();} }
export class ParseBoolean extends ParseTreeType {    key = 'boolean' as const;    constructor(public token: Token) { super();} }

export class ParseStatements extends ParseTreeType { key = 'statements' as const; constructor(public token: Token, public exprs: ParseAst[]) { super();} }
export class ParseLet extends ParseTreeType {        key = 'let' as const;        constructor(public token: Token, public name: ParseIdentifier, public type: ParseAst | null, public value: ParseAst | null) { super();} }
export class ParseSet extends ParseTreeType {        key = 'set' as const;        constructor(public token: Token, public name: ParseAst, public value: ParseAst) { super();} }
export class ParseOperator extends ParseTreeType {   key = 'operator' as const;   constructor(public token: Token, public exprs: ParseAst[]) { super();} }
export class ParseNote extends ParseTreeType {       key = 'note' as const;       constructor(public token: Token, public expr: ParseAst) { super();} }
export class ParseMeta extends ParseTreeType {       key = 'meta' as const;       constructor(public token: Token, public expr: ParseAst) { super();} }
export class ParseMetaIf extends ParseTreeType {     key = 'metaif' as const;     constructor(public token: Token, public expr: ParseIf) { super();} }
export class ParseMetaFor extends ParseTreeType {    key = 'metafor' as const;    constructor(public token: Token, public expr: ParseFor) { super();} }
export class ParseCompTime extends ParseTreeType {   key = 'comptime' as const;   constructor(public token: Token, public expr: ParseAst) { super();} }
export class ParseCall extends ParseTreeType {       key = 'call' as const;       constructor(public token: Token, public left: ParseAst, public args: ParseAst[], public typeArgs: ParseAst[]) { super();} }
export class ParseList extends ParseTreeType {       key = 'list' as const;       constructor(public token: Token, public exprs: ParseAst[]) { super();} }
export class ParseListComp extends ParseTreeType {   key = 'listcomp' as const;   constructor(public token: Token, public exprs: ParseAst[], public mapping: ParseAst[], public reduce: ParseAst | null) { super();} }
export class ParseOr extends ParseTreeType {         key = 'or' as const;         constructor(public token: Token, public exprs: ParseAst[]) { super();} }
export class ParseAnd extends ParseTreeType {        key = 'and' as const;        constructor(public token: Token, public exprs: ParseAst[]) { super();} }
export class ParseElse extends ParseTreeType {       key = 'else' as const;       constructor(public token: Token, public body: ParseAst) { super();} }
export class ParseIf extends ParseTreeType {         key = 'if' as const;         constructor(public token: Token, public condition: ParseAst, public trueBody: ParseAst, public falseBody: ParseIf | ParseElse | null) { super();} }
export class ParseLetConst extends ParseTreeType {   key = 'letconst' as const;   constructor(public token: Token, public name: ParseIdentifier, public value: ParseAst) { super();} }
export class ParseFunction extends ParseTreeType {   key = 'function' as const;   constructor(public token: Token, public functionDecl: ParserFunctionDecl) { super();} }
export class ParseClass extends ParseTreeType {      key = 'class' as const;      constructor(public token: Token, public classDecl: ParserClassDecl) { super();} }
export class ParseReturn extends ParseTreeType {     key = 'return' as const;     constructor(public token: Token, public expr: ParseAst | null) { super();} }
export class ParseBreak extends ParseTreeType {      key = 'break' as const;      constructor(public token: Token, public expr: ParseAst) { super();} }
export class ParseContinue extends ParseTreeType {   key = 'continue' as const;   constructor(public token: Token, public expr: ParseAst) { super();} }
export class ParseFor extends ParseTreeType {        key = 'for' as const;        constructor(public token: Token, public identifier: ParseIdentifier, public expr: ParseAst, public body: ParseAst) { super();} }
export class ParseCast extends ParseTreeType {       key = 'cast' as const;       constructor(public token: Token, public expr: ParseAst, public as: ParseAst) { super();} }
export class ParseOpEq extends ParseTreeType {       key = 'opeq' as const;       constructor(public token: Token, public left: ParseAst, public right: ParseAst) { super();} }
export class ParseWhile extends ParseTreeType {      key = 'while' as const;      constructor(public token: Token, public condition: ParseAst, public body: ParseAst) { super();} }
export class ParseWhileExpr extends ParseTreeType {  key = 'whileexpr' as const;  constructor(public token: Token, public condition: ParseAst, public body: ParseAst) { super();} }
export class ParseForExpr extends ParseTreeType {    key = 'forexpr' as const;    constructor(public token: Token, public identifier: ParseIdentifier, public expr: ParseAst, public body: ParseAst) { super();} }
export class ParseNot extends ParseTreeType {        key = 'not' as const;        constructor(public token: Token, public expr: ParseAst) { super();} }
export class ParseField extends ParseTreeType {      key = 'field' as const;      constructor(public token: Token, public expr: ParseAst, public field: ParseIdentifier) { super();} }
export class ParseExpand extends ParseTreeType {     key = 'expand' as const;     constructor(public token: Token, public expr: ParseAst) { super();} }
export class ParseDict extends ParseTreeType {       key = 'dict' as const;       constructor(public token: Token, public pairs: [ParseAst, ParseAst][]) { super();} }
export class ParsePostCall extends ParseTreeType {   key = 'postcall' as const;   constructor(public token: Token, public expr: ParseAst, public arg: ParseAst) { super();} }
export class ParseSlice extends ParseTreeType {      key = 'slice' as const;      constructor(public token: Token, public expr: ParseAst, public a: ParseAst | null, public b: ParseAst | null, public c: ParseAst | null, public isStatic: boolean) { super();} }
export class ParseSubscript extends ParseTreeType {  key = 'subscript' as const;  constructor(public token: Token, public expr: ParseAst, public subscript: ParseAst, public isStatic: boolean) { super();} }
export class ParseTuple extends ParseTreeType {      key = 'tuple' as const;      constructor(public token: Token, public exprs: ParseAst[]) { super();} }

export type ParseAst = ParseStatements | ParseLet | ParseSet | ParseOperator | ParseName | ParseIdentifier | 
  ParseNumber | ParseMeta | ParseCompTime | ParseLetConst | ParseCall | ParseList | ParseOr | ParseAnd | 
  ParseIf | ParseFunction | ParseString | ParseReturn | ParseBreak | ParseContinue | ParseFor | ParseCast |
  ParseOpEq | ParseWhile | ParseWhileExpr | ParseForExpr | ParseNot | ParseField | ParseExpand | ParseListComp |
  ParseDict | ParsePostCall | ParseSymbol | ParseNote | ParseSlice | ParseSubscript | ParseTuple | ParseClass |
  ParseNil | ParseBoolean | ParseElse | ParseMetaIf | ParseMetaFor

// Void types mean that in secondOrder compilation, the AST doesn't return an AST
export const isParseVoid = (ast: ParseAst) => ast.key == 'letconst' || ast.key === 'function' || ast.key === 'class' || ast.key === 'comptime';

export type BytecodeInstr = 
  { type: 'binding', name: string } |
  { type: 'push', value: unknown } |
  { type: 'operator', name: string, count: number } |
  { type: 'name', name: string } |
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
  { type: 'nameast', name: string } |
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
  body: ParseAst
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
    code: any[]
    locations: SourceLocation[]
  }
  table: MetaInstructionTable
  globalCompilerState: GlobalCompilerState // Not nice
}

export type MetaInstructionTable = {
  [E in ParseAst as E['key']]: (out: BytecodeOut, ast: E) => void;
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
    public typeArgs: ParseAst[],
    public args: ArgumentTypePair[],
    public returnType: ParseAst | null,
    public body: ParseAst | null) {}

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
  quoteStack: ParseAst[][]
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
// export const popSubCompilerState = () => {
//   globalCompiler.subCompilerState = globalCompiler.subCompilerState?.prevCompilerState;
//   compilerState = globalCompiler.subCompilerState!;
// }
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
