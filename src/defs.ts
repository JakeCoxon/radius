import { Event } from "./tasks";

export const Inspect = globalThis.Bun ? Bun.inspect : (() => {
  const Inspect = (x) => x
  Inspect.custom = Symbol('input')
  return Inspect
})()

export class CompilerError extends Error {
  constructor(message: string, public info: object) { super(message) }
}
export const createCompilerError = (message: string, info: object) => {
  let out = message.replace(/\$([a-zA-Z]+)/g, (match, capture) => { 
    const obj = info[capture]
    if (obj === undefined || obj === null) return makeColor(`null`)
    if (obj[Inspect.custom]) return Inspect(obj, { depth: 0, colors: true });
    if (typeof obj !== 'object') return Inspect(obj, { depth: 0, colors: true });
    if (obj.constructor) return makeColor(`[${obj.constructor.name}]`)
    return Inspect(obj)
  }); 
  return new CompilerError(out, info);
}
export function compilerAssert(expected: unknown, message: string="", info: object={}): asserts expected {
  if (expected) return;
  throw createCompilerError(message, info)
}

const makeColor = (x) => {
  return Inspect({
    [Inspect.custom](depth, options, inspect) {
      return options.stylize(x, 'special');
    }
  }, { colors: true })
}

export class Source {
  tokens: Token[]
  constructor(public debugName: string, public input: string) {}
}

export class SourceLocation {
  static anon = new SourceLocation(-1, -1, null!)
  constructor(public line: number, public column: number, public source: Source) {}
  [Inspect.custom](depth, options, inspect) {
    if (!this.source) return options.stylize(`[SourceLocation generated]`, 'special');
    return options.stylize(`[SourceLocation ${this.source.debugName}:${this.line}:${this.column}]`, 'special');
  }
}

export type Token = { value: string, type: string, location: SourceLocation }

const TokenRoot = {
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[Token ${this.value}]`, 'string');
  }
}
export const createToken = (source: Source, value: any, type = "NONE"): Token => Object.assign(Object.create(TokenRoot), { value, type, location: new SourceLocation(0, 0, source) });
export const createAnonymousToken = (value: any, type = "NONE"): Token => Object.assign(Object.create(TokenRoot), { value, type, location: new SourceLocation(-1, -1, null!) });

export type ArgumentTypePair = [ParseIdentifier, ParseNode | null];

// These are reference types that id will be filled in later.
export type ParserFunctionDecl = {
  id: number | undefined, debugName: string, anonymous?: boolean,
  token: Token, functionMetaName: ParseIdentifier | null,
  name: ParseIdentifier | null, typeArgs: ParseNode[], args: ArgumentTypePair[], 
  returnType: ParseNode | null, body: ParseNode | null, keywords: ParseNode[] }

export const createAnonymousParserFunctionDecl = (debugName: string, sourceToken: Token, args: ArgumentTypePair[], body: ParseNode) => {
  const decl: ParserFunctionDecl = {
    debugName: debugName,
    id: undefined,
    args: args,
    name: null,
    body: body,
    returnType: null,
    anonymous: true,
    token: sourceToken,
    keywords: [],
    typeArgs: [],
    functionMetaName: null
  }
  return decl;
}
  
export type ParserClassDecl = {
  id: number | undefined, debugName: string,
  token: Token, metaType: ParseIdentifier | null,
  name: ParseIdentifier | null, typeArgs: ParseNode[], 
  body: ParseNode | null, keywords: ParseNode[] }

class ParseNodeType {
  key: unknown;
  [Inspect.custom](depth, options, inspect) {
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
export class ParseSet extends ParseNodeType {        key = 'set' as const;        constructor(public token: Token, public left: ParseNode, public value: ParseNode) { super();} }
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
export class ParseBreak extends ParseNodeType {      key = 'break' as const;      constructor(public token: Token, public expr: ParseNode | null) { super();} }
export class ParseContinue extends ParseNodeType {   key = 'continue' as const;   constructor(public token: Token, public expr: ParseNode | null) { super();} }
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
export class ParseBlock extends ParseNodeType {      key = 'block' as const;      constructor(public token: Token, public statements: ParseStatements) { super();} }
export class ParseImport extends ParseNodeType {     key = 'import' as const;     constructor(public token: Token, public module: ParseIdentifier, public identifiers: ParseIdentifier[]) { super();} }
export class ParseValue extends ParseNodeType {      key = 'value' as const;      constructor(public token: Token, public value: unknown) { super();} }
export class ParseConstructor extends ParseNodeType { key = 'constructor' as const; constructor(public token: Token, public type: ParseNode, public args: ParseNode[]) { super();} }
export class ParseCompilerIden extends ParseNodeType { key = 'compileriden' as const; constructor(public token: Token, public value: string) { super();} }

export type ParseNode = ParseStatements | ParseLet | ParseSet | ParseOperator | ParseIdentifier | 
  ParseNumber | ParseMeta | ParseCompTime | ParseLetConst | ParseCall | ParseList | ParseOr | ParseAnd | 
  ParseIf | ParseFunction | ParseString | ParseReturn | ParseBreak | ParseContinue | ParseFor | ParseCast |
  ParseOpEq | ParseWhile | ParseWhileExpr | ParseForExpr | ParseNot | ParseField | ParseExpand | ParseListComp |
  ParseDict | ParsePostCall | ParseSymbol | ParseNote | ParseSlice | ParseSubscript | ParseTuple | ParseClass |
  ParseNil | ParseBoolean | ParseElse | ParseMetaIf | ParseMetaFor | ParseBlock | ParseImport | ParseCompilerIden | 
  ParseValue | ParseConstructor

// Void types mean that in secondOrder compilation, the AST doesn't return an AST
export const isParseVoid = (ast: ParseNode) => ast.key == 'letconst' || ast.key === 'function' || ast.key === 'class' || ast.key === 'comptime';
export const isParseNode = (ast: unknown): ast is ParseNode => ast instanceof ParseNodeType

export type BytecodeInstr = 
  { type: 'comment', comment: string } |
  { type: 'binding', name: string } |
  { type: 'push', value: unknown } |
  { type: 'operator', name: string, count: number } |
  { type: 'letlocal', name: string, t: boolean, v: boolean } |
  { type: 'setlocal', name: string } |
  { type: 'list', count: number } |
  { type: 'tuple', count: number } |
  { type: 'closure', id: number } |
  { type: 'call', name: string, count: number, tcount: number } |
  { type: 'compilerfn', name: string, count: number, tcount: number } |
  { type: 'return', r: boolean } |
  { type: 'not' } |
  { type: 'pop' } |
  { type: 'nil' } |
  { type: 'beginblockast', breakType: 'break' | 'continue' | null } |
  { type: 'endblockast'  } |
  { type: 'bindingast', name: string } |
  { type: 'totype' } |
  { type: 'numberast', value: number } |
  { type: 'stringast', value: string } |
  { type: 'boolast', value: boolean } |
  { type: 'setlocalast', name: string } |
  { type: 'setfieldast', name: string } |
  { type: 'fieldast', name: string } |
  { type: 'subscriptast' } |
  { type: 'operatorast', name: string, count: number } |
  { type: 'constructorast', count: number } |
  { type: 'toast' } |
  { type: 'whileast' } |
  { type: 'returnast', r: boolean } |
  { type: 'breakast', v: boolean } |
  { type: 'continueast', v: boolean } |
  { type: 'listast', count: number } |
  { type: 'andast', count: number } |
  { type: 'orast', count: number } |
  { type: 'ifast', f: boolean } |
  { type: 'notast' } |
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

export type BytecodeProgram = {
  code: BytecodeInstr[]
  locations: SourceLocation[]
}
export interface BytecodeWriter {
  bytecode: {
    code: BytecodeInstr[]
    locations: SourceLocation[],
  },
  state: {
    labelBlock: LabelBlock | null
  }
  instructionTable: ParseTreeTable
  globalCompilerState: GlobalCompilerState // Not nice
}
export type FunctionPrototype = {
  name: string
  bytecode?: BytecodeProgram | undefined
  body: ParseNode
  initialInstructionTable: ParseTreeTable
}

export type ParseTreeTable = {
  [E in ParseNode as E['key']]: (out: BytecodeWriter, ast: E) => void;
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
    public body: ParseNode | null,
    public inline: boolean) {}

  [Inspect.custom](depth, options, inspect) {
    if (options.ast) return options.stylize(`[FunctionDefinition ${this.name}]`, 'special');
    return {...this}
  }
}

export class TypeField {
  constructor(
    public location: SourceLocation,
    public name: string,
    public sourceType: Type,
    public index: number,
    public fieldType: Type,
    ) {}
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[TypeField ${this.index} ${inspect(this.sourceType)} ${this.name}]`, 'special');
  }
}
export class CompiledClass {
  metaobject: {} = Object.create(null)

  constructor(
    public location: SourceLocation,
    public debugName: string,
    public binding: Binding,
    public classDefinition: ClassDefinition,
    public type: Type,
    public body: Ast,
    public fields: TypeField[],
    public typeParameters: unknown[],
    public typeParamHash: unknown) {}

  [Inspect.custom](depth, options, inspect) {
    if (options.ast) return options.stylize(`[CompiledClass ${this.debugName}]`, 'special');
    return {...this}
  }
}

export class ClassDefinition {
  headerPrototype?: FunctionPrototype | undefined
  templatePrototype?: FunctionPrototype | undefined

  compiledClasses: CompiledClass[] = []
  concreteType: ConcreteClassType | undefined

  abstract: boolean = false

  constructor(
    public id: number,
    public location: SourceLocation,
    public parentScope: Scope,
    public debugName: string,
    public name: ParseIdentifier | null,
    public typeArgs: ParseNode[],
    public body: ParseNode | null) {
      this.abstract = typeArgs.length > 0
    }

  [Inspect.custom](depth, options, inspect) {
    if (options.ast) return options.stylize(`[ClassDefinition ${this.name}]`, 'special');
    return {...this}
  }
}

export class AstRoot {
  [Inspect.custom](depth, options, inspect) {
    if (depth <= 1) return options.stylize(`[${this.constructor.name}]`, 'special');
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
export class BreakAst extends AstRoot {      constructor(public type: Type, public location: SourceLocation, public binding: Binding, public expr: Ast | null) { super() } }
export class BlockAst extends AstRoot {      constructor(public type: Type, public location: SourceLocation, public binding: Binding, public body: Ast) { super() } }
export class FieldAst extends AstRoot {      constructor(public type: Type, public location: SourceLocation, public left: Ast, public field: TypeField) { super() } }
export class SetFieldAst extends AstRoot {   constructor(public type: Type, public location: SourceLocation, public left: Ast, public field: TypeField, public value: Ast) { super() } }
export class VoidAst extends AstRoot {       constructor(public type: Type, public location: SourceLocation) { super() } }
export class CastAst extends AstRoot {       constructor(public type: Type, public location: SourceLocation, public expr: Ast) { super() } }
export class SubscriptAst extends AstRoot {  constructor(public type: Type, public location: SourceLocation, public left: Ast, public right: Ast) { super() } }
export class NotAst extends AstRoot {        constructor(public type: Type, public location: SourceLocation, public expr: Ast) { super() } }
export class ConstructorAst extends AstRoot { constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }

export type Ast = NumberAst | LetAst | SetAst | OperatorAst | IfAst | ListAst | CallAst | AndAst |
  OrAst | StatementsAst | WhileAst | ReturnAst | SetFieldAst | VoidAst | CastAst | SubscriptAst | ConstructorAst
export const isAst = (value: unknown): value is Ast => value instanceof AstRoot;

export class Tuple {
  constructor(public values: unknown[]) {}
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[Tuple ...]`, 'special');
  }
}

export class Binding {
  constructor(public name: string, public type: Type) {}
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[Binding ${this.name} ${inspect(this.type)}]`, 'special');
  }
}

let uniqueId = 1000
// const uniqueMap = new WeakMap<object, number>()
const UNIQUE_ID = Symbol('UNIQUE_ID')
const getUniqueId = (obj: object) => {
  if (obj[UNIQUE_ID] !== undefined) return obj[UNIQUE_ID]
  obj[UNIQUE_ID] = uniqueId++
  return obj[UNIQUE_ID];
}
export const hashValues = (values: unknown[]) => {
  return values.map(value => {
    if (value instanceof PrimitiveType) return `$${value.typeName}`
    if (typeof value === 'number') return value
    if (value instanceof FunctionDefinition) return getUniqueId(value)
    if (value instanceof Closure) return getUniqueId(value)
    compilerAssert(false, "Cannot hash value", { value })
  }).join("__")
}

export class TypeRoot {}
export class PrimitiveType extends TypeRoot {
  metaobject: {} = Object.create(null)
  constructor(public typeName: string) { super() }
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[PrimitiveType ${this.typeName}]`, 'special');
  }
}
export class ExternalType extends TypeRoot {
  metaobject: {} = Object.create(null)
  constructor(public typeName: string) { super() }
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[ExternalType ${this.typeName}]`, 'special');
  }
}
export class ConcreteClassType extends TypeRoot {
  metaobject: {}
  constructor(public compiledClass: CompiledClass) { 
    super()
    this.metaobject = compiledClass.metaobject
  }
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[ConcreteClassType ${this.compiledClass.debugName}]`, 'special');
  }
}
export class ParameterizedType extends TypeRoot {
  metaobject: {}
  constructor(public typeConstructor: Type, public args: Type[]) {
    super()
    this.metaobject = typeConstructor.metaobject
  }
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[ParameterizedType ...]`, 'special');
  }
}
export type Type = PrimitiveType | ExternalType | ConcreteClassType | ParameterizedType
export const isType = (type: unknown): type is Type => type instanceof TypeRoot

export class Closure {
  constructor(public func: FunctionDefinition, public scope: Scope) {}
}

export const ScopeEventsSymbol = Symbol('ScopeEventsSymbol')
export type Scope = object & {
  _scope: true,
  [ScopeEventsSymbol]: {[key:string]:Event<unknown, CompilerError>}
}
const ScopePrototype = {
  [Inspect.custom](depth, options, inspect) {
    if (depth <= 1) return options.stylize(`[Scope]`, 'special');
    return inspect({...this})
  }
}
export const createScope = (obj: object, parentScope: Scope | undefined) => Object.assign(Object.create(parentScope || ScopePrototype), obj) as Scope;

export class ExternalFunction {
  constructor(public name: string, public returnType: Type, public func: Function) {}
  [Inspect.custom](depth, options, inspect) {
    return options.stylize(`[ExternalFunction ${this.name}]`, 'special');
  }
}

export const VoidType = new PrimitiveType("void")
export const IntType = new PrimitiveType("int")
export const BoolType = new PrimitiveType("bool")
export const FloatType = new PrimitiveType("float")
export const DoubleType = new PrimitiveType("double")
export const StringType = new PrimitiveType("string")
export const FunctionType = new PrimitiveType("function")
export const ListType = Object.assign(new ExternalType("List"), {
  lengthField: null! as TypeField
});
ListType.lengthField = new TypeField(new SourceLocation(-1, -1, null!), "length", ListType, 0, IntType)

export const BuiltinTypes = {
  int: IntType,
  float: FloatType,
  double: DoubleType,
  void: VoidType,
  string: StringType,
  bool: BoolType,
  List: ListType
}

class TypeTable {
  array: Type[] = [] // TODO: Use a map here?
  constructor() { }

  get(type: Type) { 
    for (const t of this.array) {
      if (typesEqual(t, type)) return t;
    }
  }
  insert(type: Type) { 
    this.array.push(type);
    return type;
  }
  getOrInsert(type: Type): Type {
    let v = this.get(type)
    if (v) return v;
    return this.insert(type)
  }
  
}

// Don't use directly, use type table to see if types are equal
const typesEqual = (t1: Type, t2: any) => {
  compilerAssert(t1 && t2, "Unexpected", { t1, t2 })
  if (Object.getPrototypeOf(t1) !== Object.getPrototypeOf(t2)) return false;
  if (t1 instanceof PrimitiveType) return t1 == t2;
  if (t1 instanceof ExternalType) return t1 == t2;
  if (t1 instanceof ConcreteClassType) return t1.compiledClass == t2.compiledClass;
  if (t1 instanceof ParameterizedType) {
    if (!typesEqual(t1.typeConstructor, (t2 as any).typeConstructor)) return false;
    if (t1.args.length !== t2.args.length) return false;
    return t1.args.every((x, i) => typesEqual(x, t2.args[i]))
  }
}
export const isParameterizedTypeOf = (a: Type, expected: Type) => {
  return a instanceof ParameterizedType && a.typeConstructor === expected;
}


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
  compilerAssert(isType(expected), "Expected Type got $expected", { expected, ...info }); 
  return expected;
};
export const expectAsts = <T>(expected: unknown[], info: object = {}) => {
  compilerAssert(expected.every(isAst), "Expected ASTs got $expected", { expected, ...info }); 
  return expected;
};
export const createStatements = (location: SourceLocation, list: Ast[]) => {
  if (list.length === 0) return new VoidAst(VoidType, location);
  if (list.length === 1) return list[0];
  return new StatementsAst(list[list.length - 1].type, location, list);
};

export type Vm = {
  ip: number,
  stack: unknown[],
  scope: Scope,
  location: SourceLocation,
  bytecode: BytecodeProgram,
  context: TaskContext
}
export type LabelBlockType = 'break' | 'continue' | null

// Isn't it weird that these are similar but not the same?
export class LabelBlock {
  public completion: ((value: unknown) => void)[] = []
  constructor(
    public parent: LabelBlock | null,
    public name: string | null,
    public type: LabelBlockType,
    public binding: Binding | null) {}
}

export const findLabelBlockByType = (labelBlock: LabelBlock | null, type: LabelBlockType) => {
  let block = labelBlock
  while (block) {
    if (block.type === type) return block
    block = block.parent;
  }
  compilerAssert(false, `Invalid ${type} outside a block`, { labelBlock })
}

export type Logger = { log: (...args: any[]) => void }

export type GlobalCompilerState = {
  compiledFunctions: Map<Binding, CompiledFunction>;
  functionDefinitions: FunctionDefinition[],
  classDefinitions: ClassDefinition[],
  moduleLoader: ModuleLoader

  allWaitingEvents: Event<unknown, unknown>[],
  logger: Logger,
  typeTable: TypeTable
}

export interface ParsedModule {
  classDefs: ParserClassDecl[]
  functionDecls: ParserFunctionDecl[]
  rootNode: ParseStatements
}

export interface ModuleLoader {
  loadModule: (module: string) => ParsedModule
}

export interface TaskContext {
  subCompilerState: SubCompilerState
  globalCompiler: GlobalCompilerState
}

export const createDefaultGlobalCompiler = () => {
  const globalCompiler: GlobalCompilerState = {
    compiledFunctions: new Map(),
    functionDefinitions: [],
    classDefinitions: [],
    allWaitingEvents: [],
    moduleLoader: null!,
    typeTable: new TypeTable(),
    logger: null!
  }
  return globalCompiler
}


export class SubCompilerState {
  constructor(
    public debugName: string,
    ) {}

  vm: Vm
  func: FunctionDefinition
  quoteStack: Ast[][] = []
  scope: Scope
  // Enclosing function is used for binding lookup, but not by default because 
  // not all bindings are accessible across function boundaries
  functionCompiler: SubCompilerState | undefined
  lexicalParent: SubCompilerState | undefined
  prevCompilerState: SubCompilerState | undefined
  labelBlock: LabelBlock | null

  [Inspect.custom](depth, options, inspect) {
    if (depth <= 1) return options.stylize(`[CompilerState ${this.debugName}]`, 'special');
    const newOptions = Object.assign({}, options, {
      depth: options.depth === null ? null : options.depth - 1,
    });
    return inspect({ast: this.constructor.name, ...this}, newOptions)
  }
}

export const pushSubCompilerState = (ctx: TaskContext, obj: { debugName: string, vm?: Vm, func?: FunctionDefinition, scope: Scope, lexicalParent: SubCompilerState | undefined }) => {
  const state = new SubCompilerState(obj.debugName)
  state.prevCompilerState = ctx.subCompilerState;
  state.lexicalParent = obj.lexicalParent
  state.scope = obj.scope
  state.functionCompiler = state.lexicalParent?.functionCompiler
  ctx.subCompilerState = state;
  return state;
}

export function bytecodeToString(bytecodeProgram: BytecodeProgram) {
  const { locations, code } = bytecodeProgram
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

// https://gist.github.com/JBlond/2fea43a3049b38287e5e9cefc87b2124
export const textColors = {
  red: (string: string) => `\x1b[31m${string}\x1b[39m`,
  yellow: (string: string) => `\x1b[33m${string}\x1b[39m`,
  green: (string: string) => `\x1b[32m${string}\x1b[39m`,
  cyan: (string: string) => `\x1b[36m${string}\x1b[39m`,
  gray: (string: string) => `\x1b[38;5;242m${string}\x1b[39m`,
}

export const outputSourceLocation = (location: SourceLocation) => {
  if (!location?.source) return '<generated code>'
  let out = `${location.source.debugName}:${location.line}:${location.column}`;
  out += '\n'
  const lines = location.source.input.split('\n')
  for (let i = -2; i < 3; i++) {
    const line = location.line + i
    if (lines[line - 1] !== undefined) {
      out += textColors.gray(`${String(line).padStart(2)}|`)
      out += `  ${lines[line - 1]}\n`
      if (i === 0) {
        const repeat = " ".repeat(location.column);
        out += textColors.red(`     ${repeat}^-- here\n`)
      }
    }
  }
  return out;
}