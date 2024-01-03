import { Event } from "./tasks";

export type UnknownObject = {[key:string]:unknown}

export const Inspect = globalThis.Bun ? Bun.inspect : (() => {
  const Inspect = (x: unknown) => String(x)
  Inspect.custom = Symbol('input')
  return Inspect
})()

export class CompilerError extends Error {
  constructor(message: string, public info: object) { super(message) }
}
export const createCompilerError = (message: string, info: object) => {
  const userinfo: string[] = []
  let out = message.replace(/\$([a-zA-Z]+)/g, (match, capture) => { 
    const obj = (info as any)[capture]
    userinfo.push(capture)
    if (obj === undefined || obj === null) return makeColor(`null`)
    if (obj[Inspect.custom]) return Inspect(obj, { depth: 0, colors: true });
    if (typeof obj !== 'object') return Inspect(obj, { depth: 0, colors: true });
    if (obj.constructor) return makeColor(`[${obj.constructor.name}]`)
    return Inspect(obj)
  }); 
  (info as any)._userinfo = userinfo
  return new CompilerError(out, info);
}
export function compilerAssert(expected: unknown, message: string="", info: object={}): asserts expected {
  if (expected) return;
  throw createCompilerError(message, info)
}

const makeColor = (x) => {
  return Inspect({
    [Inspect.custom](depth: any, options: any, inspect: any) {
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
  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (!this.source) return options.stylize(`[SourceLocation generated]`, 'special');
    return options.stylize(`[SourceLocation ${this.source.debugName}:${this.line}:${this.column}]`, 'special');
  }
}

export type Token = { value: string, type: string, location: SourceLocation }

export const TokenRoot = {
  [Inspect.custom](depth: any, options: any, inspect: any) {
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
  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1) return options.stylize(`[${this.constructor.name}]`, 'special');
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
export class ParseLet extends ParseNodeType {        key = 'let' as const;        constructor(public token: Token, public name: ParseIdentifier | ParseFreshIden, public type: ParseNode | null, public value: ParseNode | null) { super();} }
export class ParseSet extends ParseNodeType {        key = 'set' as const;        constructor(public token: Token, public left: ParseNode, public value: ParseNode) { super();} }
export class ParseOperator extends ParseNodeType {   key = 'operator' as const;   constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseNote extends ParseNodeType {       key = 'note' as const;       constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseMeta extends ParseNodeType {       key = 'meta' as const;       constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseMetaIf extends ParseNodeType {     key = 'metaif' as const;     constructor(public token: Token, public expr: ParseIf) { super();} }
export class ParseMetaFor extends ParseNodeType {    key = 'metafor' as const;    constructor(public token: Token, public expr: ParseFor) { super();} }
export class ParseMetaWhile extends ParseNodeType {  key = 'metawhile' as const;  constructor(public token: Token, public expr: ParseWhile) { super();} }
export class ParseCompTime extends ParseNodeType {   key = 'comptime' as const;   constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseCall extends ParseNodeType {       key = 'call' as const;       constructor(public token: Token, public left: ParseNode, public args: ParseNode[], public typeArgs: ParseNode[]) { super();} }
export class ParseList extends ParseNodeType {       key = 'list' as const;       constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseListComp extends ParseNodeType {   key = 'listcomp' as const;   constructor(public token: Token, public exprs: ParseNode[], public mapping: ParseNode[], public reduce: ParseNode | null) { super();} }
export class ParseOr extends ParseNodeType {         key = 'or' as const;         constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseAnd extends ParseNodeType {        key = 'and' as const;        constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseElse extends ParseNodeType {       key = 'else' as const;       constructor(public token: Token, public body: ParseNode) { super();} }
export class ParseIf extends ParseNodeType {         key = 'if' as const;         constructor(public token: Token, public isExpr: boolean, public condition: ParseNode, public trueBody: ParseNode, public falseBody: ParseIf | ParseElse | null) { super();} }
export class ParseLetConst extends ParseNodeType {   key = 'letconst' as const;   constructor(public token: Token, public name: ParseIdentifier, public value: ParseNode) { super();} }
export class ParseFunction extends ParseNodeType {   key = 'function' as const;   constructor(public token: Token, public functionDecl: ParserFunctionDecl) { super();} }
export class ParseClass extends ParseNodeType {      key = 'class' as const;      constructor(public token: Token, public classDecl: ParserClassDecl) { super();} }
export class ParseReturn extends ParseNodeType {     key = 'return' as const;     constructor(public token: Token, public expr: ParseNode | null) { super();} }
export class ParseBreak extends ParseNodeType {      key = 'break' as const;      constructor(public token: Token, public name: ParseIdentifier | null, public expr: ParseNode | null) { super();} }
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
export class ParseSlice extends ParseNodeType {      key = 'slice' as const;      constructor(public token: Token, public expr: ParseNode, public start: ParseNode | null, public end: ParseNode | null, public step: ParseNode | null, public isStatic: boolean) { super();} }
export class ParseSubscript extends ParseNodeType {  key = 'subscript' as const;  constructor(public token: Token, public expr: ParseNode, public subscript: ParseNode, public isStatic: boolean) { super();} }
export class ParseTuple extends ParseNodeType {      key = 'tuple' as const;      constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseBlock extends ParseNodeType {      key = 'block' as const;      constructor(public token: Token, public breakType: BreakType | null, public name: ParseIdentifier | null, public statements: ParseStatements) { super();} }
export class ParseImportName extends ParseNodeType { key = 'importname' as const; constructor(public token: Token, public identifier: ParseIdentifier, public rename: ParseIdentifier | null) { super();} }
export class ParseImport extends ParseNodeType {     key = 'import' as const;     constructor(public token: Token, public module: ParseIdentifier, public rename: ParseIdentifier | null, public imports: ParseImportName[]) { super();} }
export class ParseValue extends ParseNodeType {      key = 'value' as const;      constructor(public token: Token, public value: unknown) { super();} }
export class ParseQuote extends ParseNodeType {      key = 'quote' as const;      constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseBytecode extends ParseNodeType {   key = 'bytecode' as const;   constructor(public token: Token, public bytecode: { code: BytecodeInstr[]; locations: SourceLocation[]; }) { super();} }
export class ParseFreshIden extends ParseNodeType {  key = 'freshiden' as const; constructor(public token: Token, public freshBindingToken: FreshBindingToken) { super();} }
export class ParseConstructor extends ParseNodeType { key = 'constructor' as const; constructor(public token: Token, public type: ParseNode, public args: ParseNode[]) { super();} }
export class ParseCompilerIden extends ParseNodeType { key = 'compileriden' as const; constructor(public token: Token, public value: string) { super();} }

export type ParseNode = ParseStatements | ParseLet | ParseSet | ParseOperator | ParseIdentifier | 
  ParseNumber | ParseMeta | ParseCompTime | ParseLetConst | ParseCall | ParseList | ParseOr | ParseAnd | 
  ParseIf | ParseFunction | ParseString | ParseReturn | ParseBreak | ParseContinue | ParseFor | ParseCast |
  ParseOpEq | ParseWhile | ParseWhileExpr | ParseForExpr | ParseNot | ParseField | ParseExpand | ParseListComp |
  ParseDict | ParsePostCall | ParseSymbol | ParseNote | ParseSlice | ParseSubscript | ParseTuple | ParseClass |
  ParseNil | ParseBoolean | ParseElse | ParseMetaIf | ParseMetaFor | ParseMetaWhile | ParseBlock | ParseImport | 
  ParseCompilerIden | ParseValue | ParseConstructor | ParseQuote | ParseBytecode | ParseFreshIden

// Void types mean that in secondOrder compilation, the AST doesn't return an AST
export const isParseVoid = (ast: ParseNode) => ast.key == 'letconst' || ast.key === 'function' || ast.key === 'class' || ast.key === 'comptime' || ast.key === 'metawhile';
export const isParseNode = (ast: unknown): ast is ParseNode => ast instanceof ParseNodeType

export type BreakType = 'break' | 'continue'

export type BytecodeInstr = 
  { type: 'comment', comment: string } |
  { type: 'binding', name: string } |
  { type: 'push', value: unknown } |
  { type: 'operator', name: string, count: number } |
  { type: 'letlocal', name: string, t: boolean, v: boolean } |
  { type: 'setlocal', name: string } |
  { type: 'list', count: number } |
  { type: 'tuple', count: number } |
  { type: 'dict', count: number } |
  { type: 'tupleast', count: number } |
  { type: 'dictast', count: number } |
  { type: 'closure', id: number } |
  { type: 'call', name: string, count: number, tcount: number } |
  { type: 'compilerfn', name: string, count: number, tcount: number } |
  { type: 'return', r: boolean } |
  { type: 'not' } |
  { type: 'pop' } |
  { type: 'nil' } |
  { type: 'beginblockast', breakType: BreakType | null, name: string | null } |
  { type: 'endblockast' } |
  { type: 'bindingast', name: string } |
  { type: 'totype' } |
  { type: 'numberast', value: number } |
  { type: 'stringast', value: string } |
  { type: 'boolast', value: boolean } |
  { type: 'setlocalast', name: string } |
  { type: 'setfieldast', name: string } |
  { type: 'fieldast', name: string } |
  { type: 'field', name: string } |
  { type: 'subscriptast' } |
  { type: 'subscript' } |
  { type: 'operatorast', name: string, count: number } |
  { type: 'constructorast', count: number } |
  { type: 'toast' } |
  { type: 'whileast' } |
  { type: 'returnast', r: boolean } |
  { type: 'breakast', n: boolean, v: boolean } |
  { type: 'continueast', v: boolean } |
  { type: 'listast', count: number } |
  { type: 'andast', count: number } |
  { type: 'orast', count: number } |
  { type: 'ifast', f: boolean, e: boolean } |
  { type: 'notast' } |
  { type: 'letast', name: string, t: boolean, v: boolean } |
  { type: 'callast', name: string, count: number, tcount: number, method?: boolean } |
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
    labelBlock: LabelBlock | null,
    expansion: {
      iteratorListIdentifier: ParseFreshIden,
      selectors: { node: ParseNode, start: ParseNode | null, end: ParseNode | null, step: ParseNode | null, indexIdentifier: ParseFreshIden }[]
    } | null
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
  [E in ParseNode as E['key']]: (out: BytecodeWriter, node: E) => void;
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
  keywords: string[] = []

  constructor(
    public id: number,
    public debugName: string,
    public name: ParseIdentifier | null,
    public typeArgs: ParseNode[],
    public args: ArgumentTypePair[],
    public returnType: ParseNode | null,
    public body: ParseNode | null,
    public inline: boolean) {}

  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1) return options.stylize(`[FunctionDefinition ${this.debugName}]`, 'special');
    const mini = depth < options.depth;
    const newOptions = Object.assign({}, options, {
      ast: true,
      depth: mini ? 3 : options.depth === null ? null : options.depth - 1,
    });
    return `${options.stylize(this.constructor.name, 'special')} ${inspect({...this}, newOptions)}`
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
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[TypeField ${this.index} ${inspect(this.sourceType)} ${this.name}]`, 'special');
  }
}
export class CompiledClass {
  metaobject: UnknownObject = Object.create(null)

  constructor(
    public location: SourceLocation,
    public debugName: string,
    public binding: Binding,
    public classDefinition: ClassDefinition,
    public type: ParameterizedType | ConcreteClassType,
    public body: Ast,
    public fields: TypeField[],
    public typeArguments: unknown[],
    public typeArgHash: unknown) {}

  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1 || depth <= options.depth || options.ast) return options.stylize(`[CompiledClass ${this.debugName}]`, 'special');
    return {...this}
  }
}

export class ClassDefinition {
  headerPrototype?: FunctionPrototype | undefined
  templatePrototype?: FunctionPrototype | undefined

  compiledClasses: CompiledClass[] = []
  concreteType: ConcreteClassType | undefined

  metaClass: ParseIdentifier | null

  isTypeConstructor: boolean = false

  constructor(
    public id: number,
    public location: SourceLocation,
    public parentScope: Scope,
    public debugName: string,
    public name: ParseIdentifier | null,
    public typeArgs: ParseNode[],
    public body: ParseNode | null) {
      this.isTypeConstructor = typeArgs.length > 0
    }

  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1 || depth <= options.depth || options.ast) return options.stylize(`[ClassDefinition ${this.debugName}]`, 'special');
    return {...this}
  }
}

export class AstRoot {
  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1) return options.stylize(`[${this.constructor.name}]`, 'special');
    const newOptions = Object.assign({}, options, {
      ast: true,
    });
    const props = { ...this}
    delete (props as any)['key']
    return options.stylize(`${this.constructor.name} `, 'string') + inspect(props, newOptions)
  }
}
export class NumberAst extends AstRoot {      key = 'number' as const;      constructor(public type: Type, public location: SourceLocation, public value: number) { super() } }
export class StringAst extends AstRoot {      key = 'string' as const;      constructor(public type: Type, public location: SourceLocation, public value: string) { super() } }
export class BindingAst extends AstRoot {     key = 'binding' as const;     constructor(public type: Type, public location: SourceLocation, public binding: Binding) { super() } }
export class BoolAst extends AstRoot {        key = 'bool' as const;        constructor(public type: Type, public location: SourceLocation, public value: boolean) { super() } }
export class LetAst extends AstRoot {         key = 'let' as const;         constructor(public type: Type, public location: SourceLocation, public binding: Binding, public value: Ast | null) { super() } }
export class SetAst extends AstRoot {         key = 'set' as const;         constructor(public type: Type, public location: SourceLocation, public binding: Binding, public value: Ast) { super() } }
export class OperatorAst extends AstRoot {    key = 'operator' as const;    constructor(public type: Type, public location: SourceLocation, public operator: string, public args: Ast[]) { super() } }
export class IfAst extends AstRoot {          key = 'if' as const;          constructor(public type: Type, public location: SourceLocation, public expr: Ast, public trueBody: Ast, public falseBody: Ast | null) { super() } }
export class ListAst extends AstRoot {        key = 'list' as const;        constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class CallAst extends AstRoot {        key = 'call' as const;        constructor(public type: Type, public location: SourceLocation, public func: ExternalFunction, public args: Ast[]) { super() } }
export class UserCallAst extends AstRoot {    key = 'usercall' as const;    constructor(public type: Type, public location: SourceLocation, public binding: Binding, public args: Ast[]) { super() } }
export class AndAst extends AstRoot {         key = 'and' as const;         constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class OrAst extends AstRoot {          key = 'or' as const;          constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class StatementsAst extends AstRoot {  key = 'statements' as const;  constructor(public type: Type, public location: SourceLocation, public statements: Ast[]) { super() } }
export class WhileAst extends AstRoot {       key = 'while' as const;       constructor(public type: Type, public location: SourceLocation, public condition: Ast, public body: Ast) { super() } }
export class ReturnAst extends AstRoot {      key = 'return' as const;      constructor(public type: Type, public location: SourceLocation, public expr: Ast | null) { super() } }
export class BreakAst extends AstRoot {       key = 'break' as const;       constructor(public type: Type, public location: SourceLocation, public binding: Binding, public expr: Ast | null) { super() } }
export class BlockAst extends AstRoot {       key = 'block' as const;       constructor(public type: Type, public location: SourceLocation, public binding: Binding, public body: Ast) { super() } }
export class FieldAst extends AstRoot {       key = 'field' as const;       constructor(public type: Type, public location: SourceLocation, public left: Ast, public field: TypeField) { super() } }
export class SetFieldAst extends AstRoot {    key = 'setfield' as const;    constructor(public type: Type, public location: SourceLocation, public left: Ast, public field: TypeField, public value: Ast) { super() } }
export class VoidAst extends AstRoot {        key = 'void' as const;        constructor(public type: Type, public location: SourceLocation) { super() } }
export class CastAst extends AstRoot {        key = 'cast' as const;        constructor(public type: Type, public location: SourceLocation, public expr: Ast) { super() } }
export class SubscriptAst extends AstRoot {   key = 'subscript' as const;   constructor(public type: Type, public location: SourceLocation, public left: Ast, public right: Ast) { super() } }
export class NotAst extends AstRoot {         key = 'not' as const;         constructor(public type: Type, public location: SourceLocation, public expr: Ast) { super() } }
export class ConstructorAst extends AstRoot { key = 'constructor' as const; constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }

export type Ast = NumberAst | LetAst | SetAst | OperatorAst | IfAst | ListAst | CallAst | AndAst | UserCallAst |
  OrAst | StatementsAst | WhileAst | ReturnAst | SetFieldAst | VoidAst | CastAst | SubscriptAst | ConstructorAst |
  BindingAst | StringAst | NotAst | FieldAst | BlockAst | BreakAst | BoolAst
export const isAst = (value: unknown): value is Ast => value instanceof AstRoot;

export class Tuple {
  constructor(public values: unknown[]) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[Tuple ...]`, 'special');
  }
}

export const isPlainObject = (obj: unknown): obj is UnknownObject & { _object: true } => {
  return !!obj && typeof obj === 'object' && Object.getPrototypeOf(obj) === Object.prototype
}

export class Binding {
  definitionCompiler: SubCompilerState | undefined
  constructor(public name: string, public type: Type) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[Binding ${this.name} ${inspect(this.type)}]`, 'special');
  }
}
export class FreshBindingToken {
  uniqueId = getUniqueId(this)
  constructor(public debugName: string) {}
  get identifier() { return `tmp_${this.debugName}_${this.uniqueId}` }
}

let uniqueId = 1000
// const uniqueMap = new WeakMap<object, number>()
const UNIQUE_ID = Symbol('UNIQUE_ID')
const getUniqueId = (obj: any) => {
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

export interface TypeInfo {
  fields: TypeField[]
  metaobject: UnknownObject
  isReferenceType: boolean
}
export class TypeRoot {}
export class PrimitiveType extends TypeRoot {
  constructor(public typeName: string, public typeInfo: TypeInfo) { super() }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[PrimitiveType ${this.typeName}]`, 'special');
  }
}

export class ConcreteClassType extends TypeRoot {
  constructor(public compiledClass: CompiledClass, public typeInfo: TypeInfo) { 
    super()
  }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[ConcreteClassType ${this.compiledClass.debugName}]`, 'special');
  }
}
export class ParameterizedType extends TypeRoot {
  constructor(public typeConstructor: TypeConstructor, public args: unknown[], public typeInfo: TypeInfo) {
    super()
  }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1 || depth < options.depth) {
      const t = this.typeConstructor instanceof ExternalTypeConstructor ? this.typeConstructor.typeName : this.typeConstructor.debugName
      return options.stylize(`[ParameterizedType ${t}, ...]`, 'special');
    }
    return options.stylize(`[ParameterizedType ${inspect(this.typeConstructor, { depth: 0})}, ${this.args.map(x => inspect(x)).join(', ')}]`, 'special')
  }
}

export class TypeVariable {
  constructor(public name: string) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[TypeVariable ${this.name}]`, 'special');
  }
}

export class TypeMatcher {
  constructor(public typeConstructor: ExternalTypeConstructor | ClassDefinition, public args: unknown[], public typeVariables: TypeVariable[]) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[TypeMatcher]`, 'special');
  }
}
export class ExternalTypeConstructor {
  metaobject: {} = Object.create(null)
  constructor(public typeName: string, public createType: (argTypes: Type[]) => ParameterizedType) { }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[ExternalTypeConstructor ${this.typeName}]`, 'special');
  }
}

export type Type = PrimitiveType | ConcreteClassType | ParameterizedType
export type TypeConstructor = ExternalTypeConstructor | ClassDefinition // Type constructor for already-compiled types
export const isType = (type: unknown): type is Type => type instanceof TypeRoot

export class Closure {
  constructor(public func: FunctionDefinition, public scope: Scope, public lexicalParent: SubCompilerState) {}

  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1) return options.stylize(`[${this.constructor.name}]`, 'special');
    const newOptions = Object.assign({}, options, {
      ast: true,
      depth: options.depth === null ? null : options.depth - 1,
    });

    const props = {...this}
    return `${options.stylize(this.constructor.name, 'special')} ${inspect(props, newOptions)}`
  }
}

export const ScopeEventsSymbol = Symbol('ScopeEventsSymbol')
export const ScopeParentSymbol = Symbol('ScopeParentSymbol')
export type Scope = UnknownObject & {
  _scope: true,
  [ScopeEventsSymbol]: {[key:string]:Event<unknown, CompilerError>}
}
const ScopePrototype = Object.assign(Object.create(null), {
  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1 || depth <= options.depth) return options.stylize(`[Scope]`, 'special');
    return inspect({...this})
  }
});
export const createScope = (obj: object, parentScope: Scope | undefined) => 
  Object.assign(Object.create(ScopePrototype), {
    [ScopeParentSymbol]: parentScope,
    ...obj
  }) as Scope;

export class ExternalFunction {
  constructor(public name: string, public returnType: Type, public func: Function) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[ExternalFunction ${this.name}]`, 'special');
  }
}

export const VoidType =       new PrimitiveType("void",     { fields: [], metaobject: Object.create(null), isReferenceType: false })
export const IntType =        new PrimitiveType("int",      { fields: [], metaobject: Object.create(null), isReferenceType: false })
export const BoolType =       new PrimitiveType("bool",     { fields: [], metaobject: Object.create(null), isReferenceType: false })
export const FloatType =      new PrimitiveType("float",    { fields: [], metaobject: Object.create(null), isReferenceType: false })
export const DoubleType =     new PrimitiveType("double",   { fields: [], metaobject: Object.create(null), isReferenceType: false })
export const FunctionType =   new PrimitiveType("function", { fields: [], metaobject: Object.create(null), isReferenceType: false })
export const RawPointerType = new PrimitiveType("rawptr",   { fields: [], metaobject: Object.create(null), isReferenceType: false })

export const StringType = (() => {
  const type = new PrimitiveType("string", { fields: [], metaobject: Object.create(null), isReferenceType: false })
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "length", type, 0, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "data", type, 1, RawPointerType))
  return type;
})()

export const ListTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("List", (argTypes) => {
  compilerAssert(argTypes.length === 1, "Expected one type arg", { argTypes })
  const type = new ParameterizedType(ListTypeConstructor, argTypes, { fields: [], metaobject: Object.create(null), isReferenceType: false });
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "length", type, 0, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "capacity", type, 1, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "data", type, 2, RawPointerType))
  return type;
})
export const TupleTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("Tuple", (argTypes) => {
  const type = new ParameterizedType(TupleTypeConstructor, argTypes, { fields: [], metaobject: Object.create(null), isReferenceType: false });
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "length", type, 0, IntType))
  argTypes.forEach((argType, i) => {
    type.typeInfo.fields.push(new TypeField(SourceLocation.anon, `_${i+1}`, type, i, argType))
  })
  return type;
})

export const BuiltinTypes = {
  int: IntType,
  float: FloatType,
  double: DoubleType,
  void: VoidType,
  string: StringType,
  bool: BoolType,
  List: ListTypeConstructor,
  Tuple: TupleTypeConstructor,
}

class TypeTable {
  array: Type[] = [] // TODO: Use a map here?
  constructor() { }

  get(type: Type) { 
    for (const t of this.array) {
      if (typesEqual(t, type)) return t;
    }
  }
  insert<T extends Type>(type: T) { 
    this.array.push(type);
    return type;
  }
  getOrInsert<T extends Type>(type: T): T {
    let v = this.get(type)
    if (v) return v as T;
    return this.insert(type)
  }
  
}

// Don't use directly, use type table to see if types are equal
const typesEqual = (t1: unknown, t2: any): boolean => {
  if (t1 instanceof ExternalTypeConstructor) return t1 === t2;
  if (!isType(t1)) {
    return hashValues([t1]) === hashValues([t2])
  }
  compilerAssert(t1 && t2, "Unexpected", { t1, t2 })
  if (Object.getPrototypeOf(t1) !== Object.getPrototypeOf(t2)) return false;
  if (t1 instanceof PrimitiveType) return t1 == t2;
  if (t1 instanceof ConcreteClassType) return t1.compiledClass == t2.compiledClass;
  if (t1 instanceof ParameterizedType) {
    if (!typesEqual(t1.typeConstructor, t2.typeConstructor)) return false;
    if (t1.args.length !== t2.args.length) return false;
    return t1.args.every((x, i) => typesEqual(x, t2.args[i]))
  }
  return false;
}

export const typeMatcherEquals = (matcher: TypeMatcher, expected: Type, output: UnknownObject) => {
  const testTypeConstructor = (matcher: ExternalTypeConstructor | ClassDefinition, expected: TypeConstructor) => {
    if (matcher instanceof ExternalTypeConstructor) {
      if (matcher === expected) return true
      compilerAssert(false, "$matcher does not equal $expected", { matcher, expected })
    }
    if (expected instanceof ClassDefinition) {
      if (matcher !== expected) {
        compilerAssert(false, "$matcher does not equal $expected", { matcher, expected: expected })
        return false;
      }
      return true
    }
    compilerAssert(false, "Not implemented", { matcher, expected })
  }
  
  const test = (matcher: unknown, expected: unknown) => {
    if (matcher instanceof TypeMatcher && expected instanceof ParameterizedType) {
      if (!testTypeConstructor(matcher.typeConstructor, expected.typeConstructor)) {
        compilerAssert(false, "Not implemented", { matcher, expected })
        return false;
      }

      let i = 0;
      for (const arg of matcher.args) {
        if (!test(arg, expected.args[i])) return false;
        i++
      }
      return true;
    }
    if (matcher instanceof TypeVariable) {
      if (output[matcher.name]) return output[matcher.name] === expected;
      output[matcher.name] = expected;
      return true
    }
    if (matcher instanceof TypeMatcher && expected instanceof ConcreteClassType) {
      compilerAssert(false, "Not implemented", { matcher, expected })
      return false;
    }
    compilerAssert(false, "Not implemented", { matcher, expected })
  }
  return test(matcher, expected)
}

export const isParameterizedTypeOf = (a: Type, expected: TypeConstructor) => {
  return a instanceof ParameterizedType && a.typeConstructor === expected;
}


export const expectMap = <T extends UnknownObject, K extends keyof T>(object: T, key: K, message: string, info: object = {}) => {
  compilerAssert(object[key] !== undefined, message, { object, key, ...info }); 
  return object[key];
};
export const expectAll = <T>(fn: (x: unknown) => x is T, expected: unknown[], info: object = {}) => {
  compilerAssert(expected.every(fn), "Expected something got $expected", { expected, ...info }); 
  return expected;
};
export const expectAst = (expected: unknown, info: object = {}) => {
  compilerAssert(isAst(expected), "Expected AST got $expected", { expected, ...info }); 
  return expected;
};
export const expectType = (expected: unknown, info: object = {}) => {
  compilerAssert(isType(expected), "Expected Type got $expected", { expected, ...info }); 
  return expected;
};
export const expectAsts = (expected: unknown[], info: object = {}) => {
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

// Isn't it weird that these are similar but not the same?
export class LabelBlock {
  public completion: ((value: unknown) => void)[] = []
  public type: Type | null = null;
  constructor(
    public parent: LabelBlock | null,
    public name: string | null,
    public breakType: BreakType | null,
    public binding: Binding | null) {}
}

export const findLabelBlockByType = (labelBlock: LabelBlock | null, breakType: BreakType | null) => {
  let block = labelBlock
  while (block) {
    if (block.breakType === breakType) return block
    block = block.parent;
  }
  compilerAssert(false, breakType === 'continue' ? `Invalid continue outside a loop` : `Invalid ${breakType} outside a block`, { labelBlock })
}

export const findLabelByBinding = (labelBlock: LabelBlock | null, binding: Binding) => {
  let block = labelBlock
  while (block) {
    if (block.binding === binding) return block
    block = block.parent;
  }
  compilerAssert(false, `No block with the give name found`, { labelBlock })
}

export type Logger = { log: (...args: any[]) => void }

export type GlobalCompilerState = {
  compiledFunctions: Map<Binding, CompiledFunction>;
  functionDefinitions: FunctionDefinition[],
  classDefinitions: ClassDefinition[],
  moduleLoader: ModuleLoader
  methods: WeakMap<Scope, [TypeConstructor, Closure][]>,
  allWaitingEvents: Event<unknown, unknown>[],
  logger: Logger,
  typeTable: TypeTable
}

export interface ParsedModule {
  classDefs: ParserClassDecl[]
  functionDecls: ParserFunctionDecl[]
  rootNode: ParseStatements
}
export class Module {
  constructor(public debugName: string, public compilerState: SubCompilerState, public parsedModule: ParsedModule) {}

  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[Module ${this.debugName}]`, 'special');
  }
}

export interface ModuleLoader {
  cache: {[key:string]: Module}
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
    methods: new WeakMap(),
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
  inlineIntoCompiler: SubCompilerState | undefined
  labelBlock: LabelBlock | null
  nextLabelBlockDepth: number = 0; // Just used for debug labelling
  globalCompiler: GlobalCompilerState

  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1) return options.stylize(`[CompilerState ${this.debugName}]`, 'special');
    const mini = depth < options.depth;
    const newOptions = Object.assign({}, options, {
      depth: mini ? 1 : options.depth === null ? null : options.depth - 1,
    });
    return inspect({...this}, newOptions)
  }
}

export const pushSubCompilerState = (ctx: TaskContext, obj: { debugName: string, vm?: Vm, func?: FunctionDefinition, scope: Scope, lexicalParent: SubCompilerState | undefined }) => {
  const state = new SubCompilerState(obj.debugName)
  state.globalCompiler = ctx.globalCompiler
  state.prevCompilerState = ctx.subCompilerState;
  state.nextLabelBlockDepth = ctx.subCompilerState?.nextLabelBlockDepth ?? 0
  state.lexicalParent = obj.lexicalParent
  state.scope = obj.scope
  state.functionCompiler = state.lexicalParent?.functionCompiler
  state.inlineIntoCompiler = state.lexicalParent?.inlineIntoCompiler
  ctx.subCompilerState = state;
  return state;
}

export function bytecodeToString(bytecodeProgram: BytecodeProgram) {
  const { locations, code } = bytecodeProgram
  const instr = (instr: BytecodeInstr) => {
    const { type, ...args } = instr;
    const values = Object.entries(args)
      .map(([k, v]) => `${k}: ${v}`)
      .join(", ");
    return `${type.padStart("beginblockast".length, " ")}  ${values}`;
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

export type FileWriter = {
  write(
    chunk: string | ArrayBufferView | ArrayBuffer | SharedArrayBuffer,
  ): number;
}
export type AstWriterTable = {
  [A in Ast as A['key']]: (writer: CodegenFunctionWriter, ast: A) => void;
}
export type CodegenFunctionWriter = {
  writer: CodegenWriter
  argSlots: number
  returnSlots: number
  bytecode: number[]
  constants: Map<unknown, number>
  constantSlots: number[]
  nextConstantSlot: number
  locals: { binding: Binding, slot: number, scopeIndex: number }[]
  blocks: { binding: Binding, slotIndex: number, patches: { location: number }[] }[],
  currentScopeIndex: number
  nextLocalSlot: number
}
export type CodegenWriter = {
  functions: CodegenFunctionWriter[]
  globalCompilerState: GlobalCompilerState
  functionToIndex: Map<Binding, number>
  typeSizes: Map<Type, number>
}