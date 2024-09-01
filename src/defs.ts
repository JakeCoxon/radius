import { createParameterizedExternalType } from "./compiler";
import { Event, Task } from "./tasks";

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

const makeColor = (x: unknown) => {
  return Inspect({
    [Inspect.custom](depth: any, options: any, inspect: any) {
      return options.stylize(x, 'special');
    }
  }, { colors: true })
}

export const isArray = (value: any): value is unknown[] => Array.isArray(value)
export const filterNotNull = <T>(arr: (T | null | undefined)[]): T[] => arr.filter(x => x !== null && x !== undefined) as T[]

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
    const str = this.value.replace(/\n/g, '\\n')
    if (str == '') return options.stylize(`[Token ${this.type}]`, 'string');
    return options.stylize(`[Token ${str}]`, 'string');
  }
}
export const createToken = (source: Source, value: any, type = "NONE"): Token => Object.assign(Object.create(TokenRoot), { value, type, location: new SourceLocation(0, 0, source) });
export const createAnonymousToken = (value: any, type = "NONE"): Token => Object.assign(Object.create(TokenRoot), { value, type, location: new SourceLocation(-1, -1, null!) });

export type ParserFunctionParameter = {
  name: ParseIdentifier | ParseFreshIden,
  type: ParseNode | null,
  storage: 'ref' | null
}
// These are reference types that id will be filled in later.
export type ParserFunctionDecl = {
  debugName: string, anonymous?: boolean,
  token: Token, functionMetaName: ParseIdentifier | null,
  name: ParseIdentifier | null, typeParams: ParseNode[], params: ParserFunctionParameter[], 
  returnType: ParseNode | null, body: ParseNode | null, keywords: ParseNode[],
  annotations: ParseNode[], variadic: boolean }

export const createAnonymousParserFunctionDecl = (debugName: string, sourceToken: Token, params: ParserFunctionParameter[], body: ParseNode) => {
  const decl: ParserFunctionDecl = {
    debugName: debugName,
    params: params,
    name: null,
    body: body,
    returnType: null,
    anonymous: true,
    token: sourceToken,
    keywords: [],
    typeParams: [],
    functionMetaName: null,
    annotations: [],
    variadic: false
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

export class ParseVoid extends ParseNodeType {         key = 'void' as const;         constructor(public token: Token) { super();} }
export class ParseIdentifier extends ParseNodeType {   key = 'identifier' as const;   constructor(public token: Token) { super();} }
export class ParseSymbol extends ParseNodeType {       key = 'symbol' as const;       constructor(public token: Token) { super();} }
export class ParseNil extends ParseNodeType {          key = 'nil' as const;          constructor(public token: Token) { super();} }
export class ParseNumber extends ParseNodeType {       key = 'number' as const;       constructor(public token: Token) { super();} }
export class ParseString extends ParseNodeType {       key = 'string' as const;       constructor(public token: Token, public string: string) { super();} }
export class ParseBoolean extends ParseNodeType {      key = 'boolean' as const;      constructor(public token: Token) { super();} }
export class ParseStatements extends ParseNodeType {   key = 'statements' as const;   constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseLet extends ParseNodeType {          key = 'let' as const;          constructor(public token: Token, public left: ParseIdentifier | ParseFreshIden | ParseTuple | ParseCase, public type: ParseNode | null, public value: ParseNode | null) { super();} }
export class ParseSet extends ParseNodeType {          key = 'set' as const;          constructor(public token: Token, public left: ParseNode, public value: ParseNode) { super();} }
export class ParseOperator extends ParseNodeType {     key = 'operator' as const;     constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseNote extends ParseNodeType {         key = 'note' as const;         constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseMeta extends ParseNodeType {         key = 'meta' as const;         constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseMetaIf extends ParseNodeType {       key = 'metaif' as const;       constructor(public token: Token, public expr: ParseIf) { super();} }
export class ParseMetaFor extends ParseNodeType {      key = 'metafor' as const;      constructor(public token: Token, public expr: ParseFor) { super();} }
export class ParseMetaWhile extends ParseNodeType {    key = 'metawhile' as const;    constructor(public token: Token, public expr: ParseWhile) { super();} }
export class ParseCompTime extends ParseNodeType {     key = 'comptime' as const;     constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseCall extends ParseNodeType {         key = 'call' as const;         constructor(public token: Token, public left: ParseNode, public args: ParseNode[], public typeArgs: ParseNode[]) { super();} }
export class ParseList extends ParseNodeType {         key = 'list' as const;         constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseListComp extends ParseNodeType {     key = 'listcomp' as const;     constructor(public token: Token, public exprs: ParseNode[], public mapping: ParseNode[], public reduce: ParseNode | null) { super();} }
export class ParseIterator extends ParseNodeType {     key = 'iterator' as const;     constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseOr extends ParseNodeType {           key = 'or' as const;           constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseAnd extends ParseNodeType {          key = 'and' as const;          constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseElse extends ParseNodeType {         key = 'else' as const;         constructor(public token: Token, public body: ParseNode) { super();} }
export class ParseIf extends ParseNodeType {           key = 'if' as const;           constructor(public token: Token, public isExpr: boolean, public condition: ParseNode, public trueBody: ParseNode, public falseBody: ParseIf | ParseElse | null) { super();} }
export class ParseLetConst extends ParseNodeType {     key = 'letconst' as const;     constructor(public token: Token, public name: ParseIdentifier | ParseFreshIden, public value: ParseNode) { super();} }
export class ParseFunction extends ParseNodeType {     key = 'function' as const;     constructor(public token: Token, public functionDecl: ParserFunctionDecl) { super();} }
export class ParseClass extends ParseNodeType {        key = 'class' as const;        constructor(public token: Token, public classDecl: ParserClassDecl) { super();} }
export class ParseReturn extends ParseNodeType {       key = 'return' as const;       constructor(public token: Token, public expr: ParseNode | null) { super();} }
export class ParseBreak extends ParseNodeType {        key = 'break' as const;        constructor(public token: Token, public name: ParseIdentifier | ParseFreshIden | null, public expr: ParseNode | null) { super();} }
export class ParseContinue extends ParseNodeType {     key = 'continue' as const;     constructor(public token: Token, public name: ParseIdentifier | ParseFreshIden | null) { super();} }
export class ParseBreakOpt extends ParseNodeType {     key = 'breakopt' as const;     constructor(public token: Token, public name: ParseIdentifier | ParseFreshIden | null) { super();} }
export class ParseFor extends ParseNodeType {          key = 'for' as const;          constructor(public token: Token, public left: ParseIdentifier | ParseTuple | ParseCase, public expr: ParseNode, public body: ParseNode) { super();} }
export class ParseIs extends ParseNodeType {           key = 'is' as const;           constructor(public token: Token, public expr: ParseNode, public type: ParseNode) { super();} }
export class ParseOrElse extends ParseNodeType {       key = 'orelse' as const;       constructor(public token: Token, public expr: ParseNode, public orElse: ParseNode) { super();} }
export class ParseCast extends ParseNodeType {         key = 'cast' as const;         constructor(public token: Token, public expr: ParseNode, public as: ParseNode) { super();} }
export class ParseOpEq extends ParseNodeType {         key = 'opeq' as const;         constructor(public token: Token, public left: ParseNode, public right: ParseNode) { super();} }
export class ParseWhile extends ParseNodeType {        key = 'while' as const;        constructor(public token: Token, public condition: ParseNode, public body: ParseNode) { super();} }
export class ParseWhileExpr extends ParseNodeType {    key = 'whileexpr' as const;    constructor(public token: Token, public condition: ParseNode, public body: ParseNode) { super();} }
export class ParseForExpr extends ParseNodeType {      key = 'forexpr' as const;      constructor(public token: Token, public left: ParseIdentifier | ParseTuple | ParseCase, public expr: ParseNode, public body: ParseNode) { super();} }
export class ParseNot extends ParseNodeType {          key = 'not' as const;          constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseField extends ParseNodeType {        key = 'field' as const;        constructor(public token: Token, public expr: ParseNode, public field: ParseIdentifier) { super();} }
export class ParseExpand extends ParseNodeType {       key = 'expand' as const;       constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseDict extends ParseNodeType {         key = 'dict' as const;         constructor(public token: Token, public pairs: [ParseNode, ParseNode][]) { super();} }
export class ParsePostCall extends ParseNodeType {     key = 'postcall' as const;     constructor(public token: Token, public expr: ParseNode, public arg: ParseNode) { super();} }
export class ParseSlice extends ParseNodeType {        key = 'slice' as const;        constructor(public token: Token, public expr: ParseNode, public start: ParseNode | null, public end: ParseNode | null, public step: ParseNode | null, public isStatic: boolean) { super();} }
export class ParseCase extends ParseNodeType {         key = 'case' as const;         constructor(public token: Token, public name: ParseIdentifier, public exprs: ParseNode[]) { super();} }
export class ParseMatch extends ParseNodeType {        key = 'match' as const;        constructor(public token: Token, public expr: ParseNode, public cases: [ParseIdentifier | ParseTuple | ParseCase, ParseNode][]) { super();} }
export class ParseSubscript extends ParseNodeType {    key = 'subscript' as const;    constructor(public token: Token, public expr: ParseNode, public subscript: ParseNode, public isStatic: boolean) { super();} }
export class ParseTuple extends ParseNodeType {        key = 'tuple' as const;        constructor(public token: Token, public exprs: ParseNode[]) { super();} }
export class ParseBlock extends ParseNodeType {        key = 'block' as const;        constructor(public token: Token, public breakType: BreakType | null, public name: ParseIdentifier | ParseFreshIden | null, public statements: ParseStatements) { super();} }
export class ParseImportName extends ParseNodeType {   key = 'importname' as const;   constructor(public token: Token, public identifier: ParseIdentifier, public rename: ParseIdentifier | null) { super();} }
export class ParseImport extends ParseNodeType {       key = 'import' as const;       constructor(public token: Token, public module: ParseIdentifier, public rename: ParseIdentifier | null, public imports: ParseImportName[]) { super();} }
export class ParseValue extends ParseNodeType {        key = 'value' as const;        constructor(public token: Token, public value: unknown) { super();} }
export class ParseQuestion extends ParseNodeType {     key = 'question' as const;     constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseQuote extends ParseNodeType {        key = 'quote' as const;        constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseFold extends ParseNodeType {         key = 'fold' as const;         constructor(public token: Token, public expr: ParseNode) { super();} }
export class ParseNamedArg extends ParseNodeType {     key = 'namedarg' as const;     constructor(public token: Token, public name: ParseIdentifier, public expr: ParseNode) { super();} }
export class ParseBytecode extends ParseNodeType {     key = 'bytecode' as const;     constructor(public token: Token, public bytecode: { code: BytecodeInstr[]; locations: SourceLocation[]; }) { super();} }
export class ParseFreshIden extends ParseNodeType {    key = 'freshiden' as const;    constructor(public token: Token, public freshBindingToken: FreshBindingToken) { super();} }
export class ParseConstructor extends ParseNodeType {  key = 'constructor' as const;  constructor(public token: Token, public type: ParseNode, public args: ParseNode[]) { super();} }
export class ParseCompilerIden extends ParseNodeType { key = 'compileriden' as const; constructor(public token: Token, public value: string) { super();} }
export class ParseEvalFunc extends ParseNodeType {     key = 'evalfunc' as const;     constructor(public token: Token, public func: (vm: Vm) => void | Task<unknown, CompilerError>, public args: ParseNode[], public typeArgs: ParseNode[]) { super();} }
export class ParseConcurrency extends ParseNodeType {  key = 'concurrency' as const;  constructor(public token: Token, public fns: ParseNode[]) { super();} }

export type ParseNode = ParseStatements | ParseLet | ParseSet | ParseOperator | ParseIdentifier | 
  ParseNumber | ParseMeta | ParseCompTime | ParseLetConst | ParseCall | ParseList | ParseOr | ParseAnd | 
  ParseIf | ParseFunction | ParseString | ParseReturn | ParseBreak | ParseContinue | ParseFor | ParseCast |
  ParseOpEq | ParseWhile | ParseWhileExpr | ParseForExpr | ParseNot | ParseField | ParseExpand | ParseListComp |
  ParseDict | ParsePostCall | ParseSymbol | ParseNote | ParseSlice | ParseSubscript | ParseTuple | ParseClass |
  ParseNil | ParseBoolean | ParseElse | ParseMetaIf | ParseMetaFor | ParseMetaWhile | ParseBlock | ParseImport | 
  ParseCompilerIden | ParseValue | ParseConstructor | ParseQuote | ParseBytecode | ParseFreshIden | ParseFold | 
  ParseNamedArg | ParseEvalFunc | ParseConcurrency | ParseVoid | ParseIs | ParseOrElse | ParseQuestion | 
  ParseIterator | ParseCase | ParseMatch | ParseBreakOpt

// Void types mean that in secondOrder compilation, the AST doesn't return an AST
export const isParseVoid = (ast: ParseNode) => ast.key == 'letconst' || ast.key === 'function' || ast.key === 'class' || ast.key === 'comptime' || ast.key === 'metawhile';
export const isParseNode = (ast: unknown): ast is ParseNode => ast instanceof ParseNodeType

export type BreakType = 'break' | 'continue' | 'option'

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
  { type: 'callobj', count: number, tcount: number } |
  { type: 'compilerfn', name: string, count: number, tcount: number } |
  { type: 'return', r: boolean } |
  { type: 'namedarg' } |
  { type: 'not' } |
  { type: 'pop' } |
  { type: 'nil' } |
  { type: 'beginblockast', breakType: BreakType | null, name: string | null } |
  { type: 'endblockast' } |
  { type: 'bindingast', name: string } |
  { type: 'totype' } |
  { type: 'numberast', value: string } |
  { type: 'stringast', value: string } |
  { type: 'boolast', value: boolean } |
  { type: 'setlocalast', name: string } |
  { type: 'setfieldast', name: string } |
  { type: 'fieldast', name: string } |
  { type: 'field', name: string } |
  { type: 'subscriptast' } |
  { type: 'staticsubscriptast' } |
  { type: 'setsubscriptast' } |
  { type: 'subscript' } |
  { type: 'setmetaast' } |
  { type: 'operatorast', name: string, count: number } |
  { type: 'constructorast', count: number } |
  { type: 'toast' } |
  { type: 'whileast' } |
  { type: 'returnast', r: boolean } |
  { type: 'breakast', v: boolean, named: boolean, breakType: BreakType } |
  { type: 'listast', count: number } |
  { type: 'andast', count: number } |
  { type: 'orast', count: number } |
  { type: 'ifast', f: boolean, e: boolean } |
  { type: 'notast' } |
  { type: 'letast', name: string, t: boolean, v: boolean } |
  { type: 'letmatchast', t: boolean, v: boolean } |
  { type: 'callast', name: string, count: number, tcount: number, method?: boolean } |
  { type: 'pushqs' } |
  { type: 'popqs' } |
  { type: 'appendq' } |
  { type: 'jump', address: number } |
  { type: 'jumpf', address: number } |
  { type: 'evalfunc', func: (vm: Vm) => void | Task<unknown, CompilerError> } |
  { type: 'concurrency', count: number } |
  { type: 'voidast' } |
  { type: 'halt' }


export type InstructionMapping = {
  [T in BytecodeInstr as T['type']]: (vm: Vm, instr: T) => unknown | Task<unknown, CompilerError>;
}

export type BytecodeProgram = {
  code: BytecodeInstr[]
  locations: SourceLocation[]
}

export type ExpansionSelector = {
  node: ParseNode, start: ParseNode | null, 
  end: ParseNode | null, step: ParseNode | null,
  elemIdentifier: ParseFreshIden,
  indexIdentifier: ParseFreshIden | null,
  setterIdentifier: ParseFreshIden | null,
}
export type ExpansionCompilerState = {
  debugName: string,
  location: SourceLocation,
  optimiseSimple?: boolean,
  loopBodyNode: ParseNode | null,
  loopBodyMeta: ParseNode[],
  iteratorListIdentifier: ParseFreshIden,
  fold: { iden: ParseFreshIden, initial: ParseNode } | null,
  lets: ParseNode[],
  metaResult: ParseNode,
  metaResultIden: ParseFreshIden,
  setterSelector: ExpansionSelector | null,
  breakIden: ParseFreshIden,
  selectors: ExpansionSelector[],
  filterNode: ParseNode | null,
  whileNode: ParseNode | null,
}
export interface BytecodeWriter {
  location: SourceLocation
  bytecode: {
    code: BytecodeInstr[]
    locations: SourceLocation[],
  },
  state: {
    optionalBlock?: { didBreak: boolean }  | null,
    labelBlock: LabelBlock | null,
    expansion: ExpansionCompilerState | null
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
  externalName: string | undefined = undefined

  constructor(
    public id: number,
    public debugName: string,
    public name: ParseIdentifier | null,
    public typeParams: ParseNode[],
    public params: ParserFunctionParameter[],
    public returnType: ParseNode | null,
    public body: ParseNode | null,
    public inline: boolean,
    public annotations: ParseNode[] = [],
    public variadic: boolean = false) {}

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
    return options.stylize(`[TypeField ${this.index} ${this.sourceType.shortName} ${this.name} : ${this.fieldType.shortName}]`, 'special');
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
  keywords: string[] = []

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

  get shortName(): string { return this.name?.token.value ?? 'unknown' }
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
export class NumberAst extends AstRoot {        key = 'number' as const;         constructor(public type: Type, public location: SourceLocation, public value: number) { super() } }
export class StringAst extends AstRoot {        key = 'string' as const;         constructor(public type: Type, public location: SourceLocation, public value: string) { super() } }
export class BindingAst extends AstRoot {       key = 'binding' as const;        constructor(public type: Type, public location: SourceLocation, public binding: Binding) { super() } }
export class BoolAst extends AstRoot {          key = 'bool' as const;           constructor(public type: Type, public location: SourceLocation, public value: boolean) { super() } }
export class LetAst extends AstRoot {           key = 'let' as const;            constructor(public type: Type, public location: SourceLocation, public binding: Binding, public value: Ast | null) { super() } }
export class SetAst extends AstRoot {           key = 'set' as const;            constructor(public type: Type, public location: SourceLocation, public binding: Binding, public value: Ast) { super() } }
export class OperatorAst extends AstRoot {      key = 'operator' as const;       constructor(public type: Type, public location: SourceLocation, public operator: string, public args: Ast[]) { super() } }
export class IfAst extends AstRoot {            key = 'if' as const;             constructor(public type: Type, public location: SourceLocation, public expr: Ast, public trueBody: Ast, public falseBody: Ast | null) { super() } }
export class ListAst extends AstRoot {          key = 'list' as const;           constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class CallAst extends AstRoot {          key = 'call' as const;           constructor(public type: Type, public location: SourceLocation, public binding: Binding, public args: Ast[], public typeArgs: unknown[]) { super() } }
export class UserCallAst extends AstRoot {      key = 'usercall' as const;       constructor(public type: Type, public location: SourceLocation, public binding: Binding, public args: Ast[]) { super() } }
export class AndAst extends AstRoot {           key = 'and' as const;            constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class OrAst extends AstRoot {            key = 'or' as const;             constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class StatementsAst extends AstRoot {    key = 'statements' as const;     constructor(public type: Type, public location: SourceLocation, public statements: Ast[]) { super() } }
export class WhileAst extends AstRoot {         key = 'while' as const;          constructor(public type: Type, public location: SourceLocation, public condition: Ast, public body: Ast) { super() } }
export class ReturnAst extends AstRoot {        key = 'return' as const;         constructor(public type: Type, public location: SourceLocation, public expr: Ast | null) { super() } }
export class BreakAst extends AstRoot {         key = 'break' as const;          constructor(public type: Type, public location: SourceLocation, public binding: Binding, public expr: Ast | null) { super() } }
export class BlockAst extends AstRoot {         key = 'block' as const;          constructor(public type: Type, public location: SourceLocation, public binding: Binding, public breakExprBinding: Binding | null, public body: Ast) { super() } }
export class FieldAst extends AstRoot {         key = 'field' as const;          constructor(public type: Type, public location: SourceLocation, public left: Ast, public field: TypeField) { super() } }
export class ValueFieldAst extends AstRoot {    key = 'valuefield' as const;     constructor(public type: Type, public location: SourceLocation, public left: BindingAst, public fieldPath: TypeField[]) { super() } }
export class SetFieldAst extends AstRoot {      key = 'setfield' as const;       constructor(public type: Type, public location: SourceLocation, public left: Ast, public field: TypeField, public value: Ast) { super() } }
export class SetValueFieldAst extends AstRoot { key = 'setvaluefield' as const;  constructor(public type: Type, public location: SourceLocation, public left: BindingAst, public fieldPath: TypeField[], public value: Ast) { super() } }
export class VoidAst extends AstRoot {          key = 'void' as const;           constructor(public type: Type, public location: SourceLocation) { super() } }
export class CastAst extends AstRoot {          key = 'cast' as const;           constructor(public type: Type, public location: SourceLocation, public expr: Ast) { super() } }
export class SubscriptAst extends AstRoot {     key = 'subscript' as const;      constructor(public type: Type, public location: SourceLocation, public left: Ast, public right: Ast) { super() } }
export class SetSubscriptAst extends AstRoot {  key = 'setsubscript' as const;   constructor(public type: Type, public location: SourceLocation, public left: Ast, public right: Ast, public value: Ast) { super() } }
export class NotAst extends AstRoot {           key = 'not' as const;            constructor(public type: Type, public location: SourceLocation, public expr: Ast) { super() } }
export class ConstructorAst extends AstRoot {   key = 'constructor' as const;    constructor(public type: Type, public location: SourceLocation, public args: Ast[]) { super() } }
export class VariantCastAst extends AstRoot   { key = 'variantcast' as const;    constructor(public type: Type, public location: SourceLocation, public enumType: Type, public expr: Ast) { super() } }
export class DefaultConsAst extends AstRoot {   key = 'defaultcons' as const;    constructor(public type: Type, public location: SourceLocation) { super() } }
export class AddressAst extends AstRoot {       key = 'address' as const;        constructor(public type: Type, public location: SourceLocation, public binding: Binding) { super() } }
export class DerefAst extends AstRoot {         key = 'deref' as const;          constructor(public type: Type, public location: SourceLocation, public left: BindingAst, public fieldPath: TypeField[]) { super() } }
export class NamedArgAst extends AstRoot {      key = 'namedarg' as const;       constructor(public type: Type, public location: SourceLocation, public name: string, public expr: Ast) { super() } }
export class SetDerefAst extends AstRoot {      key = 'setderef' as const;       constructor(public type: Type, public location: SourceLocation, public left: BindingAst, public fieldPath: TypeField[], public value: Ast) { super() } }
export class CompTimeObjAst extends AstRoot {   key = 'comptimeobj' as const;    constructor(public type: Type, public location: SourceLocation, public value: unknown) { super() } }
export class InterleaveAst extends AstRoot {    key = 'interleave' as const;     constructor(public type: Type, public location: SourceLocation, public binding: Binding, public entryLabels: Binding[], public elseLabels: Binding[], public entryBlock: Ast, public elseBlock: Ast) { super() } }
export class ContinueInterAst extends AstRoot { key = 'continueinter' as const;  constructor(public type: Type, public location: SourceLocation, public interleaveBinding: Binding, public labelBinding: Binding) { super() } }

export type Ast = NumberAst | LetAst | SetAst | OperatorAst | IfAst | ListAst | CallAst | AndAst | UserCallAst |
  OrAst | StatementsAst | WhileAst | ReturnAst | SetFieldAst | VoidAst | CastAst | SubscriptAst | ConstructorAst |
  BindingAst | StringAst | NotAst | FieldAst | BlockAst | BreakAst | BoolAst | CastAst | DefaultConsAst | ValueFieldAst |
  SetValueFieldAst | SetSubscriptAst | AddressAst | DerefAst | SetDerefAst | CompTimeObjAst | NamedArgAst | InterleaveAst | ContinueInterAst | VariantCastAst
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
  storage: 'ref' | null = null
  constructor(public name: string, public type: Type) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[Binding ${this.name} : ${this.type.shortName}${this.storage ? ` (${this.storage})` : ''}]`, 'special');
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
export const hashValues = (values: unknown[], info={}) => {
  return values.map(value => {
    if (typeof value === 'number') return value
    if (value instanceof PrimitiveType) return `$${value.typeName}`
    if (value instanceof ParameterizedType) return getUniqueId(value)
    if (value instanceof ConcreteClassType) return getUniqueId(value)
    if (value instanceof ClassDefinition) return getUniqueId(value)
    if (value instanceof FunctionDefinition) return getUniqueId(value)
    if (value instanceof Closure) return getUniqueId(value)
    compilerAssert(false, "Cannot hash value", { value, ...info })
  }).join("__")
}

export interface TypeInfo {
  fields: TypeField[]
  metaobject: UnknownObject
  isReferenceType: boolean,
  sizeof: number
  variantPadding?: number
}
export class TypeRoot {}
export class PrimitiveType extends TypeRoot {
  constructor(public typeName: string, public typeInfo: TypeInfo) { super() }
  get shortName() { return this.typeName }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[PrimitiveType ${this.typeName}]`, 'special');
  }
}

export class ConcreteClassType extends TypeRoot {
  constructor(public compiledClass: CompiledClass, public typeInfo: TypeInfo) { super() }
  get shortName() { return this.compiledClass.debugName }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[ConcreteClassType ${this.compiledClass.debugName}]`, 'special');
  }
}
export class ParameterizedType extends TypeRoot {
  constructor(public typeConstructor: TypeConstructor, public args: unknown[], public typeInfo: TypeInfo) { super() }
  get shortName(): string {
    const args = this.args.map(x => isType(x) ? x.shortName : '?').join(', ')
    return `${this.typeConstructor.shortName}!(${args})`
  }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[ParameterizedType ${this.shortName}]`, 'special');
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
  constructor(public typeName: string, public createType: (globalCompiler: GlobalCompilerState, argTypes: Type[]) => Task<ParameterizedType, CompilerError>) { }
  get shortName() { return this.typeName }
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[ExternalTypeConstructor ${this.typeName}]`, 'special');
  }
}

export type Type = PrimitiveType | ConcreteClassType | ParameterizedType
export type TypeConstructor = ExternalTypeConstructor | ClassDefinition // Type constructor for already-compiled types
export const isType = (type: unknown): type is Type => !!type && type instanceof TypeRoot

export class Closure {
  constructor(public func: FunctionDefinition, public scope: Scope, public lexicalParent: SubCompilerState) {}

  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1) return options.stylize(`[${this.constructor.name} ${this.func.debugName}]`, 'special');
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
  [ScopeParentSymbol]: Scope | undefined
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

export type CompilerFunctionCallContext = {
  compilerState: SubCompilerState
  location: SourceLocation,
  typeCheckResult: TypeCheckResult | undefined // Added later
  resultAst: Ast | undefined // Added later
}

// Used in operators and function calls
export type TypeCheckVar = { type: Type }
export type TypeCheckConfig = { a: TypeCheckVar, b: TypeCheckVar, inferType: Type | null }

export type TypeCheckResult = {
  concreteTypes: Type[]
  substitutions: UnknownObject
  returnType: Type
  sortedArgs: Ast[],
  checkFailed: boolean // default false
}

export const isTypeCheckError = (error: CompilerError) => {
  return !!(error.info as any).typeCheckResult
}

export class ExternalFunction {
  constructor(public name: string, public returnType: Type, public func: (ctx: CompilerFunctionCallContext, values: unknown[]) => unknown) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[ExternalFunction ${this.name}]`, 'special');
  }
}

export type CompilerFunctionBuilder = (ctx: CompilerFunctionCallContext, typeArgs: unknown[], args: Ast[]) => Task<Ast, CompilerError>
export class CompilerFunction {
  constructor(public name: string, public func: CompilerFunctionBuilder) {}
  [Inspect.custom](depth: any, options: any, inspect: any) {
    return options.stylize(`[CompilerFunction ${this.name}]`, 'special');
  }
}

export type CompilerCallable = ExternalFunction | CompilerFunction | Closure
export const isCompilerCallable = (value: unknown): value is CompilerCallable => {
  return value instanceof ExternalFunction || value instanceof CompilerFunction || value instanceof Closure
}

export const NeverType =        new PrimitiveType("never",         { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false })
export const VoidType =         new PrimitiveType("void",          { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false })
export const IntType =          new PrimitiveType("int",           { sizeof: 4, fields: [], metaobject: Object.assign(Object.create(null), { init: 0, min: -Math.pow(2, 31), max: Math.pow(2, 31) - 1 }), isReferenceType: false })
export const u64Type =          new PrimitiveType("u64",           { sizeof: 4, fields: [], metaobject: Object.assign(Object.create(null), { init: 0, min: -Math.pow(2, 63), max: Math.pow(2, 63) - 1 }), isReferenceType: false })
export const u8Type =           new PrimitiveType("u8",            { sizeof: 1, fields: [], metaobject: Object.assign(Object.create(null), { init: 0, min: 0, max: 255 }), isReferenceType: false })
export const IntLiteralType =   new PrimitiveType("int_literal",   { sizeof: 4, fields: [], metaobject: Object.create(null), isReferenceType: false })
export const FloatLiteralType = new PrimitiveType("float_literal", { sizeof: 4, fields: [], metaobject: Object.create(null), isReferenceType: false })
export const BoolType =         new PrimitiveType("bool",          { sizeof: 1, fields: [], metaobject: Object.assign(Object.create(null), { init: false, min: 0, max: 1 }), isReferenceType: false })
export const FloatType =        new PrimitiveType("float",         { sizeof: 4, fields: [], metaobject: Object.assign(Object.create(null), { init: 0.0, min: -Math.pow(2, 31), max: Math.pow(2, 31) - 1 }), isReferenceType: false })
export const DoubleType =       new PrimitiveType("double",        { sizeof: 8, fields: [], metaobject: Object.assign(Object.create(null), { init: 0.0, min: -Math.pow(2, 63), max: Math.pow(2, 63) - 1 }), isReferenceType: false })
export const FunctionType =     new PrimitiveType("function",      { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false })
export const RawPointerType =   new PrimitiveType("rawptr",        { sizeof: 8, fields: [], metaobject: Object.create(null), isReferenceType: false })
export const AstType =          new PrimitiveType("ast",           { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false })
export const CompileTimeObjectType = new PrimitiveType("ctobj",    { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false })

export const StringType = (() => {
  const type = new PrimitiveType("string", { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false })
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "length", type, 0, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "data", type, 1, RawPointerType))
  return type;
})()

export const ListTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("List", (compiler, argTypes) => {
  compilerAssert(argTypes.length === 1, "Expected one type arg", { argTypes })
  const type = new ParameterizedType(ListTypeConstructor, argTypes, { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false });
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "length", type, 0, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "capacity", type, 1, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "data", type, 2, RawPointerType))
  return Task.of(type);
})


export const NoneTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("None", (compiler, argTypes) => {
  compilerAssert(argTypes.length === 1, "Expected one type arg", { argTypes })
  const sizeof = argTypes[0].typeInfo.sizeof + IntType.typeInfo.sizeof
  const variantPadding = argTypes[0].typeInfo.sizeof
  const type = new ParameterizedType(NoneTypeConstructor, argTypes, { sizeof, variantPadding, fields: [], metaobject: Object.create(null), isReferenceType: false });
  type.typeInfo.metaobject.isEnumVariant = true
  type.typeInfo.metaobject.enumConstructorVariantOf = OptionTypeConstructor
  type.typeInfo.metaobject.enumVariantIndex = 0
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "tag", type, 0, IntType))
  return Task.of(type)
})

export const SomeTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("Some", (compiler, argTypes) => {
  compilerAssert(argTypes.length === 1, "Expected one type arg", { argTypes })
  const sizeof = argTypes[0].typeInfo.sizeof + IntType.typeInfo.sizeof
  const type = new ParameterizedType(SomeTypeConstructor, argTypes, { sizeof, fields: [], metaobject: Object.create(null), isReferenceType: false });
  type.typeInfo.metaobject.isEnumVariant = true
  type.typeInfo.metaobject.enumConstructorVariantOf = OptionTypeConstructor
  type.typeInfo.metaobject.enumVariantIndex = 1
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "tag", type, 0, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "value", type, 1, argTypes[0]))
  return Task.of(type)
})

export const OptionTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("Option", (compiler, argTypes) => {
  compilerAssert(argTypes.length === 1, "Expected one type arg", { argTypes })

  const sizeof = argTypes[0].typeInfo.sizeof + IntType.typeInfo.sizeof
  const variantPadding = argTypes[0].typeInfo.sizeof
  const opttype = new ParameterizedType(OptionTypeConstructor, argTypes, { sizeof, variantPadding, fields: [], metaobject: Object.create(null), isReferenceType: false });

  opttype.typeInfo.metaobject.isEnum = true
  opttype.typeInfo.fields.push(new TypeField(SourceLocation.anon, "tag", opttype, 0, IntType))

  return (
    createParameterizedExternalType(compiler, SomeTypeConstructor, argTypes)
    .chainFn((task, someType) => {
      return (
        createParameterizedExternalType(compiler, NoneTypeConstructor, argTypes)
        .chainFn((task, noneType) => {
          opttype.typeInfo.metaobject.variants = [someType, noneType]
          opttype.typeInfo.metaobject.Some = someType
          opttype.typeInfo.metaobject.None = noneType
          return Task.of(opttype)
        })
      )
    })
  )
})
export const TupleTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("Tuple", (compiler, argTypes) => {
  const type = new ParameterizedType(TupleTypeConstructor, argTypes, { sizeof: 0, fields: [], metaobject: Object.create(null), isReferenceType: false });
  // TODO: Add getter for length
  // type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "length", type, 0, IntType))
  argTypes.forEach((argType, i) => {
    type.typeInfo.fields.push(new TypeField(SourceLocation.anon, `_${i+1}`, type, i, argType))
  })
  return Task.of(type)
})

export const isTypeInteger = (type: Type) => type === IntType || type === u64Type || type === u8Type
export const isTypeFloating = (type: Type) => type === FloatType || type === DoubleType
export const isTypeScalar = (type: Type) => isTypeInteger(type) || isTypeFloating(type)

export const BuiltinTypes = {
  int: IntType,
  float: FloatType,
  double: DoubleType,
  void: VoidType,
  string: StringType,
  bool: BoolType,
  List: ListTypeConstructor,
  Tuple: TupleTypeConstructor,
  rawptr: RawPointerType,
  u64: u64Type,
  u8: u8Type
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
export const typesEqual = (t1: unknown, t2: any): boolean => {
  if (Object.getPrototypeOf(t1) !== Object.getPrototypeOf(t2)) return false;
  if (t1 instanceof ExternalTypeConstructor) return t1 === t2;
  if (!isType(t1)) {
    return hashValues([t1]) === hashValues([t2])
  }
  compilerAssert(t1 && t2, "Unexpected", { t1, t2 })
  if (t1 instanceof PrimitiveType) return t1 == t2;
  if (t1 instanceof ConcreteClassType) return t1.compiledClass == t2.compiledClass;
  if (t1 instanceof ParameterizedType) {
    if (!typesEqual(t1.typeConstructor, t2.typeConstructor)) return false;
    if (t1.args.length !== t2.args.length) return false;
    return t1.args.every((x, i) => typesEqual(x, t2.args[i]))
  }
  return false;
}

export const typeMatcherEquals = (matcher: TypeMatcher, expected: Type, substitutions: UnknownObject) => {
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
      if (substitutions[matcher.name]) return substitutions[matcher.name] === expected;
      substitutions[matcher.name] = expected;
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
  public type: Type | null = null
  public didBreak = false
  // Does a break with an expression occur?
  // If so it needs to pass to BlockAst to compile differently in LLVM
  public breakWithExpr: boolean = false;
  constructor(
    public parent: LabelBlock | null,
    public name: string | null,
    public breakType: BreakType | null,
    public binding: Binding | null) {}
}

export class LoopObject {
  constructor(public continueBlock: LabelBlock, public breakBlock: LabelBlock) {}
}

export const findLabelBlockByType = (labelBlock: LabelBlock | null, breakType: BreakType | null) => {
  // TODO: This should not find blocks that are outside of our current function,
  // whereas findLabelByBinding should
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
  functionDefinitionsByDeclaration: Map<ParserFunctionDecl, FunctionDefinition>,
  classDefinitions: ClassDefinition[],
  moduleLoader: ModuleLoader
  methods: WeakMap<Scope, [TypeConstructor, Closure][]>,
  allWaitingEvents: Event<unknown, unknown>[],
  logger: Logger,
  typeTable: TypeTable,
  globalLets: LetAst[],
  entryFunction: CompiledFunction | undefined,
  initializerFunction: CompiledFunction | undefined,
  initializerFunctionBinding: Binding,
  mainFunction: CompiledFunction | undefined,
  externalDefinitions: ExternalDefinition[],
  externalCompilerOptions: ExternalCompilerOptions,
  exports: {[key:string]: CompiledFunction},
  rootScope: Scope
}
export type ExternalCompilerOptions = {
  buildName: string
  compilationUnits: string[],
  libraries: string[],
  macosFrameworks: string[],
  globalOptions: GlobalExternalCompilerOptions,
  llPath: string,
  assemblyPath: string,
  nativePath: string
}

// Options that can be shared between multiple compilations
export type GlobalExternalCompilerOptions = {
  libraryDirs: string[]
  outputDir: string
  llcPath: string
  clangPath: string
  importPaths: string[]
}
export type ExternalDefinition = {
  name: string, // Should be unique
  binding: Binding,
  paramHash: string
  paramTypes: Type[]
  returnType: Type
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
    functionDefinitionsByDeclaration: new Map(),
    classDefinitions: [],
    allWaitingEvents: [],
    globalLets: [],
    moduleLoader: null!,
    methods: new WeakMap(),
    typeTable: new TypeTable(),
    logger: null!,
    entryFunction: undefined, // Inserted later
    initializerFunction: undefined, // Inserted later
    initializerFunctionBinding: new Binding(`radius_initializer`, FunctionType),
    mainFunction: undefined, // Inserted later
    externalDefinitions: [],
    exports: {},
    rootScope: null!,
    externalCompilerOptions: {
      globalOptions: {
        libraryDirs: [],
        llcPath: '',
        outputDir: '',
        clangPath: '',
        importPaths: []
      },
      buildName: "", 
      compilationUnits: [], 
      libraries: [],
      macosFrameworks: [],
      assemblyPath: '',
      llPath: '',
      nativePath: ''
    },
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
  moduleCompiler: SubCompilerState
  functionReturnBreakBlock: LabelBlock | undefined

  [Inspect.custom](depth: any, options: any, inspect: any) {
    if (depth <= 1 || true) return options.stylize(`[CompilerState ${this.debugName}]`, 'special');
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
  state.moduleCompiler = ctx.subCompilerState.moduleCompiler
  ctx.subCompilerState = state;
  return state;
}

export function bytecodeToString(bytecodeProgram: BytecodeProgram) {
  const { locations, code } = bytecodeProgram
  const instr = (instr: BytecodeInstr) => {
    const { type, ...args } = instr;
    const values = Object.entries(args)
      .map(([k, v]) => `${k}: ${typeof v === 'function' ? '<function>' : typeof v === 'string' ? v : Inspect(v, { depth: 1})}`)
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
      const lineGutter = `${String(line).padStart(2)}|  `
      out += textColors.gray(lineGutter)
      out += `${lines[line - 1]}\n`
      if (i === 0) {
        const repeat = " ".repeat(location.column + lineGutter.length);
        out += textColors.red(`${repeat}^-- here\n`)
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
export type AstWriterTable<Writer> = {
  [A in Ast as A['key']]: (writer: Writer, ast: A) => void;
}
export type CodegenFunctionWriter = {
  writer: CodegenWriter
  argSlots: number
  returnSlots: number
  bytecode: number[]
  constantsByType: Map<Type, Map<unknown, number>>
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
  globals: Map<Binding, number>
  nextGlobalSlot: number
}


export type Pointer = Binding & {_type: 'pointer'}
export type Register = Binding & {_type: 'register'}

export type LlvmFunctionWriter = {
  writer: LlvmWriter
  function: CompiledFunction,
  // argSlots: number
  // returnSlots: number
  // bytecode: number[]
  constantsByType: Map<Type, Map<unknown, number>>
  // constantSlots: number[]
  // nextConstantSlot: number
  // locals: { binding: Binding, slot: number, scopeIndex: number }[]
  blocks: { 
    binding: Binding, 
    breakExprBinding: Binding | null,

    // We have to do some bookkeeping here to handle
    // interleave blocks since we do codgen in a single
    // pass. Maybe I'd like to add another pass to neaten
    // this up
    interleave?: { 
      jumpPointer: Pointer,
      returnPointer: Pointer,
      interleaveCurrentLabels: Binding[]
      interleaveLabels: Binding[]
      unreachable: Binding
    }
  }[],
  // currentScopeIndex: number
  // nextLocalSlot: number,
  currentBlockLabel: Binding,
  currentOutput: string[],
  outputFunctionBody: string[]
  outputFunctionHeaders: string[],
  printNextStatement: boolean
}
export type LlvmWriter = {
  functions: LlvmFunctionWriter[]
  globalCompilerState: GlobalCompilerState
  functionToIndex: Map<Binding, number>
  typeSizes: Map<Type, number>
  globals: Map<Binding, number>
  globalNames: Map<Binding | Type, string>
  globalNameToBinding: Map<string, Binding | Type>
  nextGlobalSlot: number,
  outputWriter: FileWriter,
  outputStrings: string[],
  outputHeaders: string[],
  mallocBinding: Binding,

  astVisitMap: Map<Ast, boolean>,

  writer: LlvmWriter // weirdness for formatting
  currentOutput: string[]
}

export class BuildObject {
  constructor(
    public moduleName: string,
    public inputPath: string,
    public globalOptions: GlobalExternalCompilerOptions,
    public globalCompiler: GlobalCompilerState,
    public input: string,
    public debugOutputPath: string,
    public gotError = false
  ) {}
}