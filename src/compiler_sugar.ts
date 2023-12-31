import { BytecodeSecondOrder, getOperatorTable, visitParseNode } from "./compiler"
import { insertFunctionDefinition } from "./compiler_functions"
import { ArgumentTypePair, Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden } from "./defs"

export const forLoopSugar = (out: BytecodeWriter, node: ParseFor) => {
  const fnArgs: ArgumentTypePair[] = [[node.identifier, null]]
  const continueBlock = new ParseBlock(node.token, 'continue', null, new ParseStatements(node.token, [node.body]))
  const decl = createAnonymousParserFunctionDecl("for", node.token, fnArgs, continueBlock)
  const fn = new ParseFunction(node.token, decl)
  const iterateFn = new ParseCompilerIden(createAnonymousToken(''), 'iteratefn');
  const call = new ParseCall(node.token, iterateFn, [node.expr], [fn])
  const breakBlock = new ParseBlock(node.token, 'break', null, new ParseStatements(node.token, [call]))
  visitParseNode(out, breakBlock)
}

const rangeLoop = (token: Token, iden: ParseIdentifier | ParseFreshIden, max: ParseNode, body: ParseNode) => {
  const letNode = new ParseLet(token, iden, null, new ParseNumber(createAnonymousToken('0')))
  const loopBody = new ParseStatements(token, [body, new ParseOpEq(createAnonymousToken("+="), iden, new ParseNumber(createAnonymousToken('1')))])
  const loop = new ParseWhile(token, new ParseOperator(createAnonymousToken('<'), [iden, max]), loopBody)
  return [letNode, loop]
}
const getLength = (token: Token, expr: ParseNode) => new ParseCall(token, new ParseCompilerIden(createAnonymousToken(''), 'lenfn'), [expr], [])

const minAll = (token: Token, letIden: ParseIdentifier | ParseFreshIden, exprs: ParseNode[]) => {
  // Iteratively compute the minimum of a list of expressions
  const letMin = new ParseLet(token, letIden, null, exprs[0])
  const mins = exprs.flatMap((expr, i) => {
    const tmpIden = new ParseFreshIden(token, new FreshBindingToken("min"))
    const letExpr = new ParseLet(token, tmpIden, null, expr)
    const less = new ParseOperator(createAnonymousToken('<'), [letIden, tmpIden])
    const min = new ParseIf(token, true, less, letIden, new ParseElse(token, tmpIden))
    const set = new ParseSet(token, letIden, min)
    return [letExpr, set]
  })
  return [letMin, ...mins]
}

export const expandLoopSugar = (out: BytecodeWriter, node: ParseExpand) => {

  // Because we want to use the expansion state to insert a loop construct
  // around the expansion body we need to evaluate the body first.
  // Visit the body of the expansion node but output to a fresh bytecode
  // array which we will append later using a ParseBytecode.
  // This is in combination with sliceSugar below
  // TODO: use fresh variables

  const bytecode = { code: [], locations: [] }
  const indexIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('i'))
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  out.state.expansion = { selectors: [], indexIdentifier, iteratorListIdentifier }
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.expr)

  const getIterator = (i: number) => new ParseMeta(node.token, new ParseSubscript(node.token, 
    iteratorListIdentifier, new ParseNumber(createAnonymousToken(i)), false))
  const lengths = out.state.expansion.selectors.map((s, i) => getLength(node.token, getIterator(i)))

  const list = new ParseList(node.token, out.state.expansion.selectors.map(x => new ParseQuote(node.token, x.node)))
  const letIteratorList = new ParseLet(node.token, iteratorListIdentifier, null, list)
  const maxIden = new ParseFreshIden(node.token, new FreshBindingToken('max'))
  const letMax2 = minAll(node.token, maxIden, lengths)
  const loop = rangeLoop(node.token, indexIdentifier, maxIden, new ParseBytecode(node.token, bytecode))

  visitParseNode(out, new ParseStatements(node.token, [new ParseMeta(node.token, letIteratorList), ...letMax2, ...loop]))

  out.state.expansion = null
}

export const sliceSugar = (out: BytecodeWriter, node: ParseSlice) => {
  compilerAssert(node.a === null, "Not implemented", { node })
  compilerAssert(node.b === null, "Not implemented", { node })
  compilerAssert(node.c === null, "Not implemented", { node })
  compilerAssert(out.state.expansion, "Expected expansion locus for slice operator")
  const index = out.state.expansion.selectors.length
  out.state.expansion.selectors.push({ node: node.expr })
  const indexNode = new ParseNumber(createAnonymousToken(index))
  const iteratorList = new ParseMeta(node.token, new ParseSubscript(node.token, out.state.expansion.iteratorListIdentifier, indexNode, false))
  const subscriptIterator = new ParseSubscript(node.token, iteratorList, out.state.expansion.indexIdentifier, false);
  visitParseNode(out, subscriptIterator)
}

export const listComprehensionSugar = (out: BytecodeWriter, node: ParseListComp) => {
  let list: ParseNode = new ParseList(node.token, node.exprs)
  if (node.exprs.length === 1 && node.exprs[0] instanceof ParseExpand) {
    list = node.exprs[0].expr
  }
  const trx = node.mapping[0]
  const reducer = node.reduce!
  // TODO: Do this without looking up transduce in scope?
  const call = new ParseCall(node.token, new ParseIdentifier(createAnonymousToken('transduce')), [list], [trx, reducer])
  visitParseNode(out, call)
}

const insertMetaObjectPairwiseOperator = (compiledClass: CompiledClass, operatorName: string, operatorSymbol: string) => {
  const operatorFunc = new ExternalFunction(operatorName, VoidType, (location: SourceLocation, a: Ast, b: Ast) => {
    if (b.type instanceof ParameterizedType && b.type.typeConstructor === TupleTypeConstructor) {
      compilerAssert(b.type.args.length === compiledClass.fields.length, `Expected tuple of size ${compiledClass.fields.length}, got ${b.type.args.length}`, { type: b.type })
      const constructorArgs = compiledClass.fields.map((field, i) => {
        const otherField = b.type.typeInfo.fields.find(x => x.name === `_${i+1}`)
        compilerAssert(otherField?.fieldType === field.fieldType, `Expected type of tuple field ${i+1} to be $fieldType got $otherFieldType`, { fieldType: field.fieldType, otherFieldType: otherField?.fieldType })
        return getOperatorTable()[operatorSymbol].func(location, 
          new FieldAst(field.fieldType, location, a, field),
          new FieldAst(otherField.fieldType, location, b, otherField))
      })
      return new ConstructorAst(compiledClass.type, location, constructorArgs)
    }
    compilerAssert(b.type === a.type, "Expected vec or tuple. got $type", { type: b.type })
    const constructorArgs = compiledClass.fields.map(field => 
      getOperatorTable()[operatorSymbol].func(location, 
        new FieldAst(field.fieldType, location, a, field),
        new FieldAst(field.fieldType, location, b, field))
    )
    return new ConstructorAst(compiledClass.type, location, constructorArgs)
  })
  compiledClass.metaobject[operatorName] = operatorFunc
}

export const VecTypeMetaClass = new ExternalFunction('VecType', VoidType, (compiledClass: CompiledClass) => {
  insertMetaObjectPairwiseOperator(compiledClass, "add", "+")
  insertMetaObjectPairwiseOperator(compiledClass, "sub", "-")
  insertMetaObjectPairwiseOperator(compiledClass, "mul", "*")
  insertMetaObjectPairwiseOperator(compiledClass, "div", "/")
})

export const defaultMetaFunction = (subCompilerState: SubCompilerState, compiledClass: CompiledClass, definitionScope: Scope, templateScope: Scope) => {
  const iterate = templateScope['__iterate']
  compilerAssert(!iterate || iterate instanceof Closure)

  const fnArgs: ArgumentTypePair[] = compiledClass.fields.map(x => 
    [new ParseIdentifier(createAnonymousToken(x.name)), 
    new ParseValue(createAnonymousToken(''), x.fieldType)] as ArgumentTypePair)
  const constructorBody = new ParseConstructor(
    createAnonymousToken(''), 
    new ParseValue(createAnonymousToken(''), compiledClass.type), 
    compiledClass.fields.map(x => new ParseIdentifier(createAnonymousToken(x.name))))
  const decl = createAnonymousParserFunctionDecl("constructor", createAnonymousToken(''), fnArgs, constructorBody)
  const funcDef = insertFunctionDefinition(subCompilerState.globalCompiler, decl)
  const constructor = new Closure(funcDef, definitionScope, subCompilerState.lexicalParent!)

  Object.assign(compiledClass.metaobject, { iterate, constructor })
}