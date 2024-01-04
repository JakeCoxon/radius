import { BytecodeSecondOrder, getOperatorTable, visitParseNode } from "./compiler"
import { insertFunctionDefinition } from "./compiler_functions"
import { ArgumentTypePair, Ast, BytecodeWriter, Closure, CompiledClass, ConstructorAst, ExternalFunction, FieldAst, FreshBindingToken, ParameterizedType, ParseBlock, ParseBytecode, ParseCall, ParseCompilerIden, ParseConstructor, ParseElse, ParseExpand, ParseFor, ParseFunction, ParseIdentifier, ParseIf, ParseLet, ParseList, ParseListComp, ParseMeta, ParseNode, ParseNumber, ParseOpEq, ParseOperator, ParseQuote, ParseSet, ParseSlice, ParseStatements, ParseSubscript, ParseValue, ParseWhile, Scope, SourceLocation, SubCompilerState, Token, TupleTypeConstructor, VoidType, compilerAssert, createAnonymousParserFunctionDecl, createAnonymousToken, ParseFreshIden, ParseAnd } from "./defs"

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

const rangeLoop = (token: Token, iden: ParseIdentifier | ParseFreshIden, start: ParseNode, end: ParseNode, body: ParseNode) => {
  const letNode = new ParseLet(token, iden, null, start)
  const loopBody = new ParseStatements(token, [body, new ParseOpEq(createAnonymousToken("+="), iden, new ParseNumber(createAnonymousToken('1')))])
  const loop = new ParseWhile(token, new ParseOperator(createAnonymousToken('<'), [iden, end]), loopBody)
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
  // const indexIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('i'))
  const iteratorListIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('iterator_list'))

  out.state.expansion = { selectors: [], iteratorListIdentifier }
  visitParseNode({ bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.expr)

  const getIterator = (i: number) => new ParseMeta(node.token, new ParseSubscript(node.token, 
    iteratorListIdentifier, new ParseNumber(createAnonymousToken(i)), false))

  const iteratorNodes = out.state.expansion.selectors.map((s, i) => {
    let lengthNode: ParseNode = getLength(node.token, getIterator(i))
    // TODO: Handle positive end positions!
    if (s.end) lengthNode = new ParseOperator(createAnonymousToken("+"), [lengthNode, s.end])
    let letLengthNode: ParseNode = new ParseLet(node.token, new ParseFreshIden(node.token, new FreshBindingToken('length')), null, lengthNode)
    const letItNode = new ParseLet(node.token, s.indexIdentifier, null, s.start ?? new ParseNumber(createAnonymousToken('0')))
    const incNode = new ParseOpEq(createAnonymousToken("+="), letItNode.name, s.step ?? new ParseNumber(createAnonymousToken('1')))
    const condNode = new ParseOperator(createAnonymousToken("<"), [letItNode.name, letLengthNode.name])
    return { letLengthNode, condNode, letItNode, incNode }
  })

  const list = new ParseList(node.token, out.state.expansion.selectors.map(x => new ParseQuote(node.token, x.node)))
  const letIteratorList = new ParseLet(node.token, iteratorListIdentifier, null, list)
  const lets = iteratorNodes.flatMap(x => [x.letLengthNode, x.letItNode])
  const cond = iteratorNodes.slice(1).reduce((acc: ParseNode, x) => new ParseAnd(node.token, [acc, x.condNode]), iteratorNodes[0].condNode)
  const loop = new ParseWhile(node.token, cond, new ParseStatements(node.token, [
    new ParseBytecode(node.token, bytecode),
    ...iteratorNodes.map(x => x.incNode)]))

  visitParseNode(out, new ParseStatements(node.token, [new ParseMeta(node.token, letIteratorList), 
    ...lets, loop]))

  out.state.expansion = null
}

export const sliceSugar = (out: BytecodeWriter, node: ParseSlice) => {
  compilerAssert(out.state.expansion, "Expected expansion locus for slice operator")
  const index = out.state.expansion.selectors.length
  const indexIdentifier = new ParseFreshIden(node.token, new FreshBindingToken('i'))
  out.state.expansion.selectors.push({ node: node.expr, start: node.a, end: node.b, step: node.c, indexIdentifier })
  const indexNode = new ParseNumber(createAnonymousToken(index))
  const iteratorList = new ParseMeta(node.token, new ParseSubscript(node.token, out.state.expansion.iteratorListIdentifier, indexNode, false))
  const subscriptIterator = new ParseSubscript(node.token, iteratorList, indexIdentifier, false);
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

  if (compiledClass.classDefinition.keywords.includes('struct'))
    compiledClass.type.typeInfo.isReferenceType = false

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