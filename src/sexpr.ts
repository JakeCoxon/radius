
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
  const parseArgs = () => {
    expect('[')
    const args: [string, IrAst][] = []
    while (!match(']')) args.push([parseName(), parseSExpr()])
    return args;
  }
  const parseDefn = () => {
    const id = addFunctionDefinition({
      name: parseName(),
      typeArgs: (expect('['), parseList().exprs),
      args: parseArgs(),
      returnType: parseSExpr(),
      body: parseSExpr()
    });
    return expectTrailingParen(new IrFunction(previous, id))
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
      else if (match('let'))      return expectTrailingParen(new IrLet(previous, parseName(), new IrCompTime(previous, parseSExpr()), parseSExpr()))
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
      else if (match('call'))     return expectTrailingParen(new IrCall(previous, parseName(), parseExprList()))
      else if (match('defn'))     return parseDefn()
      else throw new Error(`Not found ${currentToken.value}`)
    }
    else if (match('['))        return expectTrailingSquare(new IrList(previous, parseExprList()))
    else if (isNumber(token))   return (advance(), new IrNumber(previous))
    else if (isAlpha(token[0])) return (advance(), new IrIdentifier(previous))
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



const input = `
  (statements
    (defn main [] [] int (statements
      (let x int (+ 3 2))
      (letconst foo (+ 32 42))

      (defn bam [] [a int b int] int (statements
        (call print (+ a b))))

      (let c int (call bam 2 1))

      (let f int (meta (+ bar 3)))
      (set f (* 32 2))
      (call print (or (== 3 25) (>= 2 3)))
      (let foo2 int [])
      (let y int (if (== 2 3) 8 9))
      (let z int (meta (call famp)))
      (let z2 int (call famp))
      ))
    (defn famp [] [] int (statements
      (+ 3 2)
      ))
      
      
    )`;