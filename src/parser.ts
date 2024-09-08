import { ParseAnd, ParseNode, ParseBreak, ParseCall, ParseCast, ParseCompTime, ParseContinue, ParseDict, ParseExpand, ParseField, ParseFor, ParseForExpr, ParseIf, ParseLet, ParseLetConst, ParseList, ParseListComp, ParseMeta, ParseNot, ParseNumber, ParseOpEq, ParseOperator, ParseOr, ParseReturn, ParseSet, ParseStatements, ParseString, ParseIdentifier, ParseWhile, ParseWhileExpr, ParserFunctionDecl, Token, compilerAssert, ParsePostCall, ParseSymbol, ParseNote, ParseSlice, ParseSubscript, ParserClassDecl, ParseClass, ParseFunction, createToken, ParseBoolean, ParseElse, ParseMetaIf, ParseMetaFor, ParseBlock, ParseImport, ParsedModule, Source, ParseMetaWhile, ParseTuple, ParseImportName, ParseFold, ParserFunctionParameter, ParseNamedArg, ParseIs, ParseOrElse, ParseIterator, ParseQuestion, ParseExtract, ParseMatch, ParseMatchCase, ParseGuard, createAnonymousToken, ParseLetAs } from "./defs";

const regexes = {
  KEYWORD:
    /^(?:and|as\!|as\?|as|break|class|continue|comptime|const|def|defn|elif|else|fn|for|guard|if|ifx|in|is|iter|lambda|let|match|meta|null|or|orelse|pass|return|try|while|with|type|interface|import|block|fold|ref|var)(?=\W)/, // note (?=\W)
  IDENTIFIER: /^[a-zA-Z_][a-zA-Z_0-9]*/,
  STRING: /^(?:"(?:[^"\\]|\\.)*")/,
  SPECIALNUMBER: /^0o[0-7]+|^0x[0-9a-fA-F_]+|^0b[01_]+/,
  NUMBER: /^-?(0|[1-9][0-9_]*)(\.[0-9_]+)?(?:[eE][+-]?[0-9]+)?/,
  COMMENT: /^#[^\n]+/,
  OPENPAREN: /^(?:[\[\{\(]|%{)/,
  CLOSEPAREN: /^[\]\}\)]/,
  PUNCTUATION: /^(?:==|!=|:=|<=|>=|\+=|\-=|\*=|\/=|::|->|\|\>|\.\.\.|@@|[@!:,=<>\-+\.*\/'\|?;])/,
  NEWLINE: /^\n/,
  WHITESPACE: /^[ ]+/ // Not newline
}

const tokenize = (source: Source) => {
  let remain = source.input
  const tokens: Token[] = []
  source.tokens = tokens

  let lineNumber = 1
  let lineStart = 0
  let charIndex = 0
  let prevCharIndex = 0

  let match: RegExpExecArray | null
  
  const getToken = () => {
    while (remain.length > 0) {
      const matchType = (() => {
        for (const [type, regex] of Object.entries(regexes)) {
          if ((match = regex.exec(remain))) {
            remain = remain.substring(match[0].length);
            prevCharIndex = charIndex;
            charIndex += match[0].length;
            return type
          }
        }
      })()

      if (!matchType) {
        const char = remain[0]
        const errline = source.input.substring(lineStart, source.input.indexOf("\n", lineStart));
        const repeat = " ".repeat(charIndex - lineStart);
        const message = `Unable to tokenize line ${lineNumber}. Character was '${char}' \n${errline}\n${repeat}^-- here`;
        throw new Error(message);
      }
      const token = createToken(source, match![0], matchType)
      token.location.column = prevCharIndex - lineStart
      token.location.line = lineNumber
      if (matchType === "NEWLINE") { lineNumber++; lineStart = charIndex; }
      else if (matchType === "WHITESPACE" || matchType === "COMMENT") continue
      return token
    }
  }
  return { getToken }

}

const makeAdvancedLexer = (source: Source) => {
  const state = { significantNewlines: true };
  const tokenizer = tokenize(source);
  let tokens: Token[] = [];
  let previous: Token

  let lineSeparate = false
  let hadToken = false
  let endStatement = true
  const indents = [0]
  const parenStack = [] as string[]

  const createTokenWithLocation = (type: string, value: string = "", line: number, column: number) => {
    const token = createToken(source, value, type)
    Object.assign(token.location, { line, column })
    return token
  };

  function *advancedGenerator(): Generator<Token, undefined> {
    while (true) {
      let token = tokenizer.getToken()
      if (!token) break

      if (token.type === "NEWLINE") {
        if (hadToken) lineSeparate = true
        continue
      }

      if (state.significantNewlines && lineSeparate) {
        const numSpaces = token.location.column

        if (numSpaces > indents[indents.length - 1]) {
          indents.push(numSpaces);
          yield createTokenWithLocation("INDENT", '', token.location.line, token.location.column)
          lineSeparate = false
        }
        while (numSpaces < indents[indents.length - 1]) {
          indents.pop();
          yield createTokenWithLocation("ENDSTMT", "", token.location.line, token.location.column)
          yield createTokenWithLocation("DEDENT", '', token.location.line, token.location.column)
        }
      }

      if (state.significantNewlines && lineSeparate && !endStatement) {
        yield createTokenWithLocation("ENDSTMT", "", token.location.line, token.location.column)
        endStatement = true
      }
      lineSeparate = false
      hadToken = true

      if (token.value === "|") {
        if (parenStack.at(-1) === "|") parenStack.pop()
        else if (previous?.value === "{") {
          parenStack[parenStack.length - 1] = "{|"
          parenStack.push(token.value)
        }
      }
      if (token.type === "OPENPAREN") parenStack.push(token.value)
      else if (token.type === "CLOSEPAREN") parenStack.pop()
      if (token.type === "CLOSEPAREN" && state.significantNewlines && !endStatement) {
        yield createTokenWithLocation("ENDSTMT", "", token.location.line, token.location.column)
        endStatement = true
      }
      const topParen = parenStack.at(-1)
      state.significantNewlines = topParen === undefined || topParen === "{|" || topParen === "{"
      tokens.push(token);
      yield token
      endStatement = false
    }

    while (indents.length > 1) {
      yield createTokenWithLocation("ENDSTMT", "", previous.location.line, previous.location.column)
      yield createTokenWithLocation("DEDENT", "", previous.location.line, previous.location.column)
      indents.pop()
    }
    yield createTokenWithLocation("ENDSTMT", "", previous.location.line, previous.location.column)
  }
  const gen = advancedGenerator()
  const getToken = () => {
    const token = gen.next().value
    // console.log(token)
    previous = token!
    return token
  }
  return { source, tokens, state, getToken }
}

export const tokenString = (token: Token) => token.value;

export const makeParser = (input: string, debugName: string) => {
  let token: Token | undefined;
  let previous: Token = undefined!;
  let prevSignificantNewlines = false;

  const source = new Source(debugName, input)

  const state = { classDecls: [] as ParserClassDecl[], functionDecls: [] as ParserFunctionDecl[], node: null! as ParseNode };

  const lexer = makeAdvancedLexer(source);

  const advance = () => {
    prevSignificantNewlines = lexer.state.significantNewlines;
    previous = token!;
    token = lexer.getToken();
  };

  const throwExpectError = (error: string, info: any = {}) => {
    const num = token?.location.line ?? previous?.location.line;
    const value = token?.value !== undefined ? `'${token?.value}' (${token?.type})` : "EOL";
    const msg = `${error} got ${value} on line ${num}`;
    compilerAssert(false, msg, { lexer, token, previous, location: token?.location, prevLocation: previous?.location, ...info });
  };
  const expect = (expected: string | boolean, error: string) => {
    if (expected === true) return previous;
    if (expected === false) throwExpectError(error);
    if (!(token && tokenString(token) === expected)) throwExpectError(error);
    advance();
    return previous;
  };
  const match = (expected: string) =>
    token && tokenString(token) === expected ? (advance(), true) : false;
  const matchType = (expected: string) =>
    token && token.type === expected ? (advance(), true) : false;
  
  const assertIdentifier = (node: ParseNode): ParseIdentifier => (compilerAssert(node instanceof ParseIdentifier, "Expected identifier", { lexer, token, previous, location: previous?.location, prevLocation: previous?.location }), node);
  const assertLeftSide = (node: ParseNode): ParseIdentifier | ParseTuple => {
    (compilerAssert(node instanceof ParseIdentifier || node instanceof ParseTuple, "Expected identifier or tuple", { lexer, token, previous, location: previous?.location }), node);
    return node
  }

  const parseParens = (tupleToken: Token) => {
    const expr = parseExpr();
    if (!match(",")) { expect(")", `Expected ')' after expression`); return expr; }
    let exprs = [expr, parseExpr()]
    while (match(",")) exprs.push(parseExpr())
    expect(")", `Expected ')' after tuple expression`)
    return new ParseTuple(tupleToken, exprs)
  };
  const parseList = (): ParseNode => {
    const listToken = previous;
    if (match("]")) return new ParseList(listToken, []);

    const list = [parseExpr()];        while (match(",")) list.push(parseExpr());
    const mapping: ParseNode[] = [];   while (match("|")) mapping.push(parseExpr());
    const reduce = match("|>") ? parseExpr() : null;
    expect("]", `Expected ']' after list item`);
    if (!mapping.length && !reduce) return new ParseList(listToken, list);
    return new ParseListComp(listToken, list, mapping, reduce);
  };
  const parseNumberLiteral = () => new ParseNumber(previous);
  const parseArgs = () => {
    if (match(")")) return []

    const parseArg = (): ParseNode => {
      const argToken = token!
      const expr = parseExpr()
      if (match("=")) {
        return new ParseNamedArg(argToken, assertIdentifier(expr), parseExpr())
      }
      return expr
    }
    const args = [parseArg()];
    while (match(",")) args.push(parseArg());
    expect(")", `Expected ')' after argument list`);
    return args;
  };

  const expectIdentifier = () => 
    new ParseIdentifier(expect(matchType("IDENTIFIER"), `Expected identifier`));

  const parseDictPair = (): [ParseNode, ParseNode] => {
    return [expectIdentifier(), (expect("=", "Expected '=' after dict key"), parseExpr())]
  }
  const parseDict = (dictToken: Token): ParseNode => {
    if (match("}")) return new ParseDict(dictToken, [])
    const pairs = [parseDictPair()];
    while (match(",") && !match("}")) pairs.push(parseDictPair());
    expect("}", "Expected '}' after dict value");
    return new ParseDict(dictToken, pairs);
  };

  const parseFold = (foldToken: Token) => {
    expect("(", "Expected '(' after fold")
    const expr = parseExpr()
    expect(")", "Expected ')' after fold expression")
    return new ParseFold(foldToken, expr)
  }

  const parseReturnExpr = (returnToken: Token) => {
    const expr = token?.type === 'ENDSTMT' ? null : parseExpr()
    return new ParseReturn(returnToken, expr)
  }
  const parseSymbol = () => new ParseSymbol(expectIdentifier().token);

  const parseBasicLiteral = () => {
    if (matchType("STRING")) return new ParseString(previous, previous.value.slice(1, -1))
    else if (match("'")) return parseSymbol();
    else if (matchType("NUMBER")) return parseNumberLiteral();
    else if (matchType("SPECIALNUMBER")) return parseNumberLiteral();
    else if (match("true") || match("false")) return new ParseBoolean(previous);
    else return expectIdentifier();
  }
  
  const parseLiteral = (): ParseNode => {
    if (match("("))             return parseParens(previous);
    else if (match("["))        return parseList();
    else if (match("-"))        return new ParseOperator(previous, [parseLiteral()])
    else if (match("@"))        return new ParseNote(previous, parsePostfix()); // TODO: This is broken
    else if (match("%{"))       return parseDict(previous)
    else if (match("{"))        return match("|") ? parseLambda() : parseBraceBlockExpr("{")
    else if (match("block"))    return new ParseBlock(previous, null, token?.value != ':' ? expectIdentifier() : null, parseColonBlockExpr('block'))
    else if (match("ifx"))      return parseIf(previous, true, "if condition")
    else if (match("return"))   return parseReturnExpr(previous);
    else if (match("break"))    return parseBreak(previous)
    else if (match("fold"))     return parseFold(previous)
    else if (match("continue")) return parseContinue(previous)
    else if (prevSignificantNewlines && match("|")) return parseBracelessLambda();
    else return parseBasicLiteral()
  };

  const parseSlice = (isStatic: boolean, left: ParseNode): ParseNode => {
    const sliceToken = previous;
    let l: (ParseNode | null)[] = [];
    if (token?.value === ":") l.push(null);
    else if (match("]")) compilerAssert(false, "Not implemented");
    else {
      l.push(parseExpr());
      if (match(']')) return new ParseSubscript(sliceToken, left, l[0]!, isStatic)
    }

    while (match(":")) {
      l.push(token?.value === "]" ? null : parseExpr());
      if (token?.value === "]") break;
    }
    expect(match("]"), "Expected ']' after subscript");

    return new ParseSlice(sliceToken, left, l[0], l[1], l[2] ?? null, isStatic)
  };
  const parseFunctionTypeArguments = (callToken: Token, left: ParseNode): ParseNode => {

    const parseTypeArgs = () => {
      const typeArgs = [parseExpr()];
      while (match(",")) typeArgs.push(parseExpr());
      expect(")", `Expected ')' after type list`);
      return typeArgs;
    }

    const typeArgs = match("(") ? parseTypeArgs() : matchType("NUMBER") ? [parseNumberLiteral()] : [expectIdentifier()];
    const args = match("(") ? parseArgs() : []
    return new ParseCall(callToken, left, args, typeArgs)
  };

  const parseMatch = (matchToken: Token, subject: ParseNode): ParseNode => {
    let name: ParseIdentifier | null = null;
    if (match("'")) name = expectIdentifier();
    expect("{", "Expected '{' after match expression");
    expect(matchType("INDENT"), "Expected indent after match");
    const parseMatchBlock = () => {
      const token = expect(":", "Expected ':'");
      if (matchType("INDENT")) return trailingStatement(parseMultilineBlock(token));
      else return trailingStatement(parseExpr());
    }
    const parseCase = () => {
      return new ParseMatchCase(previous, parseLeftSideMatch(), 
        match("if") ? parseExpr() : null, parseMatchBlock())
    }
    const cases = [parseCase()];
    while (token?.type !== 'DEDENT') cases.push(parseCase());
    expect(matchType("DEDENT"), "Expected dedent after match cases");
    expect(matchType("ENDSTMT"), "Expected end of statement after match cases"); // Hmm ?
    expect("}", "Expected '}' after match cases");
    return new ParseMatch(matchToken, name, subject, cases);
  }

  const parseFieldAccess = (left: ParseNode) => new ParseField(previous, left, expectIdentifier())
  const parsePostCall = (left: ParseNode) => new ParsePostCall(previous, left, parseLambda())

  const parsePostfix = (): ParseNode => {
    let left = parseLiteral();

    while (true) {
      if (match("("))        left = new ParseCall(previous, left, parseArgs(), []);
      else if (match("?"))   left = new ParseQuestion(previous, left); 
      else if (match("["))   left = parseSlice(false, left);
      else if (match(".")) {
        if (match("[")) left = parseSlice(true, left);
        else if (match("iter")) left = new ParseIterator(previous, left);
        else if (match("orelse")) left = new ParseOrElse(previous, left, parseExpr());
        else if (match("match"))  left = parseMatch(previous, left);
        else left = parseFieldAccess(left)
      }
      else if (match("!"))   left = parseFunctionTypeArguments(previous, left);
      else if (match("{"))   left = match("|") ? parsePostCall(left) : (compilerAssert(false, "Not implemented", { left }) as never)
      else if (prevSignificantNewlines && match("|"))
        left = new ParseCall(previous, left, [parseBracelessLambda()], []);
      else return left;
    }
  };
  
  const parseNot = (): ParseNode => match("!")  ? new ParseNot(previous, parseNot()) : parsePostfix();

  const parseIs = () => {        let left = parseNot(); while (match("is"))                   left = new ParseIs(previous, left, parseNot());        return left; };
  const parseAs = () => {        let left = parseIs();       while (match("as!") || match("as"))   left = new ParseCast(previous, left, parseIs());            return left; };
  const parseFactor = () => {    let left = parseAs();       while (match("*") || match("/"))      left = new ParseOperator(previous, [left, parseAs()]);      return left; };
  const parseSum = () => {       let left = parseFactor();   while (match("+") || match("-"))      left = new ParseOperator(previous, [left, parseFactor()]);  return left; };

  const matchEquality = () => match("<") || match("<=") || match(">") || match(">=") || match("==") || match("!=");
  const parseEquality = () => {  let left = parseSum();      while (matchEquality())               left = new ParseOperator(previous, [left, parseSum()]);     return left; };

  const parseAnd = () => {     let left = parseEquality();   while (match("and"))    left = new ParseAnd(previous, [left, parseEquality()]);  return left; };
  const parseOr = () => {      let left = parseAnd();        while (match("or"))     left = new ParseOr(previous, [left, parseAnd()]);        return left; };

  const parseMeta = () =>  match("meta")  ?  new ParseMeta(previous, parseOr())  :  parseOr();

  const parseAssignExpr = parseMeta; // parseAssignExpr excludes expansion dots

  const parseComprehension = (): ParseNode => {
    let expr = parseMeta();
    if (match("while")) return new ParseWhileExpr(previous, parseComprehension(), expr);
    else if (match("for"))   return new ParseForExpr(previous, parseLeftSideMatch(), expectInExpr(), expr);
    else if (match("if"))    return new ParseIf(previous, true, parseComprehension(), expr, null);
    return expr;
  }
  const parseDots = (): ParseNode => {
    let expr = parseComprehension();
    if (match("...")) return new ParseExpand(previous, expr);
    return expr;
  };
  const parseExpr = parseDots;

  const parseFunctionParam = (): ParserFunctionParameter => {
    const name = expectIdentifier()
    if (match(":")) return { name, storage: match('ref') ? 'ref' : null, type: parseExpr() };
    return { name, storage: null, type: null };
  };
  const parseFunctionParamList = (final: string = ")") => {
    if (match(final)) return [];
    const params = [parseFunctionParam()];
    while (match(",")) params.push(parseFunctionParam());
    expect(final, `Expected '${final}' after arg list`);
    return params
  };
  const parseBraceBlockExpr = (afterMessage: string): ParseBlock => {
    const token = previous;
    if (!matchType("INDENT")) return new ParseBlock(token, 'option', null, new ParseStatements(token, [trailingEndBrace(parseExpressionStatement())]))
    return new ParseBlock(token, 'option', null, trailingEndBrace(trailingStatement(parseMultilineBlock(token))))
  }
  const parseColonBlockExpr = (afterMessage: string): ParseStatements => {
    const token = expect(":", `Expected ':' after ${afterMessage}`);
    if (!matchType("INDENT")) return new ParseStatements(token, [parseExpr()]);
    return parseMultilineBlock(token)
  };
  const parseColonBlock = (afterMessage: string): ParseStatements => {
    const token = expect(":", `Expected ':' after ${afterMessage}`);
    if (!matchType("INDENT")) return new ParseStatements(token, [parseExpr()]);
    return trailingStatement(parseMultilineBlock(token));
  };
  const parseMultilineBlock = (token: Token) => {
    const lines = parseLines();
    expect(matchType("DEDENT"), "Expected dedent");
    return new ParseStatements(token, lines);
  };
  const parseFunctionTypeParameters = () => {
    expect("(", `Expected '(' after '!'`);
    const params = [parseExpr()];
    while (match(",")) params.push(parseExpr());
    expect(")", `Expected ')' after type parameter list`);
    return params;
  };

  const parseOptionalMetaName = () => match("(") ? trailingEndParen(expectIdentifier()) :  null;

  const parseFunctionDef = () => {

    const parseOptionalReturnType = (arrowToken: Token) => {
      const list = [parseExpr()]
      while (match(",")) list.push(parseExpr())
      return list.length > 1 ? new ParseTuple(arrowToken, list) : list[0];
    }
    const parseFunctionParamListParen = () => (expect("(", `Expected '(' after function name`), parseFunctionParamList(")"))

    const parseBody = () => {
      if (matchType("ENDSTMT")) return null;
      expect(":", "Expected ':'");
      const token = previous;
      if (matchType("INDENT")) return trailingStatement(parseMultilineBlock(token));
      else return trailingStatement(parseExpr());
    }

    return createNamedFunc(state, previous, parseOptionalMetaName(), 
      expectIdentifier(), match("!") ? parseFunctionTypeParameters() : [], 
      parseFunctionParamListParen(), match('->') ? parseOptionalReturnType(previous) : null, 
      parseKeywords(), parseBody())
  };
  const parseLambda = () => {
    return trailingEndBrace(trailingStatement(parseBracelessLambda()))
  };
  const parseKeywords = () => {
    const kw: ParseNode[] = []
    while (token?.value === "@") kw.push(parseLiteral());
    return kw;
  }
  const parseOptionalReturnType = () => match(":") ? parseExpr() : null
  const parseSingleOrMultilineBody = () => 
    matchType("INDENT") ? parseMultilineBlock(previous) 
    : new ParseStatements(previous, [parseExpr()])

  const parseBracelessLambda = (): ParseNode => {
    return createAnonymousFunc(state, previous, parseFunctionParamList("|"), 
      parseKeywords(), parseOptionalReturnType(), parseSingleOrMultilineBody())
  };
  const parseOptionalMetaClass = () => match("<") ? expectIdentifier() :  null;
  const parseClassDef = (): ParseNode => {
    return createClassDef(state, previous,  
      expectIdentifier(), match("!") ? parseFunctionTypeParameters() : [], 
      parseOptionalMetaClass(),
      parseKeywords(), parseColonBlock("class definition header"))
  };

  const parseElse = (isExpr: boolean): ParseIf | ParseElse | null => {
    if (match("elif")) return parseIf(previous, isExpr, "elif condition")
    else if (match("else")) return new ParseElse(previous, parseColonBlock("else"))
    return null
  }

  const parseLetOrExpr = () => 
    match("var") ? parseLetExpr(true) : 
    match("let") ? parseLetExpr(false) : parseExpr();

  const parseLetOrExprList = () => {
    const list = [parseLetOrExpr()];
    while (match(";")) list.push(parseLetOrExpr());
    if (list.length === 1) return list[0];
    return new ParseAnd(previous, list);
  }

  const parseIf = (ifToken: Token, isExpr: boolean, message: string = "if condition"): ParseIf =>
    new ParseIf(ifToken, isExpr, parseLetOrExprList(), parseColonBlock(message), parseElse(isExpr));

  const parseGuard  = (guardToken: Token) => {
    const expr = parseLetOrExprList();
    expect("else", "Expected 'else' after guard condition");
    return new ParseGuard(guardToken, expr, parseColonBlock("guard"))
  }

  const parseWhile = () => new ParseWhile(previous, parseExpr(), parseColonBlock('while condition')); // prettier-ignore

  const parseTuple = (expr: ParseNode): ParseTuple => {
    const list = [expr, parseExpr()];
    while (match(",")) list.push(parseExpr());
    return new ParseTuple(previous, list);
  }

  const parseExpressionStatement2 = () => {
    let expr = parseExpr();
    if (match(","))       expr = parseTuple(expr);
    // if (match(":="))      return new ParseLet(previous, assertLeftSide(expr), null, parseAssignExpr());
    // else if (match("::")) return new ParseLetConst(previous, assertIdentifier(expr), parseAssignExpr());
    else if (match("="))  return new ParseSet(previous, expr, parseAssignExpr());
    else if (match("+=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("-=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("*=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("/=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match(":")) {
      const type = parseExpr();
      const value = match("=") ? parseAssignExpr() : null
      return new ParseLet(previous, true, assertLeftSide(expr), type, value)
    }
    return expr;
  };
  const parseExpressionStatement = () => {
    let expr = parseExpressionStatement2();
    if (match("..."))        expr = new ParseExpand(previous, expr);
    else if (match("while")) expr = new ParseWhileExpr(previous, parseExpr(), expr);
    else if (match("for"))   expr = new ParseForExpr(previous, parseLeftSideMatch(), expectInExpr(), expr);
    else if (match("if"))    expr = new ParseIf(previous, false, parseExpr(), expr, null);
    return trailingStatement(expr);
  };

  const parseLetExpr = (mutable: boolean) => {
    const left = parseLeftSideMatch();
    const type = match(":") ? parseExpr() : null;
    const value = match("=") ? parseAssignExpr() : null;
    let let_ = new ParseLet(previous, mutable, left, type, value)
    if (match("as?")) return new ParseLetAs(previous, let_, parseExpr())
    return let_
  }
  const parseLetStatement = (mutable: boolean) => trailingStatement(parseLetExpr(mutable))
  const parseLetConstStatement = () => {
    const left = parseLeftSideMatch();
    expect("=", "Expected '=' after const identifier");
    const value = parseAssignExpr()
    return trailingStatement(new ParseLetConst(previous, left as any, value))
  }
  
  const trailingStatement = <T>(x: T) => (expect(matchType("ENDSTMT"), "Expected end of statement"), x);
  const trailingEndBrace = <T>(x: T) => (expect(match("}"), "Expected '}'"), x);
  const trailingEndParen = <T>(x: T) => (expect(")", "Expected ')'"), x);

  // Expr after in must be lower precedence than expansion dots
  const expectInExpr = () => (expect("in", "Expected 'in' after for identifier"), parseAssignExpr())
  const parseLeftSideMatch = (): ParseNode => {
    

    const expectIden = (node: ParseNode): ParseIdentifier => (compilerAssert(node instanceof ParseIdentifier, "Expected identifier", { lexer, token, previous, location: previous?.location, prevLocation: previous?.location }), node);

    const idenOrExtract = (): ParseNode => {
      if (match("(")) {
        const token = previous
        let args = [idenOrExtract()];
        while (match(",")) args.push(idenOrExtract());
        expect(")", "Expected ')' after tuple expression");
        return new ParseTuple(token, args);
      }
      let left: ParseNode = parseBasicLiteral()
      
      if (!match("(")) return left
      if (match(")")) return new ParseExtract(previous, expectIden(left), [])
      let args = [idenOrExtract()];
      while (match(",")) args.push(idenOrExtract());
      left = new ParseExtract(previous, expectIden(left), args);
      expect(")", "Expected ')' after tuple expression");
      return left
    }

    let left = idenOrExtract();
    if (match(",")) {
      const list = [left, idenOrExtract()];
      while (match(",")) list.push(idenOrExtract());
      left = new ParseTuple(previous, list);
    }
    return left;
  }
  const parseForStatement = () =>
    new ParseFor(previous, parseLeftSideMatch(), expectInExpr(), parseColonBlock("for list-expression"))

  const parseMetaStatement = (metaToken: Token) => {
    if (match("if"))    return new ParseMetaIf(metaToken, parseIf(previous, false));
    if (match("for"))   return new ParseMetaFor(metaToken, parseForStatement());
    if (match("while")) return new ParseMetaWhile(metaToken, parseWhile());
    return new ParseMeta(previous, parseStatement());
  }
  const parseOptionalExpr = () => matchType("ENDSTMT") ? null : trailingStatement(parseExpr())

  const parseImport = (importToken: Token) => {
    const module = expectIdentifier()
    const rename = match("as") ? expectIdentifier() : null
    const idents: ParseImportName[] = []
    const parseImportName = () => {
      const name = expectIdentifier()
      const rename = match("as") ? expectIdentifier() : null
      return new ParseImportName(name.token, name, rename)
    }
    if (match("for")) {
      idents.push(parseImportName())
      while (match(",")) idents.push(parseImportName())
    }
    return trailingStatement(new ParseImport(importToken, module, rename, idents))
  }
  const parseBreak = (breakToken: Token) => {
    if (matchType("ENDSTMT")) return new ParseBreak(breakToken, null, null)
    const iden = expectIdentifier()
    if (!match("with")) return trailingStatement(new ParseBreak(breakToken, iden, null))
    return trailingStatement(new ParseBreak(breakToken, iden, parseExpr()))
  }
  const parseContinue = (continueToken: Token) => {
    if (matchType("ENDSTMT")) return new ParseContinue(continueToken, null)
    const iden = expectIdentifier()
    return trailingStatement(new ParseContinue(continueToken, iden))
  }
  const parseAnnotation = () => {
    const annotation = parseExpr()
    expect(matchType("ENDSTMT"), "Expected end of statement after annotation")
    const stmt = parseStatement()
    compilerAssert(stmt instanceof ParseFunction, "Not implemented for statement type")
    stmt.functionDecl.annotations.push(annotation)
    return stmt
  }

  const parseStatement = (): ParseNode => {
    if (match("fn"))            return parseFunctionDef();
    else if (match("@@"))       return parseAnnotation()
    else if (match("type"))     return parseClassDef();
    else if (match("let"))      return parseLetStatement(false);
    else if (match("var"))      return parseLetStatement(true);
    else if (match("const"))    return parseLetConstStatement();
    else if (match("if"))       return parseIf(previous, false);
    else if (match("guard"))    return parseGuard(previous);
    else if (match("while"))    return parseWhile();
    else if (match("comptime")) return new ParseCompTime(previous, parseColonBlock("comptime"));
    else if (match("return"))   return new ParseReturn(previous, parseOptionalExpr());
    else if (match("break"))    return parseBreak(previous);
    else if (match("continue")) return parseContinue(previous);
    else if (match("for"))      return parseForStatement();
    else if (match("meta"))     return parseMetaStatement(previous)
    else if (match("import"))   return parseImport(previous)
    return parseExpressionStatement();
  };
  const parseLines = () => {
    const stmts: ParseNode[] = [];

    // while (matchType("NEWLINE"));
    stmts.push(parseStatement());
    while (token && token.type !== "DEDENT") stmts.push(parseStatement());
    return stmts;
  };
  
  advance();
  const rootNode = new ParseStatements(createToken(null!, ""), parseLines());

  const msg = `Expected EOF but got ${previous?.value} (${previous?.type})`;
  compilerAssert(token === undefined, msg, { lexer, token: previous });
  return { rootNode, classDefs: state.classDecls, functionDecls: state.functionDecls } satisfies ParsedModule
};

const createNamedFunc = (state: any, token: Token, functionMetaName: ParseIdentifier | null, name: ParseIdentifier, typeParams: ParseNode[], params: ParserFunctionParameter[], returnType: ParseNode | null, keywords: ParseNode[], body: ParseNode | null) => {
  const decl: ParserFunctionDecl = {
    debugName: `${name.token.value}`,
    token: token, functionMetaName, name, typeParams, params,
    keywords, returnType, body, annotations: [], variadic: false
  };
  state.functionDecls.push(decl)
  return new ParseFunction(token, decl)
}

const createAnonymousFunc = (state: any, token: Token, params: ParserFunctionParameter[], keywords: ParseNode[], returnType: ParseNode | null, body: ParseStatements) => {
  const decl: ParserFunctionDecl = {
    debugName: `<anonymous line ${token.location.line}>`,
    token: token, functionMetaName: null, name: null, typeParams: [], params,
    keywords, anonymous: true, returnType, body, annotations: [], variadic: false
  };
  state.functionDecls.push(decl)
  return new ParseFunction(token, decl)
}

const createClassDef = (state: any, token: Token, name: ParseIdentifier, typeArgs: ParseNode[], metaType: ParseIdentifier | null, keywords: ParseNode[], body: ParseNode | null) => {
  const decl: ParserClassDecl = {
    id: undefined, token,
    debugName: `${name.token.value}`,
    metaType, name, typeArgs, keywords, body
  };
  state.classDecls.push(decl)
  return new ParseClass(token, decl)
}