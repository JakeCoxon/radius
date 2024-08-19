import { ParseAnd, ParseNode, ParseBreak, ParseCall, ParseCast, ParseCompTime, ParseContinue, ParseDict, ParseExpand, ParseField, ParseFor, ParseForExpr, ParseIf, ParseLet, ParseLetConst, ParseList, ParseListComp, ParseMeta, ParseNot, ParseNumber, ParseOpEq, ParseOperator, ParseOr, ParseReturn, ParseSet, ParseStatements, ParseString, ParseIdentifier, ParseWhile, ParseWhileExpr, ParserFunctionDecl, Token, compilerAssert, ParsePostCall, ParseSymbol, ParseNote, ParseSlice, ParseSubscript, ParserClassDecl, ParseClass, ParseFunction, createToken, ParseBoolean, ParseElse, ParseMetaIf, ParseMetaFor, ParseBlock, ParseImport, ParsedModule, Source, ParseMetaWhile, ParseTuple, ParseImportName, ParseFold, ParserFunctionParameter, ParseNamedArg } from "./defs";

const regexes = {
  KEYWORD:
    /^(?:and|as\!|as|break|class|continue|comptime|def|defn|elif|else|fn|for|if|ifx|in|lambda|meta|null|or|pass|return|try|while|with|type|interface|import|block|fold|ref)(?=\W)/, // note (?=\W)
  IDENTIFIER: /^[a-zA-Z_][a-zA-Z_0-9]*/,
  STRING: /^(?:"(?:[^"\\]|\\.)*")/,
  SPECIALNUMBER: /^0o[0-7]+|^0x[0-9a-fA-F_]+|^0b[01_]+/,
  NUMBER: /^(0|[1-9][0-9_]*)(\.[0-9_]+)?(?:[eE][+-]?[0-9]+)?/,
  COMMENT: /^#[^\n]+/,
  OPENPAREN: /^(?:[\[\{\(]|%{)/,
  CLOSEPAREN: /^[\]\}\)]/,
  PUNCTUATION: /^(?:==|!=|:=|<=|>=|\+=|\-=|\*=|\/=|::|->|\|\>|\.\.\.|@@|[@!:,=<>\-+\.*\/'\|])/,
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

  let lineStart = true
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
        if (state.significantNewlines) yield token
        lineStart = true
        continue
      }

      if (state.significantNewlines && lineStart) {
        const numSpaces = token.location.column

        if (numSpaces > indents[indents.length - 1]) {
          indents.push(numSpaces);
          yield createTokenWithLocation("INDENT", '', token.location.line, token.location.column)
        }
        while (numSpaces < indents[indents.length - 1]) {
          indents.pop();
          yield createTokenWithLocation("DEDENT", '', token.location.line, token.location.column)
          yield createTokenWithLocation("NEWLINE", '', token.location.line, token.location.column)
        }
      }

      lineStart = false

      if (token.value === "|") {
        if (parenStack.at(-1) === "|") parenStack.pop()
        else if (previous?.value === "{") {
          parenStack[parenStack.length - 1] = "{|"
          parenStack.push(token.value)
        }
      }
      if (token.type === "OPENPAREN") parenStack.push(token.value)
      else if (token.type === "CLOSEPAREN") parenStack.pop()
      const topParen = parenStack.at(-1)
      state.significantNewlines = topParen === undefined || topParen === "{|"
      tokens.push(token);
      yield token
    }

    while (indents.length > 1) {
      yield createTokenWithLocation("NEWLINE", "", previous.location.line, previous.location.column)
      yield createTokenWithLocation("DEDENT", "", previous.location.line, previous.location.column)
      indents.pop()
    }
    yield createTokenWithLocation("NEWLINE", "", previous.location.line, previous.location.column)
  }
  const gen = advancedGenerator()
  const getToken = () => {
    const token = gen.next().value
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
    if (previous?.type === "NEWLINE") while (token?.type === "NEWLINE") token = lexer.getToken();
  };

  const throwExpectError = (error: string) => {
    const num = token?.location.line ?? previous?.location.line;
    const value = token?.value !== undefined ? `'${token?.value}' (${token?.type})` : "EOL";
    const msg = `${error} got ${value} on line ${num}`;
    compilerAssert(false, msg, { lexer, token, previous, location: token?.location, prevLocation: previous?.location });
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

  const parseIdentifier = () => 
    new ParseIdentifier(expect(matchType("IDENTIFIER"), `Expected identifier`));

  const parseDictPair = (): [ParseNode, ParseNode] => {
    return [parseIdentifier(), (expect("=", "Expected '=' after dict key"), parseExpr())]
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

  const parseLiteral = (): ParseNode => {
    if (match("("))          return parseParens(previous);
    else if (match("["))     return parseList();
    else if (match("-"))     return new ParseOperator(previous, [parseLiteral()])
    else if (match("'"))     return new ParseSymbol(parseIdentifier().token);
    else if (match("@"))     return new ParseNote(previous, parseCall());
    else if (match("%{"))    return parseDict(previous)
    else if (match("{"))     return match("|") ? parseLambda() : throwExpectError("Not implemented")
    else if (match("block")) return new ParseBlock(previous, null, token?.value != ':' ? parseIdentifier() : null, parseColonBlockExpr('block'))
    else if (match("ifx"))   return parseIf(previous, true, "if condition")
    else if (matchType("STRING")) return new ParseString(previous, previous.value.slice(1, -1))
    else if (matchType("NUMBER")) return parseNumberLiteral();
    else if (matchType("SPECIALNUMBER")) return parseNumberLiteral();
    else if (prevSignificantNewlines && match("|")) return parseBracelessLambda();
    else if (match("true") || match("false")) return new ParseBoolean(previous);
    else if (match("fold"))  return parseFold(previous)
    else return parseIdentifier();
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

    const typeArgs = match("(") ? parseTypeArgs() : matchType("NUMBER") ? [parseNumberLiteral()] : [parseIdentifier()];
    const args = match("(") ? parseArgs() : []
    return new ParseCall(callToken, left, args, typeArgs)
  };

  const parseFieldAccess = (left: ParseNode) => new ParseField(previous, left, parseIdentifier())
  const parsePostCall = (left: ParseNode) => new ParsePostCall(previous, left, parseLambda())

  const parseCall = (): ParseNode => {
    let left = parseLiteral();

    while (true) {
      if (match("("))        left = new ParseCall(previous, left, parseArgs(), []);
      else if (match("["))   left = parseSlice(false, left);
      else if (match("."))   left = match("[") ? parseSlice(true, left) : parseFieldAccess(left)
      else if (match("!"))   left = parseFunctionTypeArguments(previous, left);
      else if (match("{"))   left = match("|") ? parsePostCall(left) : (compilerAssert(false, "Not implemented") as never)
      else if (prevSignificantNewlines && match("|"))
        left = new ParseCall(previous, left, [parseBracelessLambda()], []);
      else return left;
    }
  };
  
  const parseNot = (): ParseNode => match("!")  ? new ParseNot(previous, parseNot()) : parseCall();

  const parseAs = () => {        let left = parseNot();      while (match("as!") || match("as"))   left = new ParseCast(previous, left, parseNot());          return left; };
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
    const name = parseIdentifier()
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
  const parseColonBlockExpr = (afterMessage: string): ParseStatements => {
    expect(":", `Expected ':' after ${afterMessage}`);
    const token = previous;
    if (!matchType("NEWLINE")) return new ParseStatements(token, [parseExpr()]);
    return parseMultilineBlock(token)
  };
  const parseColonBlock = (afterMessage: string): ParseStatements => {
    expect(":", `Expected ':' after ${afterMessage}`);
    const token = previous;
    if (!matchType("NEWLINE")) return new ParseStatements(token, [parseExpr()]);
    return trailingNewline(parseMultilineBlock(token));
  };
  const parseMultilineBlock = (token: Token) => {
    expect(matchType("INDENT"), "Expected indent");
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

  const parseOptionalMetaName = () => match("(") ? trailingEndParen(parseIdentifier()) :  null;

  const parseFunctionDef = () => {

    const parseOptionalReturnType = (arrowToken: Token) => {
      const list = [parseExpr()]
      while (match(",")) list.push(parseExpr())
      return list.length > 1 ? new ParseTuple(arrowToken, list) : list[0];
    }
    const parseFunctionParamListParen = () => (expect("(", `Expected '(' after function name`), parseFunctionParamList(")"))

    const parseBody = () => {
      if (matchType("NEWLINE")) return null;
      expect(":", "Expected ':'");
      const token = previous;
      if (matchType("NEWLINE")) return trailingNewline(parseMultilineBlock(token));
      else return trailingNewline(parseExpr());
    }

    return createNamedFunc(state, previous, parseOptionalMetaName(), 
      parseIdentifier(), match("!") ? parseFunctionTypeParameters() : [], 
      parseFunctionParamListParen(), match('->') ? parseOptionalReturnType(previous) : null, 
      parseKeywords(), parseBody())
  };
  const parseLambda = () => {
    const lambda = parseBracelessLambda();
    matchType("NEWLINE");
    expect("}", "Expected '}' after lambda body");
    return lambda;
  };
  const parseKeywords = () => {
    const kw: ParseNode[] = []
    while (token?.value === "@") kw.push(parseLiteral());
    return kw;
  }
  const parseOptionalReturnType = () => match(":") ? parseExpr() : null
  const parseSingleOrMultilineBody = () => 
    matchType("NEWLINE") ? parseMultilineBlock(previous) 
    : new ParseStatements(previous, [parseExpr()])

  const parseBracelessLambda = (): ParseNode => {
    return createAnonymousFunc(state, previous, parseFunctionParamList("|"), 
      parseKeywords(), parseOptionalReturnType(), parseSingleOrMultilineBody())
  };
  const parseOptionalMetaClass = () => match("<") ? parseIdentifier() :  null;
  const parseClassDef = (): ParseNode => {
    return createClassDef(state, previous,  
      parseIdentifier(), match("!") ? parseFunctionTypeParameters() : [], 
      parseOptionalMetaClass(),
      parseKeywords(), parseColonBlock("class definition header"))
  };

  const parseElse = (isExpr: boolean): ParseIf | ParseElse | null => {
    if (match("elif")) return parseIf(previous, isExpr, "elif condition")
    else if (match("else")) return new ParseElse(previous, parseColonBlock("else"))
    return null
  }
  const parseIf = (ifToken: Token, isExpr: boolean, message: string = "if condition"): ParseIf =>
    new ParseIf(ifToken, isExpr, parseExpr(), parseColonBlock(message), parseElse(isExpr));

  const parseWhile = () => new ParseWhile(previous, parseExpr(), parseColonBlock('while condition')); // prettier-ignore

  const parseExpressionStatement2 = () => {
    let expr = parseExpr();
    if (match(",")) {
      const list = [expr, parseExpr()];
      while (match(",")) list.push(parseExpr());
      expr = new ParseTuple(previous, list);
    }
    if (match(":="))      return new ParseLet(previous, assertLeftSide(expr), null, parseAssignExpr());
    else if (match("::")) return new ParseLetConst(previous, assertIdentifier(expr), parseAssignExpr());
    else if (match("="))  return new ParseSet(previous, expr, parseAssignExpr());
    else if (match("+=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("-=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("*=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("/=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match(":")) {
      const type = parseExpr();
      const value = match("=") ? parseAssignExpr() : null
      return new ParseLet(previous, assertLeftSide(expr), type, value)
    }
    return expr;
  };
  const parseExpressionStatement = () => {
    let expr = parseExpressionStatement2();
    if (match("..."))        expr = new ParseExpand(previous, expr);
    else if (match("while")) expr = new ParseWhileExpr(previous, parseExpr(), expr);
    else if (match("for"))   expr = new ParseForExpr(previous, parseLeftSideMatch(), expectInExpr(), expr);
    else if (match("if"))    expr = new ParseIf(previous, false, parseExpr(), expr, null);
    return trailingNewline(expr);
  };
  const trailingNewline = <T>(x: T) => (expect(matchType("NEWLINE"), "Expected newline"), x);
  const trailingEndParen = <T>(x: T) => (expect(")", "Expected ')'"), x);

  // Expr after in must be lower precedence than expansion dots
  const expectInExpr = () => (expect("in", "Expected 'in' after for identifier"), parseAssignExpr())
  const parseLeftSideMatch = () => {
    let left: ParseIdentifier | ParseTuple = parseIdentifier();
    if (match(",")) {
      const list = [left, parseIdentifier()];
      while (match(",")) list.push(parseIdentifier());
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
  const parseOptionalExpr = () => matchType("NEWLINE") ? null : trailingNewline(parseExpr())

  const parseImport = (importToken: Token) => {
    const module = parseIdentifier()
    const rename = match("as") ? parseIdentifier() : null
    const idents: ParseImportName[] = []
    const parseImportName = () => {
      const name = parseIdentifier()
      const rename = match("as") ? parseIdentifier() : null
      return new ParseImportName(name.token, name, rename)
    }
    if (match("for")) {
      idents.push(parseImportName())
      while (match(",")) idents.push(parseImportName())
    }
    return trailingNewline(new ParseImport(importToken, module, rename, idents))
  }
  const parseBreak = (breakToken: Token) => {
    if (matchType("NEWLINE")) return new ParseBreak(breakToken, null, null)
    const iden = parseIdentifier()
    if (!match("with")) return trailingNewline(new ParseBreak(breakToken, iden, null))
    return trailingNewline(new ParseBreak(breakToken, iden, parseExpr()))
  }
  const parseContinue = (continueToken: Token) => {
    if (matchType("NEWLINE")) return new ParseContinue(continueToken, null)
    const iden = parseIdentifier()
    return trailingNewline(new ParseContinue(continueToken, iden))
  }
  const parseAnnotation = () => {
    const annotation = parseExpr()
    expect("\n", "Expected newline after annotation")
    const stmt = parseStatement()
    compilerAssert(stmt instanceof ParseFunction, "Not implemented for statement type")
    stmt.functionDecl.annotations.push(annotation)
    return stmt
  }

  const parseStatement = (): ParseNode => {
    if (match("fn"))            return parseFunctionDef();
    else if (match("@@"))       return parseAnnotation()
    else if (match("type"))     return parseClassDef();
    else if (match("if"))       return parseIf(previous, false);
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

    while (matchType("NEWLINE"));
    stmts.push(parseStatement());
    while (token && token.type !== "DEDENT") stmts.push(parseStatement());
    return stmts;
  };
  
  advance();
  const rootNode = new ParseStatements(createToken(null!, ""), parseLines());

  const msg = `Expected EOF but got ${previous?.value} (${previous?.type})`;
  compilerAssert(token === undefined, msg, { lexer, token: previous });
  return <ParsedModule>{ rootNode, classDefs: state.classDecls, functionDecls: state.functionDecls };
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