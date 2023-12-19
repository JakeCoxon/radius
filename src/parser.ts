import { ArgumentTypePair, ParseAnd, ParseNode, ParseBreak, ParseCall, ParseCast, ParseCompTime, ParseContinue, ParseDict, ParseExpand, ParseField, ParseFor, ParseForExpr, ParseIf, ParseLet, ParseLetConst, ParseList, ParseListComp, ParseMeta, ParseNot, ParseNumber, ParseOpEq, ParseOperator, ParseOr, ParseReturn, ParseSet, ParseStatements, ParseString, ParseIdentifier, ParseWhile, ParseWhileExpr, ParserFunctionDecl, Token, compilerAssert, ParsePostCall, ParseSymbol, ParseNote, ParseSlice, ParseSubscript, ParserClassDecl, ParseClass, ParseFunction, createToken, ParseBoolean, ParseElse, ParseMetaIf, ParseMetaFor } from "./defs";

type LexerState = { significantNewlines: boolean; parenStack: string[] };

function* tokenize(input: string, state: LexerState): Generator<Token> {
  const regexes = {
    KEYWORD:
      /^(?:and|as\!|as|break|class|continue|comptime|def|defn|elif|else|fn|for|if|ifx|in|lambda|meta|null|not|or|pass|return|try|while|with|struct|interface)(?=\W)/, // note \b
    IDENTIFIER: /^[a-zA-Z_][a-zA-Z_0-9-]*/,
    STRING: /^(?:"(?:[^"\\]|\\.)*")/,
    SPECIALNUMBER: /^0o[0-7]+|^0x[0-9a-fA-F_]+|^0b[01_]+/,
    NUMBER: /^-?(0|[1-9][0-9_]*)(\.[0-9_]+)?(?:[eE][+-]?[0-9]+)?/,
    COMMENT: /^#.+(?=\n)/,
    OPENPAREN: /^[\[\{\(]/,
    CLOSEPAREN: /^[\]\}\)]/,
    PUNCTUATION: /^(?:==|!=|:=|<=|>=|\+=|\-=|\*=|\/=|::|\|\>|\.\.\.|[@!:,=<>\-+\.*\/'\|])/,
    NEWLINE: /^\n/,
    WHITESPACE: /^[ ]+/ // Not newline
  };

  const tokens: Token[] = [];
  const indents = [0];

  let lineNumber = 1;
  let lineStart = 0;
  let charIndex = 0;
  let prevCharIndex = 0;

  let match: RegExpExecArray | null;

  const matchAndTrimLine = (regex: RegExp) => {
    if ((match = regex.exec(remain))) {
      remain = remain.substring(match[0].length);
      prevCharIndex = charIndex;
      charIndex += match[0].length;
      return true;
    }
  };
  const pushToken = (type: string, value: string = match![0]) => {
    // (state as any).remain = remain; // debug
    const token = createToken(value, type)
    token.location.column = charIndex - lineStart;
    token.location.line = lineNumber;
    tokens.push(token);
    return token;
  };

  // First is line by line
  let remain = input;
  while (remain.length > 0) {
    if (matchAndTrimLine(regexes.NEWLINE)) {
      yield pushToken("NEWLINE");
      lineNumber++;
      lineStart = charIndex;
      continue;
    }

    let matchIndent = remain.length > 0 && matchAndTrimLine(/^ */);
    if (matchIndent) {
      const numSpaces = match![0].length ?? 0;

      if (numSpaces > indents[indents.length - 1]) {
        indents.push(numSpaces);
        yield pushToken("INDENT");
      }
      while (numSpaces < indents[indents.length - 1]) {
        indents.pop();
        yield pushToken("DEDENT");
        yield pushToken("NEWLINE");
      }
    }

    // Tokens after the indentation, or within a grouped expression
    while (remain.length > 0 && (!state.significantNewlines || remain[0] !== "\n")) {
      let matchType = (() => {
        for (const [type, regex] of Object.entries(regexes)) {
          if (matchAndTrimLine(regex)) return type;
        }
      })();

      if (!matchType) {
        const errline = input.substring(lineStart, input.indexOf("\n", lineStart));
        const repeat = " ".repeat(charIndex - lineStart);
        const message = `Unable to tokenize line ${lineNumber} \n${errline}\n${repeat}^-- here`;
        throw new Error(message);
      }
      if (matchType === "NEWLINE") { lineNumber++; lineStart = charIndex; } // prettier-ignore
      else if (matchType === "WHITESPACE" || matchType === "COMMENT") undefined;
      else yield pushToken(matchType);
    }
  }

  while (indents.length > 1) {
    yield pushToken("NEWLINE", "");
    yield pushToken("DEDENT", "");
    indents.pop();
  }
  yield pushToken("NEWLINE", "");
}

const makeAdvancedLexer = (text: string) => {
  const state = { significantNewlines: true, parenStack: [] as string[] };
  const generator = tokenize(text, state);
  let tokens: Token[] = [];
  let previous: Token;
  const getToken = () => {
    const gen = generator.next();
    const token = gen.value;

    if (token?.value === "|") {
      if (state.parenStack.at(-1) === "|") state.parenStack.pop();
      else if (previous?.value === "{") {
        state.parenStack[state.parenStack.length - 1] = "{|";
        state.parenStack.push(token.value);
      }
    }
    if (token?.type === "OPENPAREN") state.parenStack.push(token.value);
    else if (token?.type === "CLOSEPAREN") state.parenStack.pop();
    const last = state.parenStack.at(-1);
    state.significantNewlines = last === undefined || last === "{|";

    if (token) tokens.push(token);
    previous = token;
    return token;
  };
  return { text, tokens, state, getToken };
};



export const tokenString = (token: Token) => token.value;


export const makeParser = (input: string) => {
  let token: Token | undefined;
  let previous: Token = undefined!;
  let prevSignificantNewlines;

  const functionDecls: ParserFunctionDecl[] = []
  const classDecls: ParserClassDecl[] = []

  const lexer = makeAdvancedLexer(input);

  const advance = () => {
    prevSignificantNewlines = lexer.state.significantNewlines;
    previous = token!;
    token = lexer.getToken();
    if (previous?.type === "NEWLINE") while (token?.type === "NEWLINE") token = lexer.getToken();
  };

  const expectError = (error: string) => {
    const num = (token as any)?.lineNumber ?? (previous as any)?.lineNumber;
    const value = token?.value !== undefined ? `'${token?.value}' (${token?.type})` : "EOL";
    const msg = `${error} got ${value} on line ${num}`;
    compilerAssert(false, msg, { lexer, token });
  };
  const expect = (expected: string | boolean, error: string) => {
    if (expected === true) return previous;
    if (expected === false) expectError(error);
    if (!(token && tokenString(token) === expected)) expectError(error);
    advance();
    return previous;
  };
  const match = (expected: string) =>
    token && tokenString(token) === expected ? (advance(), true) : false;
  const matchType = (expected: string) =>
    token && token.type === expected ? (advance(), true) : false;
  
  const assertIdentifier = (ast: ParseNode): ParseIdentifier => (compilerAssert(ast instanceof ParseIdentifier, "Expected identifier"), ast);

  const parseParens = () => {
    const expr = parseExpr();
    expect(")", `Expected ')' after expression`);
    return expr;
  };
  const parseList = (): ParseNode => {
    const listToken = previous;
    if (match("]")) return new ParseList(listToken, []);

    const list = [parseExpr()];       while (match(",")) list.push(parseExpr());
    const mapping: ParseNode[] = [];   while (match("|")) mapping.push(parseExpr());
    const reduce = match("|>") ? parseExpr() : null;
    expect("]", `Expected ']' after list item`);
    if (!mapping.length && !reduce) return new ParseList(listToken, list);
    return new ParseListComp(listToken, list, mapping, reduce);
  };
  const parseNumberLiteral = () => {
    let literal = tokenString(previous).replace(/_/g, "");
    if (tokenString(previous).startsWith("0x"))   return new ParseNumber(parseInt(literal.substring(2), 16));
    if (tokenString(previous).startsWith("0b"))   return new ParseNumber(parseInt(literal.substring(2), 2));
    return new ParseNumber(previous); // Number(literal))
  };
  const parseArgs = () => {
    if (match(")")) return [];
    const args = [parseExpr()];
    while (match(",")) args.push(parseExpr());
    expect(")", `Expected ')' after argument list`);
    return args;
  };

  const matchNumberLiteral = () =>
    (typeof token?.value === "string" &&
      ((token.value[0] >= "0" && token.value[0] <= "9") ||
        (token.value[0] === "-" && token.value[1] >= "0" && token.value[1] <= "9"))) ? (advance(), true) : false;

  const parseIdentifier = () => {
    const token = expect(matchType("IDENTIFIER"), `Expected identifier`); 
    return new ParseIdentifier(token);
  }

  const parseDictPair = (): [ParseNode, ParseNode] => {
    return [parseExpr(), (expect(":", "Expected ':' after dict key"), parseExpr())]
  }
  const parseDict = (dictToken: Token): ParseNode => {
    if (match("}")) return new ParseDict(dictToken, [])
    const pairs = [parseDictPair()];
    while (match(",") && !match("}")) pairs.push(parseDictPair());
    expect("}", "Expected '}' after dict value");
    return new ParseDict(dictToken, pairs);
  };

  const matchStringLiteral = () => typeof token?.value === "string" && token.value[0] === '"' ? (advance(), true) : false;

  const parseLiteral = (): ParseNode => {
    if (match("("))       return parseParens();
    else if (match("["))  return parseList();
    // else if (match("`"))  return [createToken("syntax-quote"), parseExpr()];
    // else if (match("~@")) return [createToken("unquote-splice"), parseExpr()];
    // else if (match("~"))  return [createToken("unquote"), parseExpr()];
    // else if (match(":"))  return [createToken("keyword"), parseExpr()];
    else if (match("'"))  return new ParseSymbol(parseIdentifier().token);
    else if (match("@"))  return new ParseNote(previous, parseExpr());
    else if (match("{"))  return match("|") ? parseLambda() : parseDict(previous)
    else if (matchStringLiteral()) return new ParseString(previous)
    else if (matchNumberLiteral()) return parseNumberLiteral();
    else if (prevSignificantNewlines && match("|")) return parseBracelessLambda();
    else if (match("true") || match("false")) return new ParseBoolean(previous);
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

    return new ParseSlice(sliceToken, left, l[0], l[1], l[2], isStatic)
  };
  const parseFunctionTypeArguments = (callToken: Token, left: ParseNode): ParseNode => {

    const parseTypeArgs = () => {
      const typeArgs = [parseExpr()];
      while (match(",")) typeArgs.push(parseExpr());
      expect(")", `Expected ')' after type list`);
      return typeArgs;
    }

    const typeArgs = match("(") ? parseTypeArgs() : matchNumberLiteral() ? [parseNumberLiteral()] : [parseIdentifier()];
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

  const matchEquality = () =>
    match("<") || match("<=") || match(">") || match(">=") || match("==") || match("!=");
  
  const parseAs = () => {        let left = parseCall();     while (match("as!") || match("as"))   left = new ParseCast(previous, left, parseCall());          return left; };
  const parseFactor = () => {    let left = parseAs();       while (match("*") || match("/"))      left = new ParseOperator(previous, [left, parseAs()]);      return left; };
  const parseSum = () => {       let left = parseFactor();   while (match("+") || match("-"))      left = new ParseOperator(previous, [left, parseFactor()]);  return left; };
  const parseEquality = () => {  let left = parseSum();      while (matchEquality())               left = new ParseOperator(previous, [left, parseSum()]);     return left; };

  const parseIfxInner = (left) => {
    const ifToken = previous;
    const cond = parseEquality();
    expect("else", `Expected 'else' after ifx condition`);
    const right = parseEquality();
    return new ParseIf(ifToken, cond, left, new ParseElse(previous, right))
  }

  const parseIfx = () => {     let left = parseEquality();   while (match("ifx"))    left = parseIfxInner(left);                              return left; };
  const parseAnd = () => {     let left = parseIfx();        while (match("and"))    left = new ParseAnd(previous, [left, parseIfx()]);       return left; };
  const parseOr = () => {      let left = parseAnd();        while (match("or"))     left = new ParseOr(previous, [left, parseAnd()]);        return left; };

  const parseNot = () =>   match("!") || match("not") ?   new ParseNot(previous, parseExpr()) :   parseOr();
  const parseMeta = () =>  match("meta")  ?  new ParseMeta(previous, parseNot())  :  parseNot();

  const parseAssignExpr = parseMeta; // parseAssignExpr excludes expansion dots

  const parseDots = (): ParseNode => {
    let expr = parseMeta();
    if (match("...")) return new ParseExpand(previous, expr);
    else if (match("while")) return new ParseWhileExpr(previous, parseExpr(), expr);
    else if (match("for"))   return new ParseForExpr(previous, parseIdentifier(), expectInExpr(), expr); // prettier-ignore
    return expr;
  };
  const parseExpr = parseDots;

  const parseArg = (): ArgumentTypePair => {
    const name = parseIdentifier()
    if (match(":")) return [name, parseExpr()];
    return [name, null];
  };
  const parseArgList = (final: string = ")") => {
    if (match(final)) return [];
    const args = [parseArg()];
    while (match(",")) args.push(parseArg());
    expect(final, `Expected '${final}' after arg list`);
    return args;
  };
  const parseColonBlock = (afterMessage: string): ParseNode => {
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
  const parseFunctionDef = () => {
    const defnToken = previous;

    const parseOptionalReturnType = () => {
      if (token && token.value !== "@" && token.value !== ":" && token.type !== "NEWLINE")
        return parseExpr();
      return null
    }

    const parseOptionalMetaName = () => match("(") ? trailingEndParen(parseIdentifier()) :  null;

    const parseBody = () => {
      if (matchType("NEWLINE")) return null;
      expect(":", "Expected ':'");
      const token = previous;
      if (matchType("NEWLINE")) return trailingNewline(parseMultilineBlock(token));
      else return trailingNewline(parseExpr());
    }

    const decl: ParserFunctionDecl = {
      id: undefined,
      token: defnToken,
      functionMetaName: parseOptionalMetaName(),
      name: parseIdentifier(),
      debugName: '',
      typeArgs: match("!") ? parseFunctionTypeParameters() : [],
      args: (expect("(", `Expected '(' after function name`), parseArgList(")")),
      returnType: parseOptionalReturnType(),
      keywords: parseKeywords(),
      body: parseBody()
    };
    decl.debugName = decl.name!.token.value;
    functionDecls.push(decl)

    return new ParseFunction(defnToken, decl)
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
    const lambdaToken = previous;

    const decl: ParserFunctionDecl = {
      id: undefined,
      debugName: `<anonymous line ${lambdaToken.location.line}>`,
      token: lambdaToken,
      functionMetaName: null,
      name: null,
      typeArgs: [],
      args: parseArgList("|"),
      keywords: parseKeywords(),
      anonymous: true,
      returnType: parseOptionalReturnType(),
      body: parseSingleOrMultilineBody()
    };
    functionDecls.push(decl)

    return new ParseFunction(lambdaToken, decl)
  };
  const parseClassDef = (): ParseNode => {
    const classToken = previous;
    const decl: ParserClassDecl = {
      id: undefined,
      token: classToken,
      debugName: '',
      metaType: match("(") ? trailingEndParen(parseIdentifier()) : null,
      name: parseIdentifier(),
      typeArgs: match("!") ? parseFunctionTypeParameters() : [],
      keywords: parseKeywords(),
      body: parseColonBlock("class definition header")
    }
    decl.debugName = decl.name!.token.value;
    classDecls.push(decl)

    return new ParseClass(classToken, decl);
  };

  const parseElse = () => {
    if (match("elif")) return parseIf(previous, "elif condition")
    else if (match("else")) return new ParseElse(previous, parseColonBlock("else"))
    return null
  }
  const parseIf = (ifToken: Token, message: string = "if condition") => {
    return new ParseIf(ifToken, parseExpr(), parseColonBlock(message), parseElse());
  };

  const parseWhile = () => new ParseWhile(previous, parseExpr(), parseColonBlock('while condition')); // prettier-ignore

  const parseExpressionStatement2 = () => {
    const expr = parseExpr();
    if (match(":="))      return new ParseLet(previous, assertIdentifier(expr), null, parseAssignExpr());
    else if (match("::")) return new ParseLetConst(previous, assertIdentifier(expr), parseAssignExpr());
    else if (match("="))  return new ParseSet(previous, expr, parseAssignExpr());
    else if (match("+=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("-=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("*=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match("/=")) return new ParseOpEq(previous, expr, parseAssignExpr());
    else if (match(":")) {
      const type = parseExpr();
      const value = match("=") ? parseAssignExpr() : null
      return new ParseLet(previous, assertIdentifier(expr), type, value)
    }
    return expr;
  };
  const parseExpressionStatement = () => {
    let expr = parseExpressionStatement2();
    if (match("..."))        expr = new ParseExpand(previous, expr);
    else if (match("while")) expr = new ParseWhileExpr(previous, parseExpr(), expr);
    else if (match("for"))   expr = new ParseForExpr(previous, parseIdentifier(), expectInExpr(), expr);
    else if (match("if"))    expr = new ParseIf(previous, parseExpr(), expr, null);
    return trailingNewline(expr);
  };
  const trailingNewline = <T>(x: T) => (expect(matchType("NEWLINE"), "Expected newline"), x);
  const trailingEndParen = <T>(x: T) => (expect(")", "Expected ')'"), x);

  const expectInExpr = () => (expect("in", "Expected 'in' after for identifier"), parseExpr())
  const parseForStatement = () =>
    new ParseFor(previous, parseIdentifier(), expectInExpr(), parseColonBlock("for list-expression")); // prettier-ignore

  const parseMetaStatement = (metaToken) => {
    if (match("if")) return new ParseMetaIf(metaToken, parseIf(previous));
    if (match("for")) return new ParseMetaFor(metaToken, parseForStatement());
    return new ParseMeta(previous, parseStatement());
  }
  const parseOptionalExpr = () =>  matchType("NEWLINE") ? null : trailingNewline(parseExpr())

  const parseStatement = (): ParseNode => {
    if (match("fn"))            return parseFunctionDef();
    else if (match("struct"))   return parseClassDef();
    else if (match("if"))       return parseIf(previous);
    else if (match("while"))    return parseWhile();
    else if (match("comptime")) return new ParseCompTime(previous, parseColonBlock("comptime"));
    else if (match("return"))   return new ParseReturn(previous, parseOptionalExpr());
    else if (match("break"))    return new ParseBreak(previous, parseOptionalExpr());
    else if (match("continue")) return new ParseContinue(previous, parseOptionalExpr());
    else if (match("for"))      return parseForStatement();
    else if (match("meta"))     return parseMetaStatement(previous)
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
  const ast = new ParseStatements(createToken(""), parseLines());

  const msg = `Expected EOF but got ${previous?.value} (${previous?.type})`;
  compilerAssert(token === undefined, msg, { lexer, token: previous });
  return { classDecls, functionDecls, ast };
};
