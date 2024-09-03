import { isParseVoid, BytecodeWriter, FunctionDefinition, Type, Binding, LetAst, UserCallAst, CallAst, Ast, NumberAst, OperatorAst, SetAst, OrAst, AndAst, ListAst, IfAst, StatementsAst, Scope, createScope, Closure, ExternalFunction, compilerAssert, VoidType, IntType, FunctionPrototype, Vm, ParseTreeTable, Token, createStatements, DoubleType, FloatType, StringType, expectMap, bytecodeToString, ParseCall, ParseIdentifier, ParseNode, CompiledFunction, AstRoot, isAst, pushSubCompilerState, ParseNil, createToken, ParseStatements, FunctionType, StringAst, WhileAst, BoolAst, BindingAst, SourceLocation, BytecodeInstr, ReturnAst, ParserFunctionDecl, ScopeEventsSymbol, BoolType, Tuple, ParseTuple, hashValues, TaskContext, ParseElse, ParseIf, InstructionMapping, GlobalCompilerState, expectType, expectAst, expectAll, expectAsts, BreakAst, LabelBlock, BlockAst, findLabelBlockByType, ParserClassDecl, ClassDefinition, isType, CompiledClass, ConcreteClassType, FieldAst, ParseField, SetFieldAst, CompilerError, VoidAst, SubCompilerState, ParseLetConst, PrimitiveType, CastAst, ParseFunction, ListTypeConstructor, SubscriptAst, ExternalTypeConstructor, ParameterizedType, isParameterizedTypeOf, ParseMeta, createAnonymousParserFunctionDecl, NotAst, BytecodeProgram, ParseImport, createCompilerError, createAnonymousToken, textColors, ParseCompilerIden, TypeField, ParseValue, ParseConstructor, ConstructorAst, TypeVariable, TypeMatcher, TypeConstructor, TypeInfo, TupleTypeConstructor, ParsedModule, Module, ParseSymbol, ScopeParentSymbol, isPlainObject, ParseLet, ParseList, ParseExpand, ParseBlock, findLabelByBinding, ParseSubscript, ParseNumber, ParseQuote, ParseWhile, ParseOperator, ParseBytecode, ParseOpEq, ParseSet, ParseFreshIden, UnknownObject, ParseNote, DefaultConsAst, RawPointerType, ValueFieldAst, SetValueFieldAst, FloatLiteralType, IntLiteralType, CompilerFunction, DerefAst, SetDerefAst, ParseSlice, CompilerFunctionCallContext, NeverType, LoopObject, CompTimeObjAst, CompileTimeObjectType, NamedArgAst, TypeCheckVar, TypeCheckConfig, u8Type, u64Type, isTypeScalar, FreshBindingToken, isCompilerCallable, ParseIs, OptionTypeConstructor, VariantCastAst } from "./defs";
import { CompileTimeFunctionCallArg, FunctionCallArg, insertFunctionDefinition, functionCompileTimeCompileTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall, compileExportedFunctionTask } from "./compiler_functions";
import { Event, Task, TaskDef, Unit, isTask, isTaskResult, withContext } from "./tasks";
import { createCompilerModuleTask, createListConstructor, defaultMetaFunction, optionBlockSugar, optionCastSugar, orElseSugar, print, questionSugar, smartCastSugar } from "./compiler_sugar";
import { expandDotsSugar, expandFuncAllSugar, expandFuncAnySugar, expandFuncConcatSugar, expandFuncFirstSugar, expandFuncLastSugar, expandFuncMaxSugar, expandFuncMinSugar, expandFuncSumSugar, expandIteratorSugar, foldSugar, forExprSugar, forLoopSugar, listComprehensionSugar, listConstructorSugar, sliceSugar, whileExprSugar } from "./compiler_iterator"

export const pushBytecode = <T extends BytecodeInstr>(out: BytecodeWriter, token: Token, instr: T) => {
  out.bytecode.locations.push(token.location);
  out.bytecode.code.push(instr)
  return instr;
}

export const visitParseNode = (out: BytecodeWriter, expr: ParseNode) => {
  out.location = expr.token.location
  compilerAssert(expr.key, "$expr not found", { expr })
  const table = out.instructionTable
  const instrWriter = expectMap(table, expr.key, `Not implemented parser node $key in ${table === BytecodeDefault ? 'default' : 'second order'} table`)
  instrWriter(out, expr as any)
}
export const visitParseNodeAndError = (out: BytecodeWriter, expr: ParseNode) => {
  try {
    visitParseNode(out, expr);
  } catch (e) {
    if (e instanceof CompilerError) { (e.info as any).location = out.location }
    throw e
  }
}
export const visitAll = (out: BytecodeWriter, exprs: ParseNode[]) => {
  exprs.forEach(expr => visitParseNode(out, expr))
}
export const writeMeta = (out: BytecodeWriter, expr: ParseNode) => {
  visitParseNode({ location: expr.token.location, bytecode: out.bytecode, instructionTable: BytecodeDefault, globalCompilerState: out.globalCompilerState, state: out.state }, expr)
}
export const pushGeneratedBytecode = <T extends BytecodeInstr>(out: BytecodeWriter, instr: T) => {
  out.bytecode.code.push(instr);
  out.bytecode.locations.push(new SourceLocation(-1, -1, null!));
  return instr;
}

export const BytecodeDefault: ParseTreeTable = {
  is:        (out, node) => compilerAssert(false, "Not implemented 'is' in BytecodeDefault"),
  cast:      (out, node) => compilerAssert(false, "Not implemented 'cast' in BytecodeDefault"),
  orelse:    (out, node) => compilerAssert(false, "Not implemented 'orelse' in BytecodeDefault"),
  forexpr:   (out, node) => compilerAssert(false, "Not implemented 'forexpr' in BytecodeDefault"),
  whileexpr: (out, node) => compilerAssert(false, "Not implemented 'whileexpr' in BytecodeDefault"),
  expand:    (out, node) => compilerAssert(false, "Not implemented 'expand' in BytecodeDefault"),
  void:      (out, node) => compilerAssert(false, "Not implemented 'void' in BytecodeDefault"),
  
  slice:     (out, node) => compilerAssert(false, "Not implemented 'slice' in BytecodeDefault"),
  class:     (out, node) => compilerAssert(false, "Not implemented 'class' in BytecodeDefault"),
  metafor:   (out, node) => compilerAssert(false, "Not implemented 'metafor' in BytecodeDefault"),
  for:       (out, node) => compilerAssert(false, "Not implemented 'for' in BytecodeDefault"),
  opeq:      (out, node) => compilerAssert(false, "Not implemented 'opeq' in BytecodeDefault"),
  import:    (out, node) => compilerAssert(false, "Not implemented 'import' in BytecodeDefault"),
  bytecode:  (out, node) => compilerAssert(false, "Not implemented 'bytecode' in BytecodeDefault"),
  fold:      (out, node) => compilerAssert(false, "Not implemented 'fold' in BytecodeDefault"),
  namedarg:  (out, node) => compilerAssert(false, "Not implemented 'namedarg' in BytecodeDefault"),
  question:  (out, node) => compilerAssert(false, "Not implemented 'question' in BytecodeDefault"),
  breakopt:  (out, node) => compilerAssert(false, "Not implemented 'breakopt' in BytecodeDefault"),
  case:      (out, node) => compilerAssert(false, "Not implemented 'case' in BytecodeDefault"),
  match:     (out, node) => compilerAssert(false, "Not implemented 'match' in BytecodeDefault"),
  constructor: (out, node) => compilerAssert(false, "Not implemented 'constructor' in BytecodeDefault"),
  compileriden: (out, node) => compilerAssert(false, "Not implemented 'compileriden' in BytecodeDefault"),

  value:   (out, node) => pushBytecode(out, node.token, { type: "push", value: node.value }), 
  number:  (out, node) => pushBytecode(out, node.token, { type: "push", value: Number(node.token.value) }), 
  string:  (out, node) => pushBytecode(out, node.token, { type: "push", value: node.string }), 
  nil:     (out, node) => pushBytecode(out, node.token, { type: "push", value: null }), 
  boolean: (out, node) => pushBytecode(out, node.token, { type: "push", value: node.token.value !== 'false' }), 
  list:    (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'list', count: node.exprs.length })),
  tuple:   (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'tuple', count: node.exprs.length })),
  symbol:  (out, node) => pushBytecode(out, node.token, { type: "push", value: node.token.value }),

  freshiden:  (out, node) => pushBytecode(out, node.token, { type: "binding", name: node.freshBindingToken.identifier }),
  identifier: (out, node) => pushBytecode(out, node.token, { type: "binding", name: node.token.value }), 
  operator:   (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'operator', name: node.token.value, count: node.exprs.length })), 
  set:        (out, node) => (visitParseNode(out, node.value), pushBytecode(out, node.token, { type: 'setlocal', name: node.left instanceof ParseFreshIden ? node.left.freshBindingToken.identifier : node.left.token.value })), 
  letconst:   (out, node) => (visitParseNode(out, node.value), pushBytecode(out, node.token, { type: 'letlocal', name: node.name instanceof ParseFreshIden ? node.name.freshBindingToken.identifier : node.name.token.value, t: false, v: true })), 
  meta:       (out, node) => (visitParseNode(out, node.expr)),
  comptime:   (out, node) => (visitParseNode(out, node.expr)),
  not:        (out, node) => (visitParseNode(out, node.expr), pushBytecode(out, node.token, { type: 'not' })),

  evalfunc: (out, node) => (node.typeArgs.map(x => writeMeta(out, x)), visitAll(out, node.args), pushBytecode(out, node.token, { type: "evalfunc", func: node.func })),
  concurrency: (out, node) => (visitAll(out, node.fns), pushBytecode(out, node.token, { type: 'concurrency', count: node.fns.length })),

  note: (out, node) => {
    if (node.expr instanceof ParseCall) {
      if (node.expr.left.token.value === 'concat') return expandFuncConcatSugar(out, node, node.expr.args)
    }
    compilerAssert(false, "Not implemented", { node })
  },

  dict: (out, node) => {
    node.pairs.forEach(([key, value]) => {
      visitParseNode(out, new ParseSymbol(key.token))
      visitParseNode(out, value)
    })
    pushBytecode(out, node.token, { type: 'dict', count: node.pairs.length })
  },
  field: (out, node) => {
    visitParseNode(out, node.expr)
    compilerAssert(node.field instanceof ParseIdentifier, "Not supported");
    pushBytecode(out, node.token, { type: 'field', name: node.field.token.value })
  },
  subscript: (out, node) => {
    visitParseNode(out, node.expr)
    visitParseNode(out, node.subscript)
    pushBytecode(out, node.token, { type: 'subscript' })
  },

  iterator: (out, node) => expandIteratorSugar(out, node),

  quote: (out, node) => {
    visitParseNode({ location: node.token.location, bytecode: out.bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.expr)
  },
  
  let: (out, node) => {
    if (node.value) visitParseNode(out, node.value);
    if (node.type) {
      writeMeta(out, node.type);
      pushBytecode(out, node.type.token, { type: 'totype' });
    }
    compilerAssert(!(node.left instanceof ParseTuple), "Tuple not implemented yet", { node })
    const name = node.left instanceof ParseFreshIden ? node.left.freshBindingToken.identifier : node.left.token.value
    pushBytecode(out, node.token, { type: 'letlocal', name, t: !!node.type, v: !!node.value }) 
  },

  function: (out, node) => {
    pushBytecode(out, node.token, { type: "closure", id: insertFunctionDefinition(out.globalCompilerState, node.functionDecl).id, _debugName: node.functionDecl.debugName }) 
  },
  call: (out, node) => {
    visitAll(out, node.typeArgs)
    visitAll(out, node.args);
    if (node.left instanceof ParseValue) { // Internal desugar results in a fixed value sometimes
      visitParseNode(out, node.left)
      pushBytecode(out, node.token, { type: "callobj", count: node.args.length, tcount: node.typeArgs.length })
      return;
    }
    if (node.left instanceof ParseIdentifier || node.left instanceof ParseFreshIden) {
      const name = node.left instanceof ParseFreshIden ? node.left.freshBindingToken.identifier : node.left.token.value
      pushBytecode(out, node.token, { type: "call", name, count: node.args.length, tcount: node.typeArgs.length }); 
      return;
    }
    if (node.left instanceof ParseCompilerIden) {
      pushBytecode(out, node.token, { type: "compilerfn", name: node.left.value, count: node.args.length, tcount: node.typeArgs.length });
      return;
    }
    if (node.left instanceof ParseField) {
      compilerAssert(false, "Call with field not implemented yet", { node })
      return;
    }
    if (node.left instanceof ParseFunction) {
      visitParseNode(out, node.left)
      pushBytecode(out, node.token, { type: "callobj", count: node.args.length, tcount: node.typeArgs.length })
      return;
    }
    compilerAssert(false, "Call with non-identifier not implemented yet", { left: node.left})
  },
  postcall:  (out, node) => {
    if (node.expr instanceof ParseIdentifier) {
      visitParseNode(out, new ParseCall(node.token, node.expr, [node.arg], []))
      return
    }
    compilerAssert(false, "Not implemented 'postcall' in BytecodeDefault", { node })
  },

  return: (out, node) => {
    if (node.expr) visitParseNode(out, node.expr);
    pushBytecode(out, node.token, { type: 'return', r: !!node.expr })
  },
  break: (out, node) => {
    compilerAssert(!node.name, "Not implemented", { node })
    if (node.expr) visitParseNode(out, node.expr);
    const instr = pushBytecode(out, node.token, { type: 'jump', address: 0 })
    findLabelBlockByType(out.state.labelBlock, "break").completion.push((address: number) => { instr.address = address })
  },
  continue: (out, node) => {
    // if (node.expr) visitParseNode(out, node.expr);
    const instr = pushBytecode(out, node.token, { type: 'jump', address: 0 })
    findLabelBlockByType(out.state.labelBlock, "continue").completion.push((address: number) => { instr.address = address })
  },

  listcomp: (out, node) => {
    const list = new ParseList(node.token, node.exprs)
    const trx = node.mapping[0]
    const reducer = node.reduce!
    const call = new ParseCall(node.token, new ParseIdentifier(createAnonymousToken('transduce')), [list], [trx, reducer])
    visitParseNode(out, call)
  },

  statements: (out, node) => {
    node.exprs.forEach((stmt, i) => {
      visitParseNode(out, stmt);
      if (i !== node.exprs.length - 1) pushBytecode(out, node.token, { type: "pop" });
    });
  },
  block: (out, node) => visitParseNode(out, node.statements),
  
  and: (out, node) => {
    visitParseNode(out, node.exprs[0]);
    const jump1 = { type: "jumpf" as const, address: 0 };
    pushBytecode(out, node.exprs[0].token, jump1);
    pushBytecode(out, node.exprs[0].token, { type: 'pop' });
    visitParseNode(out, node.exprs[1])
    jump1.address = out.bytecode.code.length;
  },

  or: (out, node) => {
    visitParseNode(out, node.exprs[0]);
    const jump1 = { type: "jumpf" as const, address: 0 };
    pushBytecode(out, node.exprs[0].token, jump1);
    const jump2 = { type: "jump" as const, address: 0 };
    pushBytecode(out, node.exprs[0].token, jump2);
    jump1.address = out.bytecode.code.length;
    pushBytecode(out, node.exprs[0].token, { type: 'pop' });
    visitParseNode(out, node.exprs[1])
    jump2.address = out.bytecode.code.length;
  },

  else: (out, node) => visitParseNode(out, node.body),
  if: (out, node) => {
    compilerAssert(false, "TODO: Jump instructions must use relative because we do splicing tricks of the bytecode")
    visitParseNode(out, node.condition);
    const jump1 = { type: "jumpf" as const, address: 0 };
    pushBytecode(out, node.condition.token, jump1);
    visitParseNode(out, node.trueBody);
    if (node.falseBody) {
      const jump2 = { type: "jump" as const, address: 0 };
      pushBytecode(out, node.trueBody.token, jump2);
      jump1.address = out.bytecode.code.length;
      visitParseNode(out, node.falseBody);
      jump2.address = out.bytecode.code.length;
    } else {
      jump1.address = out.bytecode.code.length;
    }
  },
  metaif: (out, node) => {
    // Same as if
    const if_ = node.expr;
    visitParseNode(out, if_.condition);
    const jump1 = pushBytecode(out, if_.condition.token, { type: "jumpf", address: 0 });
    visitParseNode(out, if_.trueBody);
    if (if_.falseBody) {
      const jump2 = pushBytecode(out, if_.trueBody.token, { type: "jump", address: 0 });
      jump1.address = out.bytecode.code.length;
      visitParseNode(out, if_.falseBody);
      jump2.address = out.bytecode.code.length;
    } else {
      jump1.address = out.bytecode.code.length;
    }
  },
  while: (out, node) => {
    pushBytecode(out, node.condition.token, { type: "comment", comment: "while begin" });
    const breakBlock = new LabelBlock(out.state.labelBlock, "labelblock", 'break', null)
    const continueBlock = new LabelBlock(breakBlock, "labelblock", 'continue', null)
    out.state.labelBlock = continueBlock;
    const loopTarget = out.bytecode.code.length
    visitParseNode(out, node.condition);
    const jump1 = pushBytecode(out, node.condition.token, { type: "jumpf", address: 0 });
    visitParseNode(out, node.body);
    continueBlock.completion.forEach(f => f(out.bytecode.code.length)) // TODO: How to do this without callbacks?
    continueBlock.completion.length = 0
    pushBytecode(out, node.condition.token, { type: "jump", address: loopTarget });
    jump1.address = out.bytecode.code.length
    breakBlock.completion.forEach(f => f(out.bytecode.code.length)) // TODO: How to do this without callbacks?
    breakBlock.completion.length = 0
    out.state.labelBlock = breakBlock.parent;
    pushBytecode(out, node.condition.token, { type: "comment", comment: "while end" });
  },
  metawhile: (out, node) => BytecodeDefault['while'](out, node.expr)
};

export const BytecodeSecondOrder: ParseTreeTable = {
  is:        (out, node) => compilerAssert(false, "Not implemented 'is'"),
  cast:      (out, node) => compilerAssert(false, "Not implemented 'cast'"),
  symbol:    (out, node) => compilerAssert(false, "Not implemented 'symbol'"),
  class:     (out, node) => compilerAssert(false, "Not implemented 'class'"),
  nil:       (out, node) => compilerAssert(false, "Not implemented 'nil'"),
  metafor:   (out, node) => compilerAssert(false, "Not implemented 'metafor'"),
  import:    (out, node) => compilerAssert(false, "Not implemented 'import'"),
  quote:     (out, node) => compilerAssert(false, "Not implemented 'quote'"),
  case:      (out, node) => compilerAssert(false, "Not implemented 'case'"),
  match:     (out, node) => compilerAssert(false, "Not implemented 'match'"),
  compileriden: (out, node) => compilerAssert(false, "Not implemented 'compileriden'"),

  constructor:  (out, node) => (visitParseNode(out, node.type), visitAll(out, node.args), pushBytecode(out, node.token, { type: 'constructorast', count: node.args.length })),
  identifier:   (out, node) => pushBytecode(out, node.token, { type: "bindingast", name: node.token.value }),

  value:    (out, node) => pushBytecode(out, node.token, { type: "push", value: node.value }), 
  number:   (out, node) => pushBytecode(out, node.token, { type: "numberast", value: node.token.value }),
  string:   (out, node) => pushBytecode(out, node.token, { type: "stringast", value: node.string }),
  boolean:  (out, node) => pushBytecode(out, node.token, { type: "boolast", value: node.token.value !== 'false' }),
  void:     (out, node) => pushBytecode(out, node.token, { type: "voidast" }),

  operator: (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'operatorast', name: node.token.value, count: node.exprs.length })),
  meta:     (out, node) => (writeMeta(out, node.expr), pushBytecode(out, node.token, { type: 'toast' })),
  comptime: (out, node) => writeMeta(out, node.expr),
  letconst: (out, node) => (writeMeta(out, node.value), pushBytecode(out, node.token, { type: 'letlocal', name: node.name instanceof ParseFreshIden ? node.name.freshBindingToken.identifier : node.name.token.value, t: false, v: true })),
  tuple:    (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'tupleast', count: node.exprs.length })),
  not:      (out, node) => (visitParseNode(out, node.expr), pushBytecode(out, node.token, { type: 'notast' })),

  orelse:   (out, node) => orElseSugar(out, node),
  for:      (out, node) => forLoopSugar(out, node),
  forexpr:  (out, node) => forExprSugar(out, node),
  whileexpr:(out, node) => whileExprSugar(out, node),
  expand:   (out, node) => expandDotsSugar(out, node),
  fold:     (out, node) => foldSugar(out, node),
  listcomp: (out, node) => listComprehensionSugar(out, node),
  slice:    (out, node) => sliceSugar(out, node, null),
  iterator: (out, node) => {
    expandIteratorSugar(out, node)
    pushBytecode(out, node.token, { type: 'toast' })
  },
  question: (out, node) => questionSugar(out, node),

  dict: (out, node) => {
    node.pairs.forEach(([key, value]) => {
      visitParseNode(out, new ParseSymbol(key.token))
      visitParseNode(out, value)
    })
    pushBytecode(out, node.token, { type: 'dictast', count: node.pairs.length })
  },

  freshiden: (out, node) => {
    visitParseNode(out, new ParseIdentifier(createAnonymousToken(node.freshBindingToken.identifier)))
  },

  evalfunc: (out, node) => (node.typeArgs.map(x => writeMeta(out, x)), visitAll(out, node.args), pushBytecode(out, node.token, { type: "evalfunc", func: node.func })),
  concurrency: (out, node) => (visitAll(out, node.fns), pushBytecode(out, node.token, { type: 'concurrency', count: node.fns.length })),

  note: (out, node) => {
    if (node.expr instanceof ParseCall) {
      if (node.expr.left.token.value === 'all') return expandFuncAllSugar(out, node, node.expr.args)
      if (node.expr.left.token.value === 'any') return expandFuncAnySugar(out, node, node.expr.args)
      if (node.expr.left.token.value === 'sum') return expandFuncSumSugar(out, node, node.expr.args)
      if (node.expr.left.token.value === 'last') return expandFuncLastSugar(out, node, node.expr.args)
      if (node.expr.left.token.value === 'first') return expandFuncFirstSugar(out, node, node.expr.args)
      if (node.expr.left.token.value === 'min') return expandFuncMinSugar(out, node, node.expr.args)
      if (node.expr.left.token.value === 'max') return expandFuncMaxSugar(out, node, node.expr.args)
      if (node.expr.left.token.value === 'concat') {
        compilerAssert(false, "Not available for this context", { node })
      }
      // if (node.expr.left.token.value === 'max') return expandFuncMaxSugar(out, node, node.expr.args)
      // if (node.expr.left.token.value === 'min') return expandFuncMinSugar(out, node, node.expr.args)
    }
    compilerAssert(false, "Not implemented", { node: node.token.value, expr: node.expr })
  },

  while: (out, node) => {
    pushBytecode(out, node.token, { type: 'beginblockast', breakType: 'break', name: null })
    pushBytecode(out, node.token, { type: 'beginblockast', breakType: 'continue', name: null })
    visitParseNode(out, node.body);
    pushBytecode(out, node.token, { type: 'endblockast' })
    visitParseNode(out, node.condition)
    pushBytecode(out, node.token, { type: 'whileast' })
    pushBytecode(out, node.token, { type: 'endblockast' })
  },

  bytecode: (out, node) => {
    out.bytecode.code.push(...node.bytecode.code)
    out.bytecode.locations.push(...node.bytecode.locations)
  },

  function: (out, node) => {
    pushBytecode(out, node.token, { type: "closure", id: insertFunctionDefinition(out.globalCompilerState, node.functionDecl).id })
  },
  return: (out, node) => {
    if (node.expr) visitParseNode(out, node.expr);
    pushBytecode(out, node.token, { type: 'returnast', r: !!node.expr })
  },
  break: (out, node) => {
    if (node.name) writeMeta(out, node.name);
    if (node.expr) visitParseNode(out, node.expr);
    pushBytecode(out, node.token, { type: 'breakast', named: !!node.name, v: !!node.expr, breakType: 'break' })
  },
  breakopt: (out, node) => {
    if (node.name) writeMeta(out, node.name);
    pushBytecode(out, node.token, { type: 'breakast', named: false, v: false, breakType: 'option' })
  },
  continue: (out, node) => {
    if (node.name) writeMeta(out, node.name)
    pushBytecode(out, node.token, { type: 'breakast', named: !!node.name, v: false, breakType: 'continue'})
  },
  field: (out, node) => {
    visitParseNode(out, node.expr)
    pushBytecode(out, node.token, { type: 'fieldast', name: node.field.token.value })
  },
  namedarg: (out, node) => {
    pushBytecode(out, node.token, { type: 'push', value: node.name.token.value })
    visitParseNode(out, node.expr)
    pushBytecode(out, node.token, { type: 'namedarg' })
  },

  set: (out, node) => {
    if (node.left instanceof ParseIdentifier) {
      visitParseNode(out, node.value);
      pushBytecode(out, node.token, { type: 'setlocalast', name: node.left.token.value })
      return
    } else if (node.left instanceof ParseFreshIden) {
      visitParseNode(out, node.value);
      pushBytecode(out, node.token, { type: 'setlocalast', name: node.left.freshBindingToken.identifier })
      return
    } else if (node.left instanceof ParseField) {
      visitParseNode(out, node.left.expr);
      visitParseNode(out, node.value);
      pushBytecode(out, node.token, { type: 'setfieldast', name: node.left.field.token.value })
      return
    } else if (node.left instanceof ParseSubscript) {
      visitParseNode(out, node.left.expr)
      visitParseNode(out, node.left.subscript)
      visitParseNode(out, node.value)
      pushBytecode(out, node.token, { type: 'setsubscriptast' })
      return
    } else if (node.left instanceof ParseSlice) {
      sliceSugar(out, node.left, node.value)
      return
    } else if (node.left instanceof ParseMeta) {
      visitParseNode(out, node.left)
      visitParseNode(out, node.value)
      pushBytecode(out, node.token, { type: 'setmetaast' })
      return
    }
    compilerAssert(false, "Not implemented", { node })
  },
  subscript: (out, node) => {
    if (node.isStatic) {
      visitParseNode(out, node.expr)
      writeMeta(out, node.subscript)
      pushBytecode(out, node.token, { type: 'staticsubscriptast' })
      return
    }
    visitParseNode(out, node.expr)
    visitParseNode(out, node.subscript)
    pushBytecode(out, node.token, { type: 'subscriptast' })
  },
  opeq: (out, node) => {
    const op = node.token.value.endsWith('=') ? node.token.value.substring(0, node.token.value.length - 1) : node.token.value
    if (node.left instanceof ParseIdentifier || node.left instanceof ParseFreshIden) {
      visitParseNode(out, node.left)
      visitParseNode(out, node.right)
      pushBytecode(out, node.token, { type: 'operatorast', name: op, count: 2 })
      pushBytecode(out, node.token, { type: 'setlocalast', name: node.left instanceof ParseFreshIden ? node.left.freshBindingToken.identifier : node.left.token.value })
      return
    }
    if (node.left instanceof ParseField) {
      visitParseNode(out, node.left.expr)
      visitParseNode(out, node.left)
      visitParseNode(out, node.right)
      pushBytecode(out, node.token, { type: 'operatorast', name: op, count: 2 })
      pushBytecode(out, node.token, { type: 'setfieldast', name: node.left.field.token.value })
      return
    }
    compilerAssert(false, "Invalid operator", { node })
  },

  postcall: (out, node) => {
    compilerAssert(false, "Not implemented 'postcall'", { node })
  },

  list: (out, node) => listConstructorSugar(out, node),
    //(visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'listast', count: node.exprs.length })),

  and: (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: "andast", count: node.exprs.length })),
  or: (out, node) =>  (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: "orast", count: node.exprs.length })),
  
  let: (out, node) => {
    if (node.value) visitParseNode(out, node.value);
    if (node.type) {
      writeMeta(out, node.type);
      pushBytecode(out, node.type.token, { type: 'totype' });
    }
    if (node.left instanceof ParseTuple) {
      const recur = (tup: ParseTuple) => {
        tup.exprs.forEach(expr => {
          if (expr instanceof ParseTuple) return recur(expr)
          else if (expr instanceof ParseIdentifier || expr instanceof ParseFreshIden)
            pushBytecode(out, expr.token, { type: 'push', value: expr instanceof ParseFreshIden ? expr.freshBindingToken.identifier : expr.token.value })
          else compilerAssert(false, "Expression is invalid for left side of a pattern match statement", { expr, location: expr.token.location })
        })
        pushBytecode(out, tup.token, { type: 'tuple', count: tup.exprs.length })
      }
      recur(node.left)
      pushBytecode(out, node.token, { type: 'letmatchast', t: !!node.type, v: !!node.value })
      return
    }
    const name = node.left instanceof ParseFreshIden ? node.left.freshBindingToken.identifier : node.left.token.value
    pushBytecode(out, node.token, { type: 'letast', name, t: !!node.type, v: !!node.value })
  },

  call: (out, node) => {
    if (node.left instanceof ParseCompilerIden) {
      node.typeArgs.forEach(x => writeMeta(out, x));
      visitAll(out, node.args);
      pushBytecode(out, node.token, { type: "compilerfn", name: node.left.value, count: node.args.length, tcount: node.typeArgs.length });
      return
    }
    if (node.left instanceof ParseIdentifier || node.left instanceof ParseFreshIden) {
      node.typeArgs.forEach(x => writeMeta(out, x));
      visitAll(out, node.args)
      const name = node.left instanceof ParseFreshIden ? node.left.freshBindingToken.identifier : node.left.token.value
      pushBytecode(out, node.token, { type: "callast", name, count: node.args.length, tcount: node.typeArgs.length });
      return;
    }
    if (node.left instanceof ParseField) {
      node.typeArgs.forEach(x => writeMeta(out, x));
      visitParseNode(out, node.left.expr)
      visitAll(out, node.args);
      pushBytecode(out, node.token, { type: "callast", name: node.left.field.token.value, count: node.args.length + 1, tcount: node.typeArgs.length, method: true });
      return;
    }
    if (node.left instanceof ParseFunction || node.left instanceof ParseValue) {
      const name = new ParseFreshIden(node.token, new FreshBindingToken('tmpfn'))
      visitParseNode(out, new ParseLetConst(node.token, name, node.left))
      pushBytecode(out, node.token, { type: 'pop' })
      node.typeArgs.forEach(x => writeMeta(out, x));
      visitAll(out, node.args)
      pushBytecode(out, node.token, { type: "callast", name: name.freshBindingToken.identifier, count: node.args.length, tcount: node.typeArgs.length });
      return
    }
    compilerAssert(false, "Call with non-identifier not implemented yet", { left: node.left})
  },

  statements: (out, node) => {
    pushBytecode(out, node.token, { type: "pushqs" });
    node.exprs.forEach((stmt, i) => {
      visitParseNode(out, stmt);
      if (!isParseVoid(stmt)) pushBytecode(out, node.token, { type: "appendq" });
      pushBytecode(out, node.token, { type: "pop" }); // Even pop the final value
    });
    pushBytecode(out, node.token, { type: "popqs" });
  },
  block: (out, node) => {
    if (node.breakType === 'option') return optionBlockSugar(out, node)
    const name = (node.name instanceof ParseFreshIden ? node.name.freshBindingToken.identifier : node.name?.token.value) ?? null
    pushBytecode(out, node.token, { type: 'beginblockast', breakType: node.breakType, name })
    visitParseNode(out, node.statements)
    pushBytecode(out, node.token, { type: 'endblockast' })
  },

  else: (out, node) => visitParseNode(out, node.body),

  metaif: (out, node) => {
    const if_ = node.expr
    writeMeta(out, if_.condition); // Meta part
    const jump1 = { type: "jumpf" as const, address: 0 };
    pushBytecode(out, if_.condition.token, jump1);
    visitParseNode(out, if_.trueBody);
    if (if_.falseBody) {
      const jump2 = { type: "jump" as const, address: 0 };
      pushBytecode(out, if_.trueBody.token, jump2);
      jump1.address = out.bytecode.code.length;
      compilerAssert(!(if_.falseBody instanceof ParseIf), "Meta elif not implemented yet")
      visitParseNode(out, if_.falseBody);
      jump2.address = out.bytecode.code.length;
    } else {
      jump1.address = out.bytecode.code.length;
    }
  },

  if: (out, node) => {
    // TODO: This is no longer possible because we use if-expr without else clause in iterators
    // if (node.isExpr) compilerAssert(node.falseBody, "If-expression needs false branch")

    if (node.condition instanceof ParseIs) return smartCastSugar(out, node)
    if (node.falseBody) visitParseNode(out, node.falseBody)
    visitParseNode(out, node.trueBody)
    visitParseNode(out, node.condition)
    pushBytecode(out, node.token, { type: "ifast", f: !!node.falseBody, e: node.isExpr });
  },

  metawhile: (out, node) => {
    // Can write this in terms on regular while?
    const condition = node.expr.condition
    const body = node.expr.body
    pushBytecode(out, condition.token, { type: "comment", comment: "while begin" });

    const breakBlock = new LabelBlock(out.state.labelBlock, "labelblock", 'break', null)
    const continueBlock = new LabelBlock(breakBlock, "labelblock", 'continue', null)
    out.state.labelBlock = continueBlock;
    const loopTarget = out.bytecode.code.length
    writeMeta(out, condition); // Meta part
    const jump1 = pushBytecode(out, condition.token, { type: "jumpf", address: 0 });
    visitParseNode(out, body);

    pushBytecode(out, condition.token, { type: "appendq" });
    pushBytecode(out, condition.token, { type: "pop" });

    continueBlock.completion.forEach(f => f(out.bytecode.code.length))
    continueBlock.completion.length = 0
    pushBytecode(out, condition.token, { type: "jump", address: loopTarget });
    jump1.address = out.bytecode.code.length
    breakBlock.completion.forEach(f => f(out.bytecode.code.length))
    breakBlock.completion.length = 0
    out.state.labelBlock = breakBlock.parent;
    pushBytecode(out, condition.token, { type: "comment", comment: "while end" });

  },
};

export const compileFunctionPrototype = (ctx: TaskContext, prototype: FunctionPrototype) => {
  if (prototype.bytecode) return prototype.bytecode;

  prototype.bytecode = { code: [], locations: [] }
  const out: BytecodeWriter = {
    location: undefined!,
    bytecode: prototype.bytecode,
    instructionTable: prototype.initialInstructionTable,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNodeAndError(out, prototype.body)
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan(`Compiled ${prototype.name}`))
  ctx.globalCompiler.logger.log(bytecodeToString(prototype.bytecode))
  ctx.globalCompiler.logger.log("")
  return prototype.bytecode;
};

export function createBytecodeVmAndExecuteTask(ctx: TaskContext, subCompilerState: SubCompilerState, bytecode: BytecodeProgram, scope: Scope): Task<unknown, CompilerError> {
  compilerAssert(ctx.subCompilerState === subCompilerState, "Subcompiler state must have been set already", { fatal: true, subCompilerState, ctxSubCompilerState: ctx.subCompilerState })
  compilerAssert(scope, "Expected scope", { fatal: true })

  const vm: Vm = { ip: 0, stack: [], scope, location: undefined!, bytecode: bytecode!, context: ctx };
  compilerAssert(bytecode, "", { fatal: true })
  subCompilerState.vm = vm

  return (
    TaskDef(executeVmTask, { vm })
    .mapRejected(error => {
      const info = error.info as any
      if (info) {
        if (!info.location) info.location = vm.location;
        if (!info.subCompilerState) info.subCompilerState = subCompilerState
      }
      return error
    })
    .chainFn((task, arg) => {

      compilerAssert(vm.stack.length === 1, "Expected 1 value on stack at end of function. Got $num", { num: vm.stack.length, vm })

      const result = vm.stack.pop();
      // vm = compilerState ? compilerState.vm : undefined!;
      return Task.of(result);
    })
  );
};

export const popValues = (vm: Vm, num: number) => {
  compilerAssert(vm.stack.length >= num, `Expected ${num} values on stack got ${vm.stack.length}`)
  return Array.from(new Array(num)).map(() => vm.stack.pop()).reverse() 
};
export const popStack = (vm: Vm) => {
  compilerAssert(vm.stack.length > 0, `Expected 1 value on stack got ${vm.stack.length}`)
  return vm.stack.pop();
};


const operators: {[key:string]: { op: string, typeCheck: unknown, comptime: (a: number, b: number) => unknown, func: (ctx: CompilerFunctionCallContext, a: Ast, b: Ast) => Task<Ast, CompilerError> }} = {};

export const getOperatorTable = () => operators

const createOperator = (op: string, operatorName: string, typeCheck: (config: TypeCheckConfig) => void, comptime: (a: number, b: number) => unknown) => {
  operators[op] = { 
    op, typeCheck, comptime,
    func: (ctx: CompilerFunctionCallContext, a: Ast, b: Ast) => {
      let metafunc = a.type.typeInfo.metaobject[operatorName]
      if (metafunc) {
        compilerAssert(metafunc instanceof CompilerFunction, "Not implemented yet", { metafunc })
        return metafunc.func(ctx, [], [a, b])
      }
      metafunc = b.type.typeInfo.metaobject[operatorName]
      if (metafunc) {
        compilerAssert(metafunc instanceof CompilerFunction, "Not implemented yet", { metafunc })
        return metafunc.func(ctx, [], [a, b])
      }
      const typeCheckConfig: TypeCheckConfig = { a: { type: a.type }, b: { type: b.type }, inferType: null }
      typeCheck(typeCheckConfig)
      compilerAssert(typeCheckConfig.inferType, "Expected infer type", { type: typeCheckConfig.inferType })
      if (typeCheckConfig.a.type !== a.type) propagateLiteralType(typeCheckConfig.a.type, a)
      if (typeCheckConfig.b.type !== b.type) propagateLiteralType(typeCheckConfig.b.type, b)
      return Task.of(new OperatorAst(typeCheckConfig.inferType, ctx.location, op, [a, b]))
    }
  }
}

export const normalizeNumberType = (from: TypeCheckVar, to: TypeCheckVar) => {
  if (from.type === IntLiteralType) {
    if (to.type === FloatType) from.type = FloatType
    else if (to.type === u8Type) from.type = u8Type
    else if (to.type === u64Type) from.type = u64Type
    else if (to.type === DoubleType) from.type = DoubleType
    else if (to.type === FloatLiteralType) from.type = FloatLiteralType
    else if (to.type === IntType) from.type = IntType
  }
  else if (from.type === FloatLiteralType) {
    if (to.type === FloatType) from.type = FloatType
    else if (to.type === DoubleType) from.type = DoubleType
  }
}
export const numberTypeToConcrete = (from: TypeCheckVar) => {
  if (from.type === IntLiteralType) from.type = IntType
  else if (from.type === FloatLiteralType) from.type = FloatType
}

export const canAssignUnknownTo = (from: unknown, to: unknown) => {
  if (from === to) return true
  return isType(from) && isType(to) && canAssignTypeTo(from, to)
}
export const canAssignTypeTo = (from: Type, to: Type) => {
  if (from === to) return true
  const a = { type: from }, b = { type: to }
  normalizeNumberType(a, b)
  if (a.type === b.type) return true
  
  if (a.type === NeverType) return true

  if (a.type instanceof ParameterizedType && b.type instanceof ParameterizedType) {
    if (a.type.typeConstructor !== b.type.typeConstructor) return false
    if (a.type.args.length !== b.type.args.length) return false
    for (let i = 0; i < a.type.args.length; i++) {
      if (!canAssignUnknownTo(a.type.args[i], b.type.args[i])) return false
    }
    return true
  }

  return false
}

const typecheckNumberOperator = (config: TypeCheckConfig): void => {
  normalizeNumberType(config.a, config.b)
  normalizeNumberType(config.b, config.a)
  compilerAssert(config.a.type === config.b.type, "Expected int, float or double type got $a $b", { a: config.a.type, b: config.b.type })
  config.inferType = config.b.type
}

const typecheckNumberComparison = (config: TypeCheckConfig) => {
  normalizeNumberType(config.a, config.b)
  normalizeNumberType(config.b, config.a)
  numberTypeToConcrete(config.a)
  numberTypeToConcrete(config.b)
  const aok = isTypeScalar(config.a.type)
  const bok = isTypeScalar(config.b.type)
  compilerAssert(aok && bok, "Expected int, float or double type got $a $b", { a: config.a.type, b: config.b.type })
  config.inferType = BoolType
}

const typecheckEquality = (config: TypeCheckConfig) => {
  normalizeNumberType(config.a, config.b)
  normalizeNumberType(config.b, config.a)
  numberTypeToConcrete(config.a)
  numberTypeToConcrete(config.b)
  const aok = config.a.type === BoolType || isTypeScalar(config.a.type)
  const bok = config.b.type === BoolType || isTypeScalar(config.b.type)
  compilerAssert(aok && bok, "Expected bool, int, float or double type got $a $b", { a: config.a.type, b: config.b.type })
  config.inferType = BoolType
}
createOperator("-",  "sub", typecheckNumberOperator,   (a, b) => a - b)
createOperator("*",  "mul", typecheckNumberOperator,   (a, b) => a * b)
createOperator("+",  "add", typecheckNumberOperator,   (a, b) => a + b)
createOperator("/",  "div", typecheckNumberOperator,   (a, b) => a / b)
createOperator("mod", "mod", typecheckNumberOperator,  (a, b) => a % b)
createOperator(">",  "gt",  typecheckNumberComparison, (a, b) => a > b)
createOperator("<",  "lt",  typecheckNumberComparison, (a, b) => a < b)
createOperator(">=", "gte", typecheckNumberComparison, (a, b) => a >= b)
createOperator("<=", "lte", typecheckNumberComparison, (a, b) => a <= b)
createOperator("==", "eq",  typecheckEquality,         (a, b) => a == b)
createOperator("!=", "neq", typecheckEquality,         (a, b) => a != b)

export const propagateLiteralType = (inferType: Type, ast: Ast | null): Type => {
  // This is for literals only, not for types that need implicit casts
  if (!ast) return inferType
  if (inferType === IntLiteralType) inferType = IntType
  if (inferType === FloatLiteralType) inferType = FloatType
  if (inferType === ast.type) return inferType
  if (!canAssignTypeTo(ast.type, inferType)) return inferType

  const recur = (ast: Ast) => {
    if (inferType === ast.type) return
    if (ast instanceof VariantCastAst) {
      // This is to support None!never into Option!int, assume that the type was originally allowed to contain never
      ast.type = inferType;
    } else if (ast instanceof NumberAst) {
      ast.type = inferType
    } else if (ast instanceof OperatorAst) {
      ast.type = inferType;
      recur(ast.args[0]); recur(ast.args[1])
    } else if (ast instanceof StatementsAst) {
      ast.type = inferType;
      recur(ast.statements.at(-1)!)
    }
  }
  recur(ast)
  return inferType
}
export const propagatedLiteralAst = (ast: Ast) => { propagateLiteralType(ast.type, ast); return ast }

export const calculateSizeOfType =  (expr: Type): number => {
  // TODO: Padding

  if (expr instanceof ConcreteClassType) {
    compilerAssert(expr, "Expected concrete type") // TODO: Compile it?
    return expr.typeInfo.fields.reduce((acc, x) => acc + calculateSizeOfType(x.fieldType), 0)
  }
  if (expr instanceof PrimitiveType) { return expr.typeInfo.sizeof }
  if (expr instanceof ParameterizedType) { 
    return expr.typeInfo.fields.reduce((acc, x) => acc + calculateSizeOfType(x.fieldType), 0)
  }
  compilerAssert(false, "Not implemented", { expr })
}

const letLocalAst = (vm: Vm, name: string, type: Type | null, value: Ast | null) => {
  compilerAssert(type || value, "Expected type or initial value for let binding $name", { name });
  compilerAssert(!Object.hasOwn(vm.scope, name), `Already defined $name`, { name });
  let inferType = type || value!.type
  compilerAssert(inferType !== VoidType, "Expected type for local $name but got $inferType", { name, inferType, value })
  inferType = propagateLiteralType(inferType, value)
  const binding = new Binding(name, inferType)
  setScopeValueAndResolveEvents(vm.scope, name, binding) // This is for globals usually. Locals should be in order
  compilerAssert(!inferType.typeInfo.isInvalidSize, "Expected concrete type got $inferType which cannot be assigned to", { inferType })
  if (value) {
    compilerAssert(canAssignTypeTo(value.type, inferType), "Mismatch types got $got expected $expected", { got: value.type, expected: inferType })
  }
  binding.definitionCompiler = vm.context.subCompilerState
  value ||= new DefaultConsAst(inferType.typeInfo.isReferenceType ? RawPointerType : inferType, vm.location)
  return new LetAst(VoidType, vm.location, binding, value);
}
const implicitTypeCast = (vm: Vm, ast: Ast, type: Type) => {
  if (type instanceof ParameterizedType && type.typeConstructor === OptionTypeConstructor) {
    if (isType(type.args[0]) && canAssignTypeTo(ast.type, type.args[0])) {
      return optionCastSugar(vm, propagatedLiteralAst(ast), type)
    }
  }
  return Task.of(ast)
}

export function resolveScope(ctx: TaskContext, scope: Scope, name: string): Task<unknown, CompilerError> {
  let checkScope: Scope | undefined = scope;
  while (checkScope) {
    if (checkScope[name] !== undefined) return Task.of(checkScope[name])
    checkScope = (checkScope as any)[ScopeParentSymbol]
  }

  // TODO: This should attach events to every ancestor in the scope chain
  if (!scope[ScopeEventsSymbol]) scope[ScopeEventsSymbol] = {}
  if (!scope[ScopeEventsSymbol][name]) scope[ScopeEventsSymbol][name] = new Event<string, CompilerError>()
  ctx.globalCompiler.allWaitingEvents.push(scope[ScopeEventsSymbol][name])
  return Task.waitFor(scope[ScopeEventsSymbol][name]).mapRejected((error) => {
    return createCompilerError('Binding $name not found in scope', { name })
  })
}

export function callFunctionFromValueTask(ctx: TaskContext, vm: Vm, func: unknown, typeArgs: unknown[], values: unknown[]): Task<Unit, CompilerError> {
  if (func instanceof ExternalFunction) {
    const fnctx: CompilerFunctionCallContext = { location: vm.location, compilerState: ctx.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
    compilerAssert(typeArgs.length === 0, "Not supported", { typeArgs })
    const functionResult = func.func(fnctx, values)
    if (!(functionResult instanceof Task)) {
      vm.stack.push(functionResult); return Task.success()
    }
    return functionResult.chainFn((task, value) => {
      vm.stack.push(value); return Task.success()
    })
  }
  if (func instanceof CompilerFunction) {
    if (func === print) {
      const fnctx: CompilerFunctionCallContext = { location: vm.location, compilerState: ctx.subCompilerState, resultAst: undefined, typeCheckResult: undefined } 
      ;(ctx.globalCompiler.rootScope['static_print'] as ExternalFunction).func(fnctx, values)
      vm.stack.push(null)
      return Task.success()
    }
    compilerAssert(false, "Not implemented", { func, values })
  }

  if (func instanceof Closure) {
    const call: CompileTimeFunctionCallArg = { vm, func: func.func, typeArgs, args: values, parentScope: func.scope };
    return TaskDef(functionCompileTimeCompileTask, call)
  }

  if (func instanceof ClassDefinition) {
    compilerAssert(func.isTypeConstructor, "Expected type constructor class got $func", { func });
    compilerAssert(values.length === 0, "Not implemented", { values })
    const typeVars = typeArgs.filter((x): x is TypeVariable => x instanceof TypeVariable)
    if (typeVars.length) {
      vm.stack.push(new TypeMatcher(func, typeArgs, typeVars))
      return Task.success();
    }
    return (
      TaskDef(compileClassTask, { classDef: func, typeArgs })
      .chainFn((task, type) => { vm.stack.push(type); return Task.success() })
    )
  }
  if (func instanceof ExternalTypeConstructor) {
    compilerAssert(values.length === 0, "Expected no args", { values })
    const typeVars = typeArgs.filter((x): x is TypeVariable => x instanceof TypeVariable)
    if (typeVars.length) {
      vm.stack.push(new TypeMatcher(func, typeArgs, typeVars))
      return Task.success();
    }
    return (
      createParameterizedExternalType(ctx.globalCompiler, func, typeArgs)
      .chainFn((task, type) => { vm.stack.push(type); return Task.success() })
    )
  }
  compilerAssert(!(func instanceof FunctionDefinition), "$func is not handled", { func })
  compilerAssert(false, "$func is not a function", { func })
}

export const unknownToAst = (location: SourceLocation, value: unknown) => {
  if (typeof value === 'number') {
    const type = Math.floor(value) === value ? IntLiteralType : FloatLiteralType
    return new NumberAst(type, location, value)
  }
  if (isAst(value)) return value;
  if (value === null) return new VoidAst(VoidType, location);
  if (value instanceof Binding) return new BindingAst(value.type, location, value);
  if (value instanceof LoopObject) return new CompTimeObjAst(CompileTimeObjectType, location, value)
  if (value instanceof Closure) return new CompTimeObjAst(CompileTimeObjectType, location, value)
  if (value instanceof ExternalFunction) return new CompTimeObjAst(CompileTimeObjectType, location, value)
  if (value instanceof CompilerFunction) return new CompTimeObjAst(CompileTimeObjectType, location, value)
  compilerAssert(false, "Type is not convertable to an AST: $value", { value })
}

const instructions: InstructionMapping = {
  halt: () => compilerAssert(false, "Handled elsewhere"),
  comment: () => {},
  push: (vm, { value }) => vm.stack.push(value),
  nil: (vm) => vm.stack.push(null),

  numberast: (vm, { value }) => {
    const type = value.includes('.') ? FloatLiteralType : IntLiteralType
    const literal = Number(value.replace(/_/g, ""))
    compilerAssert(!Number.isNaN(literal), "Error parsing $value", { value })
    vm.stack.push(new NumberAst(type, vm.location, literal))
  },
  stringast: (vm, { value }) =>   vm.stack.push(new StringAst(StringType, vm.location, value)),
  boolast: (vm, { value }) =>     vm.stack.push(new BoolAst(BoolType, vm.location, value)),
  orast: (vm, { count }) =>       vm.stack.push(new OrAst(BoolType, vm.location, expectAsts(popValues(vm, count)))),
  andast: (vm, { count }) => {
    const [a, b] = expectAsts(popValues(vm, count))
    vm.stack.push(new AndAst(BoolType, vm.location, [propagatedLiteralAst(a), propagatedLiteralAst(b)]))
  },
  voidast: (vm) => vm.stack.push(new VoidAst(VoidType, vm.location)),
  whileast: (vm) =>               vm.stack.push(new WhileAst(VoidType, vm.location, expectAst(popStack(vm)), expectAst(popStack(vm)))),
  returnast: (vm, { r }) => {
    const returnBreak = vm.context.subCompilerState.functionReturnBreakBlock?.binding
    // Check if we're inlining a function, if so we need to break 
    if (returnBreak) {
      vm.stack.push(returnBreak, vm.stack.pop())
      return instructions.breakast(vm, { type: 'breakast', named: true, v: !!r, breakType: 'break' })
    }
    const value = r ? propagatedLiteralAst(expectAst(popStack(vm))) : null
    vm.stack.push(new ReturnAst(NeverType, vm.location, value))
  },
  letast: (vm, { name, t, v }) => {
    const type = t ? expectType(popStack(vm)) : null
    let value = v ? expectAst(popStack(vm)) : null
    const newValue: Task<Ast | null, CompilerError> = (value && type) ? implicitTypeCast(vm, value, type) : Task.of(value)
    return newValue.chainFn((task, ast) => {
      vm.stack.push(letLocalAst(vm, name, type, ast))
      return Task.success()
    })
  },
  letmatchast: (vm, { t, v }) =>  {
    compilerAssert(!t, "Type not supported for tuple let")
    const tuple = popStack(vm)
    const value = v ? expectAst(popStack(vm)) : null
    compilerAssert(value, "Expected value for tuple let")
    compilerAssert(tuple instanceof Tuple, "Expected tuple got $tuple", { tuple })
    const stmts: Ast[] = []
    const recur = (tuple: Tuple, tupleType: Type, rightSide: Ast) => {
      const newName = new FreshBindingToken("tup")
      const letAst = letLocalAst(vm, newName.identifier, null, rightSide)
      const rightSideBinding = new BindingAst(VoidType, vm.location, letAst.binding)
      stmts.push(letAst)

      compilerAssert(tupleType instanceof ParameterizedType && tupleType.typeConstructor === TupleTypeConstructor, "Expected tuple type got $type", { type: tupleType })
      compilerAssert(tuple.values.length === tupleType.typeInfo.fields.length, "Expected $expected fields in tuple got $actual", { expected: letAst.binding.type.typeInfo.fields.length, actual: tuple.values.length, tuple: {...tuple} }) // TODO: Make this more clearer
      tuple.values.map((value, i) => {
        const field = tupleType.typeInfo.fields[i]
        const fieldAst = new FieldAst(field.fieldType, vm.location, rightSideBinding, field)
        if (value instanceof Tuple) return recur(value, field.fieldType, fieldAst)
        compilerAssert(typeof value === 'string', "Expected string got $value", { value })
        stmts.push(letLocalAst(vm, value, field.fieldType, fieldAst))
      })
    }
    recur(tuple, value.type, value)
    vm.stack.push(new StatementsAst(VoidType, vm.location, stmts))
  },
  ifast: (vm, { f, e }) => {
    const cond = propagatedLiteralAst(expectAst(popStack(vm)))
    const trueBody = propagatedLiteralAst(expectAst(popStack(vm)))
    const falseBody = f ? propagatedLiteralAst(expectAst(popStack(vm))) : null
    let resultType: Type = VoidType
    if (e && falseBody) resultType = trueBody.type === NeverType ? falseBody.type : trueBody.type
    if (trueBody.type === NeverType && (!falseBody || falseBody.type === NeverType)) resultType = NeverType // Propagate never type even if it's not an expression if
    // TODO: This is no longer possible because we use if-expr without else clause in iterators
    // if (e) compilerAssert(falseBody && falseBody.type === trueBody.type, "If expression inferred to be of type $trueType but got $falseType", { trueType: trueBody.type, falseType: falseBody?.type })
    compilerAssert(cond.type === BoolType, "Expected bool got $type", { type: cond.type })
    vm.stack.push(new IfAst(resultType, vm.location, cond, trueBody, falseBody))
  },
  evalfunc: (vm, { func }) => { return func(vm) },
  concurrency: (vm, { count }) => {
    const values = popValues(vm, count).reverse()
    const fnctx: CompilerFunctionCallContext = { location: vm.location, compilerState: vm.context.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
    return Task.concurrency(values.map(x => createCallAstFromValue(fnctx, x, [], [])))
  },
  listast: (vm, { count }) => {
    const values = expectAsts(popValues(vm, count))
    const elementType = getCommonType(values.map(x => x.type))
    return createListConstructor(vm, elementType, values)
  },
  callast: (vm, { name, count, tcount, method }) => {
    const args_ = popValues(vm, count)
    const typeArgs = popValues(vm, tcount || 0);
    const receiver = method ? args_.shift() : null
    const args = args_.map(val => {
      if (val instanceof Closure) return new CompTimeObjAst(CompileTimeObjectType, vm.location, val)
      return val
    })
    compilerAssert(args.every(isAst), "Expected ASTs for function call $name", { name, args })

    if (receiver) {
      if (receiver instanceof Module) {
        // TODO: Can this be better?
        return (
          TaskDef(resolveScope, receiver.compilerState.scope, name)
          .chainFn((task, value) => createCallAstFromValueAndPushValue(vm, value, typeArgs, args))
          .wrap(withContext({ subCompilerState: receiver.compilerState }))
        )
      } else if (isPlainObject(receiver)) {
        const func = expectMap(receiver, name, "Expected field $name in object $receiver", { name, receiver })
        return createCallAstFromValueAndPushValue(vm, func, typeArgs, args)
      } else if (isAst(receiver)) {
        return createMethodCall(vm, receiver, name, typeArgs, args)
      }
      args.unshift(expectAst(receiver))
    }

    return (
      TaskDef(resolveScope, vm.scope, name)
      .chainFn((task, value) => createCallAstFromValueAndPushValue(vm, value, typeArgs, args))
    )
  },

  toast: (vm) => vm.stack.push(unknownToAst(vm.location, popStack(vm))),
  namedarg: (vm) => {
    const [name, expr_] = popValues(vm, 2)
    const expr = expectAst(expr_)
    compilerAssert(typeof name === 'string')
    vm.stack.push(new NamedArgAst(expr.type, vm.location, name, expr))
  },

  field: (vm, { name }) => {
    const expr = popStack(vm)
    if (isPlainObject(expr)) {
      compilerAssert(name in expr, "Not found $name in object $expr", { name, expr })
      vm.stack.push(expr[name])
      return
    }
    if (expr instanceof Module) {
      return (
        TaskDef(resolveScope, expr.compilerState.scope, name)
        .chainFn((task, value) => { vm.stack.push(value); return Task.success() })
      )
    }
    if (name === 'sizeof') {

      // There should be a nicer way of doing this across all functions that need it
      const toType = (classDef: ClassDefinition): Task<ConcreteClassType, unknown> => {
        if (classDef.concreteType) return Task.of(classDef.concreteType)
        compilerAssert(classDef.typeArgs.length === 0, "Cannot compile class $classDef to type without specifing type arguments", { classDef })
        return (
          TaskDef(compileClassTask, { classDef, typeArgs: [] })
          .chainFn((task, res) => { compilerAssert(res instanceof ConcreteClassType); return Task.of(res) })
        );
      }

      compilerAssert(isType(expr), "Expected type got $expr", { expr })

      if (expr instanceof ClassDefinition) {
        return (toType(expr).chainFn((task, type) => {
          vm.stack.push(calculateSizeOfType(type)); return Task.success()
        }))
      }

      vm.stack.push(calculateSizeOfType(expr))
      return
    }
    compilerAssert(false, "Not implemented", { expr, name })
  },
  subscript: (vm, { }) => {
    const subscript = popStack(vm)
    const expr = popStack(vm)
    if (typeof subscript === 'string' && isPlainObject(expr)) {
      compilerAssert(subscript in expr, "Not found $subscript in object $expr", { subscript, expr })
      vm.stack.push(expr[subscript])
      return
    }
    if (typeof subscript === 'number' && Array.isArray(expr)) {
      compilerAssert(subscript in expr, "Not found $subscript in object $expr", { subscript, expr })
      vm.stack.push(expr[subscript])
      return
    }
    compilerAssert(false, "Not implemented", { expr, subscript })
  },

  constructorast: (vm, { count }) => {
    const args = expectAsts(popValues(vm, count))
    const type = expectType(popStack(vm))
    if (type instanceof ConcreteClassType || type instanceof ParameterizedType) {
      type.typeInfo.fields.forEach((field, i) => {
        compilerAssert(args[i].type === field.fieldType, "Expected $expected but got $got for field $name of constructor for object $obj", { expected: field.fieldType, got: args[i].type, name: field.name, obj: type})
        compilerAssert(args[i].type !== IntLiteralType && args[i].type !== FloatLiteralType, "Not implemented", { type })
      })
    }
    vm.stack.push(new ConstructorAst(type, vm.location, args))
  },
  operatorast: (vm, { name, count }) => {
    const values = expectAsts(popValues(vm, count));
    compilerAssert(operators[name], "Unexpected operator $name", { name, values })
    if (name == '-' && count == 1) {
      return void vm.stack.push(new OperatorAst(values[0].type, vm.location, name, [new NumberAst(values[0].type, vm.location, 0), values[0]]))
    }
    const ctx: CompilerFunctionCallContext = { location: vm.location, compilerState: vm.context.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
    return (
      operators[name].func(ctx, values[0], values[1])
      .chainFn((task, res) => { vm.stack.push(res); return Task.success() })
    )
  },
  notast: (vm, {}) => {
    let expr = expectAst(popStack(vm));
    if (expr.type !== BoolType) expr = new CastAst(BoolType, vm.location, expr)
    vm.stack.push(new NotAst(BoolType, vm.location, expr))
  },
  setlocalast: (vm, { name }) => {
    return TaskDef(resolveScope, vm.scope, name).chainFn((task, binding) => {
      compilerAssert(binding instanceof Binding, "Expected binding got $binding", { binding })
      const ast = expectAst(popStack(vm))
      propagateLiteralType(binding.type, ast)
      // Hack in implicit casting for now
      let astTask: Task<Ast, CompilerError> = implicitTypeCast(vm, ast, binding.type)
      
      return astTask.chainFn((task, ast) => {
        compilerAssert(binding.type === ast.type, "Type mismatch got $got expected $expected", { got: ast.type, expected: binding.type })
        vm.stack.push(new SetAst(VoidType, vm.location, binding, ast))
        return Task.success()
      })
    });
  },
  setmetaast: (vm, {}) => { // Allow meta evaluating left side of assignment
    const right = propagatedLiteralAst(expectAst(popStack(vm)))
    const left = expectAst(popStack(vm))
    compilerAssert(left instanceof BindingAst, "Expected binding got $left", { left })
    compilerAssert(left.type === right.type, "Type mismatch got $got expected $expected", { got: right.type, expected: left.type })
    vm.stack.push(new SetAst(VoidType, vm.location, left.binding, right))
    return Task.success()
  },
  fieldast: (vm, { name }) => {
    const left = expectAst(popStack(vm))
    const field = left.type.typeInfo.fields.find(x => x.name === name)
    if (!field) return createMethodCall(vm, left, name, [], [])
    if (left instanceof DerefAst && !left.type.typeInfo.isReferenceType) {
      vm.stack.push(new DerefAst(field.fieldType, vm.location, left.left, [...left.fieldPath, field]))
    } else if (left instanceof BindingAst && !left.type.typeInfo.isReferenceType) {
      vm.stack.push(new ValueFieldAst(field.fieldType, vm.location, left, [field]))
    } else if (left instanceof ValueFieldAst && !left.type.typeInfo.isReferenceType) {
      vm.stack.push(new ValueFieldAst(field.fieldType, vm.location, left.left, [...left.fieldPath, field]))
    } else {
      vm.stack.push(new FieldAst(field.fieldType, vm.location, left, field))
    }
  },
  setfieldast: (vm, { name }) => {
    const value = expectAst(popStack(vm));
    const left = expectAst(popStack(vm));
    const field = left.type.typeInfo.fields.find(x => x.name === name)
    compilerAssert(field, "No field $name found on type $type", { name, type: left.type })
    propagateLiteralType(field.fieldType, value)
    compilerAssert(field.fieldType === value.type, "Type $type does not match field $name type of $fieldType on object $objType", { name, objType: left.type, type: value.type, fieldType: field.fieldType })
    if (left instanceof DerefAst && !left.type.typeInfo.isReferenceType) {
      vm.stack.push(new SetDerefAst(VoidType, vm.location, left.left, [...left.fieldPath, field], value))
    } else if (left instanceof ValueFieldAst && !left.type.typeInfo.isReferenceType) {
      vm.stack.push(new SetValueFieldAst(VoidType, vm.location, left.left, [...left.fieldPath, field], value))
    } else if (left instanceof BindingAst && !left.type.typeInfo.isReferenceType) {
      vm.stack.push(new SetValueFieldAst(VoidType, vm.location, left, [field], value))
    } else if (!left.type.typeInfo.isReferenceType) {
      const newBinding = new Binding("", left.type)
      const newBindingAst = new BindingAst(left.type, vm.location, newBinding)
      const stmts = createStatements(vm.location, [
        new LetAst(VoidType, vm.location, newBinding, left),
        new SetValueFieldAst(VoidType, vm.location, newBindingAst, [field], value)
      ])
      vm.stack.push(stmts)
    } else vm.stack.push(new SetFieldAst(VoidType, vm.location, left, field, value))
  },
  subscriptast: (vm, {}) => {
    const right = propagatedLiteralAst(expectAst(popStack(vm)))
    const left = expectAst(popStack(vm))
    if (isParameterizedTypeOf(left.type, ListTypeConstructor)) {
      compilerAssert(isParameterizedTypeOf(left.type, ListTypeConstructor), "Expected list got $x", { x: left.type })
      compilerAssert(right.type === IntType, "Expected int got $x", { x: right.type })
      compilerAssert(left.type instanceof ParameterizedType)
      const elemType = expectType(left.type.args[0])
      vm.stack.push(new SubscriptAst(elemType, vm.location, left, right))
      return
    }
    const subscript = left.type.typeInfo.metaobject['subscript']
    compilerAssert(subscript, "No 'subscript' operator found for $type", { type: left.type })
    return createCallAstFromValueAndPushValue(vm, subscript, [], [left, right])
  },
  staticsubscriptast: (vm, {}) => {
    const right = popStack(vm)
    const left = expectAst(popStack(vm))
    const static_subscript = left.type.typeInfo.metaobject['static_subscript']
    compilerAssert(static_subscript, "No 'static_subscript' operator found for $type", { type: left.type })
    return TaskDef(callFunctionFromValueTask, vm, static_subscript, [], [right, left])
  },
  setsubscriptast: (vm, {}) => {
    const value = propagatedLiteralAst(expectAst(popStack(vm)))
    const right = propagatedLiteralAst(expectAst(popStack(vm)))
    const left = propagatedLiteralAst(expectAst(popStack(vm)))
    const set_subscript = left.type.typeInfo.metaobject['set_subscript']
    compilerAssert(set_subscript, "No 'set_subscript' operator found for $type", { type: left.type })
    return createCallAstFromValueAndPushValue(vm, set_subscript, [], [left, right, value])
  },
  list: (vm, { count }) => vm.stack.push(popValues(vm, count)),
  
  breakast: (vm, { v, named, breakType }) => {
    let expr = v ? propagatedLiteralAst(expectAst(popStack(vm))) : null
    const name = named ? popStack(vm) : null
    if (name && name instanceof LoopObject) { // It's possible to directly pass a loop object to break from
      const block = breakType === 'break' ? name.breakBlock : breakType === 'continue' ? name.continueBlock : (compilerAssert(false, "Invalid breakType", { breakType }) as never)
      compilerAssert(block.binding, "Expected binding")
      vm.stack.push(new BreakAst(NeverType, vm.location, block.binding, expr))
      return
    }
    compilerAssert(!name || name instanceof Binding, "Expected binding", { name })
    let block: LabelBlock
    if (name && name === vm.context.subCompilerState.functionReturnBreakBlock?.binding) 
      block = vm.context.subCompilerState.functionReturnBreakBlock
    else if (name) block = findLabelByBinding(vm.context.subCompilerState.labelBlock, name as Binding)
    else block = findLabelBlockByType(vm.context.subCompilerState.labelBlock, breakType);
    compilerAssert(block.binding, "Expected binding")
    block.didBreak = true
    if (expr?.type === VoidType) {
      // Make sure to keep the expr, but don't use it in break expression
      const break_ = new BreakAst(NeverType, vm.location, block.binding, null)
      const ast = new StatementsAst(VoidType, vm.location, [expr, break_])
      return vm.stack.push(ast)
    }
    if (expr) {
      if (block.type) compilerAssert(block.type === expr.type, "Block type is already inferred to be $blockType but got an expression of type $exprType", { blockType: block.type, exprType: expr.type })
      block.type = expr.type
      block.breakWithExpr = true
    }
    vm.stack.push(new BreakAst(NeverType, vm.location, block.binding, expr))
  },
  bindingast: (vm, { name }) => {
    return (
      TaskDef(resolveScope, vm.scope, name)
      .chainFn((task, value) => {
        if (value instanceof Binding) ensureBindingIsNotClosedOver(vm.context.subCompilerState, name, value);
        if (isPlainObject(value)) vm.stack.push(value)
        else if (value instanceof Module) vm.stack.push(value)
        // else if (value instanceof LoopObject) vm.stack.push(value)
        else if (value instanceof Binding) {
          if (value.storage !== 'ref') vm.stack.push(unknownToAst(vm.location, value))
          else vm.stack.push(new DerefAst(value.type, vm.location, unknownToAst(vm.location, value) as BindingAst, []))
        } else vm.stack.push(unknownToAst(vm.location, value))
        return Task.success()
      })
    )
  },
  return: (vm, { r }) => { 
    const ret = r ? vm.stack[vm.stack.length - 1] : null;
    vm.stack.length = 0
    vm.stack.push(ret);
    vm.ip = vm.bytecode.code.length - 1;
  },
  beginblockast: (vm, { breakType, name }) => {
    const index = vm.context.subCompilerState.nextLabelBlockDepth ++
    const binding = new Binding(`${name ?? ''}_labelbreak${index}`, VoidType);
    vm.scope = createScope({}, vm.scope)
    if (name) vm.scope[name] = binding
    vm.context.subCompilerState.labelBlock = new LabelBlock(vm.context.subCompilerState.labelBlock, null, breakType, binding)
  },
  endblockast: (vm, {}) => {
    const labelBlock = vm.context.subCompilerState.labelBlock;
    vm.context.subCompilerState.nextLabelBlockDepth --
    compilerAssert(labelBlock, "Invalid endblockast")
    const binding = labelBlock.binding
    compilerAssert(binding, "Expected binding", { labelBlock: labelBlock })
    const body = propagatedLiteralAst(expectAst(vm.stack.pop()))
    const blockType = labelBlock.type ?? body.type
    // TODO: This should be NeverType instead of VoidType
    if (labelBlock.type) {
      let blockTypeOk = labelBlock.type === VoidType || labelBlock.type === body.type || body.type === NeverType
      compilerAssert(blockTypeOk, "Block type inferred to be of type $blockType but the result expression was type $bodyType", { blockType: labelBlock.type, bodyType: body.type })
    }
    const breakExprBinding = labelBlock.breakWithExpr ? new Binding(`${binding.name}_breakexpr`, body.type) : null
    vm.context.subCompilerState.labelBlock = labelBlock.parent
    compilerAssert(vm.scope[ScopeParentSymbol], "Expected parent scope")
    vm.scope = vm.scope[ScopeParentSymbol]
    vm.stack.push(new BlockAst(blockType, vm.location, binding, breakExprBinding, body))
  },

  binding: (vm, { name }) => {
    return TaskDef(resolveScope, vm.scope, name).chainFn((task, res) => {
      vm.stack.push(res)
      return Task.of(1)
    })
  },
  totype: (vm, {}) => {
    const type = popStack(vm);
    if (isType(type)) return vm.stack.push(type);
    if (type instanceof TypeVariable) return vm.stack.push(type)
    if (type instanceof TypeMatcher) return vm.stack.push(type)
    if (type instanceof Tuple) {
      return (
        createParameterizedExternalType(vm.context.globalCompiler, TupleTypeConstructor, type.values)
        .chainFn((task, type) => { vm.stack.push(type); return Task.success() })
      )
    }
    if (type instanceof ClassDefinition) {
      if (type.concreteType) return vm.stack.push(type.concreteType)
      return (
        TaskDef(compileClassTask, { classDef: type, typeArgs: [] })
        .chainFn((task, res) => { vm.stack.push(res); return Task.success() })
      );
    }
    compilerAssert(false, "Expected Type got $type", { type })
  },
  
  pop: (vm) => vm.stack.pop(),
  jumpf: (vm, { address }) => {
    if (!vm.stack.pop()) vm.ip = address;
  },
  letlocal: (vm, { name }) => {
    // Always set on the scope of the current function for now. Not sure if this is the best idea
    compilerAssert(vm.context.subCompilerState.functionCompiler, "Expected function compiler")
    const scope = vm.context.subCompilerState.functionCompiler.scope
    compilerAssert(!Object.hasOwn(scope, name), `$name is already in scope`, { name });
    setScopeValueAndResolveEvents(scope, name, popStack(vm))
    vm.stack.push(null) // statement expression
  },
  setlocal: (vm, { name }) => {
    compilerAssert(vm.context.subCompilerState.functionCompiler, "Expected function compiler")
    const scope = vm.context.subCompilerState.functionCompiler.scope
    compilerAssert(Object.hasOwn(scope, name), `$name does not exist in scope`, { name });
    scope[name] = popStack(vm);
    vm.stack.push(null) // statement expression
  },
  jump: (vm, { address }) => void (vm.ip = address),
  call: (vm, { name, count, tcount }) => {
    const values = popValues(vm, count);
    const typeArgs = popValues(vm, tcount || 0);
    return (
      TaskDef(resolveScope, vm.scope, name)
      .chainFn((task, func) => {
        return TaskDef(callFunctionFromValueTask, vm, func, typeArgs, values)
      })
    )
  },
  callobj: (vm, { count, tcount }) => {
    const callable = popStack(vm)
    const values = popValues(vm, count);
    const typeArgs = popValues(vm, tcount || 0);
    return TaskDef(callFunctionFromValueTask, vm, callable, typeArgs, values)
  },

  compilerfn: (vm, { name, count, tcount }) => {
    const values = popValues(vm, count)
    const typeArgs = popValues(vm, tcount || 0);

    if (name === 'iteratefn') {
      const iterator = expectAst(values[0])
      // compilerAssert(false, "", { iterator })
      // console.log('iterator', iterator, typeArgs[0])
      if (iterator instanceof CompTimeObjAst) {
        compilerAssert(isCompilerCallable(iterator.value), "Expected function")
        const iterateAst = new CompTimeObjAst(CompileTimeObjectType, vm.location, typeArgs[0])
        return createCallAstFromValueAndPushValue(vm, iterator.value, [], [iterateAst])
      }
      const metaObject = iterator.type.typeInfo.metaobject;
      if (metaObject['iterate']) return createCallAstFromValueAndPushValue(vm, metaObject['iterate'], [typeArgs[0]], [iterator])
      return createMethodCall(vm, iterator, 'iterate', [typeArgs[0]], [])
    }
    if (name === 'lenfn') {
      const expr = expectAst(values[0])
      const metaObject = expr.type.typeInfo.metaobject;
      if (metaObject['length']) return createCallAstFromValueAndPushValue(vm, metaObject['length'], [], [expr])
      return createMethodCall(vm, expr, 'length', [], [])
    }

    compilerAssert(false, "No such compiler function $name", { name, values, typeArgs })
  },
  operator: (vm, { name, count }) => {
    const values = popValues(vm, count);
    compilerAssert(typeof values[0] === 'number', "Expected number got $v", { v: values[0] })
    compilerAssert(typeof values[1] === 'number', "Expected number got $v", { v: values[1] })
    compilerAssert(operators[name], `Invalid operator $name`, { name });
    const operatorResult = operators[name].comptime(values[0], values[1]);
    vm.stack.push(operatorResult);
  },
  not: (vm, {}) => void vm.stack.push(!popStack(vm)),
  closure: (vm, { id }) => {
    compilerAssert(typeof id === 'number')
    const globalCompiler = (vm.context as TaskContext).globalCompiler
    compilerAssert(id in globalCompiler.functionDefinitions, "Not found in func $id", { id })
    const func = globalCompiler.functionDefinitions[id];
    const closure = new Closure(func, vm.scope, vm.context.subCompilerState);
    vm.stack.push(closure);
    if (func.name && !func.keywords.includes('method')) {
      compilerAssert(!Object.hasOwn(vm.scope, func.name.token.value), "$name already in scope", { name: func.name.token, value: vm.scope[func.name.token.value] })
      vm.scope[func.name.token.value] = closure;
    }
  },
  tuple: (vm, { count }) => vm.stack.push(new Tuple(popValues(vm, count))),
  dict: (vm, { count }) => {
    const dict: UnknownObject = {}
    for (let i = 0; i < count; i++) {
      const value = popStack(vm)
      const key = popStack(vm)
      compilerAssert(typeof key === 'string', "Expected string key")
      dict[key] = value;
    }
    vm.stack.push(dict)
  },
  dictast: (vm, {}) => compilerAssert(false, "Not implemented 'dictast'"),
  tupleast: (vm, { count }) => {
    let values = expectAsts(popValues(vm, count))
    const argTypes = values.map(x => x.type)
    return (
      createParameterizedExternalType(vm.context.globalCompiler, TupleTypeConstructor, argTypes)
      .chainFn((task, type) => {
        values = values.map(x => propagatedLiteralAst(x))
        vm.stack.push(new ConstructorAst(type, vm.location, values))
        return Task.success()
      })
    )
  },
  pushqs: (vm) => vm.context.subCompilerState.quoteStack.push([]),
  popqs: (vm) => {
    compilerAssert(vm.context.subCompilerState.quoteStack.length)
    const stmts = expectAll(isAst, vm.context.subCompilerState.quoteStack.pop()!);
    vm.stack.push(createStatements(vm.location, stmts))
  },
  appendq: (vm) => {
    const value = expectAst(vm.stack.pop());
    const compilerState = vm.context.subCompilerState;
    compilerState.quoteStack[compilerState.quoteStack.length - 1].push(value);
    vm.stack.push(null); // needed for statements
  },
};

const ensureBindingIsNotClosedOver = (subCompilerState: SubCompilerState, name: string, value: Binding) => {
  if (!value.definitionCompiler) compilerAssert(false, "Binding has no definition")
  if (value.definitionCompiler.moduleCompiler.scope === value.definitionCompiler.scope) return true // Global

  const found = (() => {
    let compiler: SubCompilerState | undefined = subCompilerState
    while (compiler) {
      if (compiler === value.definitionCompiler) return true;
      compiler = compiler.inlineIntoCompiler
    }
  })()
  compilerAssert(found, "Name $name is declared in an external function which isn't supported in this compiler. You might want to use an inline function", { name, found, subCompilerState })
}

function executeVmTask(ctx: TaskContext, { vm } : { vm: Vm }, p: void): Task<Unit, CompilerError> {
  const {locations, code} = vm.bytecode;
  let current = code[vm.ip];
  vm.location = locations[vm.ip];
  compilerAssert(current, "Expected 'halt' instruction")
  while (current.type !== "halt") {
    const startIp = vm.ip;
    const instr = instructions[current.type] as (vm: Vm, instr: BytecodeInstr) => void | Task<unknown, CompilerError>;
    compilerAssert(instr, "Not inplemented yet instruction $type", { type: current.type, current })
    let res : void | Task<unknown, CompilerError>;
    try {
      res = instr(vm, current);
    } catch(ex) {
      if (ex instanceof CompilerError) {
        if (!ex.info) ex.info = {}
        Object.assign(ex.info, { ip: vm.ip, current, location: vm.location, subCompilerState: vm.context.subCompilerState }, ex.info)
      }
      throw ex;
    }

    if (isTaskResult(res) || !isTask(res)) {
      if (vm.ip === startIp) vm.ip++;
      current = code[vm.ip];
      vm.location = locations[vm.ip];
    } else {
      return res.chainFn(() => {
        if (vm.ip === startIp) vm.ip++;
        current = code[vm.ip];
        vm.location = locations[vm.ip];

        return TaskDef(executeVmTask, { vm })
      })
    }
  }
  return Task.success()
};

const setScopeValueAndResolveEvents = (scope: Scope, name: string, value: unknown) => {
  // TODO: Make sure metagenerated names don't shadow something because this won't work
  // To do that might be difficult because the shadow might compile before the thing being shadowed.
  // Can we enforce outer scopes are compiled first?
  scope[name] = value;
  const events = scope[ScopeEventsSymbol];
  if (events && events[name]) {
    (events[name] as Event<unknown, unknown>).success(value);
    delete events[name]
  }
}

export function compileClassTask(ctx: TaskContext, { classDef, typeArgs }: { classDef: ClassDefinition, typeArgs: unknown[] }): Task<ConcreteClassType | ParameterizedType, CompilerError> {
  const binding = new Binding(classDef.debugName, VoidType);
  const body = null as any
  compilerAssert(typeArgs.length === classDef.typeArgs.length, "Expected $x type parameters for class $classDef, got $y", { x: classDef.typeArgs.length, y: typeArgs.length, classDef })
  
  if (!classDef.templatePrototype)  {
    compilerAssert(classDef.body, "Expected class body");
    classDef.templatePrototype = { name: `${classDef.debugName} class template bytecode`, body: classDef.body, initialInstructionTable: BytecodeSecondOrder }; 
    compileFunctionPrototype(ctx, classDef.templatePrototype);
  }

  const typeParamHash = hashValues(typeArgs, { classDef })
  const existing = classDef.compiledClasses.find(compiledClass => {
    if (compiledClass.typeArgHash === typeParamHash) {
      if (compiledClass.typeArguments.every((x, i) => x === typeArgs[i])) return true
    }
  })
  if (existing) return Task.of(existing.type);

  const templateScope = Object.create(classDef.parentScope);
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${classDef.debugName} class template`, lexicalParent: ctx.subCompilerState, scope: templateScope })
  subCompilerState.functionCompiler = subCompilerState; // Consider class as the functionCompiler - could be renamed something else
  
  classDef.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    templateScope[typeArg.token.value] = typeArgs[i];
  });

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, classDef.templatePrototype!.bytecode!, templateScope)
    .chainFn((task, ast) => {
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      const debugName = typeArgs.length === 0 ? classDef.debugName :
        `${classDef.debugName}!(...)`
      const compiledClass = new CompiledClass(
          classDef.location, debugName,
          binding, classDef, null!, body, [], typeArgs, typeParamHash)

      const typeInfo: TypeInfo = { sizeof: 0, fields: compiledClass.fields, metaobject: compiledClass.metaobject, isReferenceType: true }
      let type: Type
      if (classDef.typeArgs.length === 0) { 
        type = new ConcreteClassType(compiledClass, typeInfo)
        classDef.concreteType = type;
      } else {
        type = ctx.globalCompiler.typeTable.getOrInsert(new ParameterizedType(classDef, typeArgs, typeInfo))
      }
      compiledClass.type = type;
      binding.type = type;

      let index = 0
      for (const name of Object.getOwnPropertyNames(templateScope)) {
        if (templateScope[name] instanceof Binding)
          compiledClass.fields.push(new TypeField(SourceLocation.anon, name, type, index++, templateScope[name].type))
      }

      classDef.compiledClasses.push(compiledClass)

      const returnType = type;
      const definitionScope = classDef.parentScope
      
      if (classDef.metaClass) {
        return (
          TaskDef(resolveScope, classDef.parentScope, classDef.metaClass.token.value)
          .chainFn((task, func) => {
            if (func instanceof ExternalFunction) {
              const fnctx: CompilerFunctionCallContext = { location: SourceLocation.anon, compilerState: ctx.subCompilerState, resultAst: undefined, typeCheckResult: undefined }
              func.func(fnctx, [compiledClass])
            } else compilerAssert(false, "Not implemented yet", { func })
            defaultMetaFunction(subCompilerState, compiledClass, definitionScope, templateScope)
            return Task.of(returnType)
          })
        )
      }
      
      defaultMetaFunction(subCompilerState, compiledClass, definitionScope, templateScope)

      return Task.of(returnType)

    })
  )
  
}

export const getCommonType = (types: Type[]): Type => {
  if (types.some(x => x === FloatLiteralType || x === FloatType)) {
    compilerAssert(types.every(x => x === IntLiteralType || x === FloatLiteralType || x === FloatType), "Expected types to be the same for list literal", { types })
    return FloatType
  }
  if (types.some(x => x === IntLiteralType || x === IntType)) {
    compilerAssert(types.every(x => x === IntLiteralType || x === IntType), "Expected types to be the same for list literal", { types })
    return IntType
  }
  compilerAssert(types.every(x => x === types[0]), "Expected types to be the same")
  return types[0];
}
export const createParameterizedExternalType = (globalCompiler: GlobalCompilerState, typeConstructor: ExternalTypeConstructor, argTypes: unknown[]): Task<Type, CompilerError> => {
  const newArgTypes = argTypes.map(value => {
    // TODO: Do this more rigorously?
    if (isType(value)) {
      if (value === IntLiteralType) return IntType
      if (value === FloatLiteralType) return FloatType
      return value;
    }
    if (value instanceof ClassDefinition && value.concreteType) return value.concreteType
    if (value instanceof Tuple) compilerAssert(false, "Not implemented yet")
    compilerAssert(false, "Expected types got $expected", { value }); 
  })
  return (
    typeConstructor.createType(globalCompiler, newArgTypes)
    .chainFn((task, type) => {
      return Task.of(globalCompiler.typeTable.getOrInsert(type))
    })
  )
}

function topLevelFunctionDefinitionTask(ctx: TaskContext, funcDecl: ParserFunctionDecl, scope: Scope) {
  const funcDef = insertFunctionDefinition(ctx.globalCompiler, funcDecl)

  if (funcDef.keywords.includes('method')) {
    compilerAssert(funcDecl.params[0]?.name, "Expected type for first argument")
    let t = funcDecl.params[0].type!;
    const type = t instanceof ParseCall ? t.left : t
    return (
      TaskDef(resolveScope, scope, type.token.value)
      .chainFn((task, result) => {
        let methods = ctx.globalCompiler.methods.get(scope)
        if (!methods) { methods = []; ctx.globalCompiler.methods.set(scope, methods) }
        compilerAssert(result instanceof ClassDefinition || result instanceof ExternalTypeConstructor, "Expected class definition or type constructor, got $result", { result })

        methods.push([result, new Closure(funcDef, scope, ctx.subCompilerState)])
        return Task.success()
      })
    )
    
  }

  compilerAssert(!Object.hasOwn(scope, funcDef.name!.token.value), "$name already in scope", { name: funcDef.name!.token.value, value: scope[funcDef.name!.token.value] })

  setScopeValueAndResolveEvents(scope, funcDef.name!.token.value, new Closure(funcDef, scope, ctx.subCompilerState))

  return Task.success()
}
function topLevelClassDefinitionTask(ctx: TaskContext, decl: ParserClassDecl, scope: Scope) {

  const g = ctx.globalCompiler
  if (decl.id !== undefined) return Task.success();

  decl.id = g.functionDefinitions.length;
  const classDef = new ClassDefinition(
    decl.id, decl.token.location, scope, decl.debugName,
    decl.name, decl.typeArgs, decl.body)
  classDef.metaClass = decl.metaType
  const keywords = decl.keywords.map(x => x instanceof ParseNote ? x.expr.token.value : x.token.value)
  classDef.keywords.push(...keywords)

  g.classDefinitions.push(classDef);

  setScopeValueAndResolveEvents(scope, decl.name!.token.value, classDef)

  return Task.success()

}

const topLevelLetConst = (ctx: TaskContext, expr: ParseLetConst, rootScope: Scope) => {
  const out: BytecodeWriter = {
    location: expr.token.location,
    bytecode: { code: [], locations: [] },
    instructionTable: BytecodeDefault,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNodeAndError(out, expr.value);
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan("Compiled top level let const"))
  ctx.globalCompiler.logger.log(bytecodeToString(out.bytecode))
  ctx.globalCompiler.logger.log("")

  const subCompilerState = pushSubCompilerState(ctx, { debugName: 'top level const', lexicalParent: ctx.subCompilerState, scope: ctx.subCompilerState.scope })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, out.bytecode, rootScope)
    .chainFn((task, result) => {
      setScopeValueAndResolveEvents(rootScope, expr.name.token.value, result)
      return Task.success()
    })
  );
}

const topLevelLet = (ctx: TaskContext, expr: ParseLet, moduleScope: Scope) => {
  if (!expr.value) return Task.success()

  const out: BytecodeWriter = {
    location: expr.token.location,
    bytecode: { code: [], locations: [] },
    instructionTable: BytecodeSecondOrder,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNodeAndError(out, expr)
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan("Compiled top level let"))
  ctx.globalCompiler.logger.log(bytecodeToString(out.bytecode))
  ctx.globalCompiler.logger.log("")

  const subCompilerState = pushSubCompilerState(ctx, { debugName: 'top level let', lexicalParent: ctx.subCompilerState, scope: ctx.subCompilerState.scope })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, out.bytecode, moduleScope)
    .chainFn((task, result) => {
      compilerAssert(result instanceof LetAst, "Expected let ast")
      ctx.globalCompiler.globalLets.push(result)
      return Task.success()
    })
  );
}

export const loadModule = (ctx: TaskContext, location: SourceLocation, moduleName: string): Task<Module, CompilerError> => {
  const loader = ctx.globalCompiler.moduleLoader
  if (loader.cache[moduleName]) return Task.of(loader.cache[moduleName])
  const rootScope = ctx.globalCompiler.rootScope
  const parsedModule = loader.loadModule(moduleName)
  const moduleScope = createScope({ ...rootScope }, undefined)

  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${moduleName} module`, lexicalParent: undefined, scope: moduleScope })
  subCompilerState.moduleCompiler = subCompilerState
  ;(subCompilerState as any).location = location

  return (
    TaskDef(runTopLevelTask, parsedModule.rootNode, moduleScope)
    .chainFn((task, _) => {
      const module = new Module(moduleName, subCompilerState, parsedModule)
      loader.cache[moduleName] = module
      return Task.of(module)
    }) as Task<Module, CompilerError>
  )
}

export const importModule = (ctx: TaskContext, importNode: ParseImport, existingScope: Scope) => {
  const moduleName = importNode.module.token.value
  const loader = ctx.globalCompiler.moduleLoader

  const expandIntoScope = (module: Module) => {
    if (importNode.imports.length === 0) {
      const newName = importNode.rename ? importNode.rename.token.value : importNode.module.token.value
      setScopeValueAndResolveEvents(existingScope, newName, module)
    }

    const tasks = importNode.imports.map((importName) => {
      const originalName = importName.token.value
      const newName = importName.rename ? importName.rename.token.value : originalName
      const insertIntoScope = (task: unknown, result: unknown) => {
        setScopeValueAndResolveEvents(existingScope, newName, result)
        return Task.success()
      }
      return TaskDef(resolveScope, module.compilerState.scope, originalName).chainFn(insertIntoScope)
    })
    
    return Task.concurrency(tasks)
  }
  return (
    TaskDef(loadModule, importNode.token.location, moduleName)
    .chainFn((task, module) => {
      ctx.subCompilerState = loader.cache[moduleName].compilerState
      return expandIntoScope(module)
    })
  )
}

export const topLevelComptimeTask = (ctx: TaskContext, expr: ParseNode, moduleScope: Scope) => {
  const out: BytecodeWriter = {
    location: expr.token.location,
    bytecode: { code: [], locations: [] },
    instructionTable: BytecodeDefault,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNodeAndError(out, expr)
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan("Compiled top level comptime"))
  ctx.globalCompiler.logger.log(bytecodeToString(out.bytecode))
  ctx.globalCompiler.logger.log("")

  const subCompilerState = pushSubCompilerState(ctx, { debugName: 'top level comptime', lexicalParent: ctx.subCompilerState, scope: moduleScope })

  return (
    TaskDef(createBytecodeVmAndExecuteTask, subCompilerState, out.bytecode, moduleScope)
  );

}

export const runTopLevelTask = (ctx: TaskContext, stmts: ParseStatements, moduleScope: Scope) => {
  const tasks: Task<unknown, CompilerError>[] = []

  const copyEverythingIntoScope = (module: Module) => {
    Object.getOwnPropertyNames(module.compilerState.scope).forEach((k) => {
      const v = module.compilerState.scope[k]
      // TODO: All of rootScope is also copied here
      setScopeValueAndResolveEvents(moduleScope, k, v)
    })

    if (ctx.globalCompiler.methods.get(module.compilerState.scope)) {
      const methods = [...ctx.globalCompiler.methods.get(module.compilerState.scope)!]
      ctx.globalCompiler.methods.set(moduleScope, methods)
    }
  }
  const loader = ctx.globalCompiler.moduleLoader
  if (loader.cache["_preload"] && loader.cache["_preload"].compilerState.scope != moduleScope)
    copyEverythingIntoScope(loader.cache['_preload'])
  
  stmts.exprs.forEach(node => {
    if (node.key === 'import') {
      tasks.push(TaskDef(importModule, node, moduleScope));
    } else if (node.key === 'letconst') {
      tasks.push(TaskDef(topLevelLetConst, node, moduleScope));
    } else if (node.key === 'let') {
      tasks.push(TaskDef(topLevelLet, node, moduleScope));
    } else if (node.key === 'function') {
      tasks.push(TaskDef(topLevelFunctionDefinitionTask, node.functionDecl, moduleScope ));
    } else if (node.key === 'class') {
      tasks.push(TaskDef(topLevelClassDefinitionTask, node.classDecl, moduleScope ));
    } else if (node.key === 'comptime') {
      tasks.push(TaskDef(topLevelComptimeTask, node.expr, moduleScope ));
    } else {
      compilerAssert(false, `Not supported at top level $key`, { key: node.key })
    }
  })

  return Task.concurrency(tasks);
}

const createInitializerFunctionTask = (ctx: TaskContext) => {
  const decl: ParserFunctionDecl = {
    debugName: `<initializer>`,
    token: createAnonymousToken(''), functionMetaName: null, name: null, typeParams: [], params: [],
    keywords: [], anonymous: true, returnType: null, body: null, annotations: [], variadic: false
  }
  const func = insertFunctionDefinition(ctx.globalCompiler, decl)

  // Map initializers 
  const lets = ctx.globalCompiler.globalLets.map(globalLet => {
    const value = globalLet.value || new DefaultConsAst(globalLet.binding.type, globalLet.location)
    return new SetAst(VoidType, globalLet.location, globalLet.binding, value)
  })
  const ast = new StatementsAst(VoidType, SourceLocation.anon, [...lets])

  const binding = ctx.globalCompiler.initializerFunctionBinding
  const compiledFunction = new CompiledFunction(
      binding, func, VoidType, [], ast, [], [], 0)
  ctx.globalCompiler.compiledFunctions.set(binding, compiledFunction)
  func.compiledFunctions.push(compiledFunction)
  ctx.globalCompiler.initializerFunction = compiledFunction

  return Task.success()
}

const createEntryFunctionTask = (ctx: TaskContext) => {
  const decl: ParserFunctionDecl = {
    debugName: `<entry>`,
    token: createAnonymousToken(''), functionMetaName: null, name: null, typeParams: [], params: [],
    keywords: [], anonymous: true, returnType: null, body: null, annotations: [], variadic: false
  }
  const func = insertFunctionDefinition(ctx.globalCompiler, decl)

  compilerAssert(ctx.globalCompiler.mainFunction, "Expected main function")

  // Call initializer and main
  const ast = new StatementsAst(VoidType, SourceLocation.anon, [
    new UserCallAst(VoidType, SourceLocation.anon, ctx.globalCompiler.initializerFunctionBinding, []),
    new UserCallAst(VoidType, SourceLocation.anon, ctx.globalCompiler.mainFunction.binding, []),
  ])

  const id = func.compiledFunctions.length
  const binding = new Binding(`${func.debugName} compiled ${id}`, FunctionType)
  const compiledFunction = new CompiledFunction(
      binding, func, IntType, [], ast, [], [], 0)
  ctx.globalCompiler.compiledFunctions.set(binding, compiledFunction)
  func.compiledFunctions.push(compiledFunction)
  ctx.globalCompiler.entryFunction = compiledFunction
  compilerAssert(!ctx.globalCompiler.exports['main'], "Already got main export")
  ctx.globalCompiler.exports['main'] = compiledFunction

  return Task.success()
}

export const programEntryTask = (ctx: TaskContext, entryModule: ParsedModule): Task<unknown, CompilerError> => {
  const moduleScope = ctx.subCompilerState.scope

  return (
    TaskDef(createCompilerModuleTask)
    .chainFn((task, compilerModule) => {
      ctx.globalCompiler.moduleLoader.cache['compiler'] = compilerModule
      return TaskDef(loadModule, SourceLocation.anon, '_preload')
    })
    .chainFn(() => {
      return TaskDef(runTopLevelTask, entryModule.rootNode, moduleScope)
    })
    .chainFn((task, arg) => {

      const tasks: Task<unknown, CompilerError>[] = []
      
      if (moduleScope['main']) {
        compilerAssert(moduleScope['main'] instanceof Closure, "Expected main to be callable")

        const task = TaskDef(compileExportedFunctionTask, { closure: moduleScope['main'] })
          .chainFn((task, compiledFunction) => {
            ctx.globalCompiler.mainFunction = compiledFunction
            return TaskDef(createEntryFunctionTask)
          })
        
        tasks.push(task)
      }
      Object.values(moduleScope).forEach(value => {
        if (value instanceof Closure && value.func.keywords.includes("export")) {
          const exportName = value.func.externalName = value.func.debugName // TODO: Don't call this debugName
          tasks.push(TaskDef(compileExportedFunctionTask, { exportName, closure: value }))
        }
      })
      compilerAssert(tasks.length, "No 'main' and no exported function found")
      return Task.concurrency(tasks)
    })
    .chainFn(() => {
      return TaskDef(createInitializerFunctionTask)
    })
  )
  
}

export const generateCompileCommands = (globalCompiler: GlobalCompilerState) => {
  const opts = globalCompiler.externalCompilerOptions
  const globalOptions = opts.globalOptions
  const libs = opts.libraries.map(x => `-l${x}`).join(" ")
  const frameworks = opts.macosFrameworks.map(x => `-framework ${x}`).join(" ")
  const addLibraryDirs = globalOptions.libraryDirs.map(x => `-L${x}`).join(" ")
  const llcPath = globalOptions.llcPath
  const llPath = opts.llPath
  const nativePath = opts.nativePath
  const assemblyPath = opts.assemblyPath
  const clang = globalOptions.clangPath
  return {
    compile: `${llcPath} ${llPath} -O3 -o ${assemblyPath}`,
    compileAndLink: `${clang} ${llPath} -O3 -o ${nativePath} ${addLibraryDirs} ${libs} ${frameworks}`,
    nativePath
  }
}