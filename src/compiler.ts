import { isParseVoid, BytecodeWriter, FunctionDefinition, Type, Binding, LetAst, UserCallAst, CallAst, Ast, NumberAst, OperatorAst, SetAst, OrAst, AndAst, ListAst, IfAst, StatementsAst, Scope, createScope, Closure, ExternalFunction, compilerAssert, VoidType, IntType, FunctionPrototype, Vm, ParseTreeTable, Token, createStatements, DoubleType, FloatType, StringType, expectMap, bytecodeToString, ParseCall, ParseIdentifier, ParseNode, CompiledFunction, AstRoot, isAst, pushSubCompilerState, ParseNil, createToken, ParseStatements, FunctionType, StringAst, WhileAst, BoolAst, BindingAst, SourceLocation, BytecodeInstr, ReturnAst, ParserFunctionDecl, ScopeEventsSymbol, BoolType, Tuple, ParseTuple, hashValues, TaskContext, ParseElse, ParseIf, InstructionMapping, GlobalCompilerState, expectType, expectAst, expectAll, expectAsts, BreakAst, LabelBlock, BlockAst, findLabelBlockByType, ParserClassDecl, ClassDefinition, isType, CompiledClass, ConcreteClassType, FieldAst, ParseField, SetFieldAst, CompilerError, VoidAst, SubCompilerState, ParseLetConst, PrimitiveType, CastAst, ParseFunction, ListTypeConstructor, SubscriptAst, ExternalTypeConstructor, ParameterizedType, isParameterizedTypeOf, ParseMeta, createAnonymousParserFunctionDecl, ArgumentTypePair, NotAst, BytecodeProgram, ParseImport, createCompilerError, createAnonymousToken, textColors, ParseCompilerIden, TypeField, ParseValue, ParseConstructor, ConstructorAst, TypeVariable, TypeMatcher, TypeConstructor, TypeInfo, TupleTypeConstructor, ParsedModule, Module, ParseSymbol, ScopeParentSymbol, isPlainObject, ParseLet, ParseList, ParseExpand, ParseBlock, findLabelByBinding, ParseSubscript, ParseNumber, ParseQuote, ParseWhile, ParseOperator, ParseBytecode, ParseOpEq, ParseSet, ParseFreshIden, UnknownObject, ParseNote, DefaultConsAst, RawPointerType } from "./defs";
import { CompileTimeFunctionCallArg, FunctionCallArg, insertFunctionDefinition, functionCompileTimeCompileTask, createCallAstFromValue, createCallAstFromValueAndPushValue, createMethodCall } from "./compiler_functions";
import { Event, Task, TaskDef, Unit, isTask, isTaskResult, withContext } from "./tasks";
import { defaultMetaFunction, expandLoopSugar, foldSugar, forLoopSugar, listComprehensionSugar, sliceSugar } from "./compiler_sugar";

export const pushBytecode = <T extends BytecodeInstr>(out: BytecodeWriter, token: Token, instr: T) => {
  out.bytecode.locations.push(token.location);
  out.bytecode.code.push(instr)
  return instr;
}

export const visitParseNode = (out: BytecodeWriter, expr: ParseNode) => {
  compilerAssert(expr.key, "$expr not found", { expr })
  const table = out.instructionTable
  const instrWriter = expectMap(table, expr.key, `Not implemented parser node $key in ${table === BytecodeDefault ? 'default' : 'second order'} table`)
  instrWriter(out, expr as any)
}
export const visitAll = (out: BytecodeWriter, exprs: ParseNode[]) => {
  exprs.forEach(expr => visitParseNode(out, expr))
}
export const writeMeta = (out: BytecodeWriter, expr: ParseNode) => {
  visitParseNode({ bytecode: out.bytecode, instructionTable: BytecodeDefault, globalCompilerState: out.globalCompilerState, state: out.state }, expr)
}
export const pushGeneratedBytecode = <T extends BytecodeInstr>(out: BytecodeWriter, instr: T) => {
  out.bytecode.code.push(instr);
  out.bytecode.locations.push(new SourceLocation(-1, -1, null!));
  return instr;
}

export const BytecodeDefault: ParseTreeTable = {
  cast:      (out, node) => compilerAssert(false, "Not implemented 'cast' in BytecodeDefault"),
  forexpr:   (out, node) => compilerAssert(false, "Not implemented 'forexpr' in BytecodeDefault"),
  whileexpr: (out, node) => compilerAssert(false, "Not implemented 'whileexpr' in BytecodeDefault"),
  expand:    (out, node) => compilerAssert(false, "Not implemented 'expand' in BytecodeDefault"),
  
  note:      (out, node) => compilerAssert(false, "Not implemented 'note' in BytecodeDefault"),
  slice:     (out, node) => compilerAssert(false, "Not implemented 'slice' in BytecodeDefault"),
  class:     (out, node) => compilerAssert(false, "Not implemented 'class' in BytecodeDefault"),
  metafor:   (out, node) => compilerAssert(false, "Not implemented 'metafor' in BytecodeDefault"),
  for:       (out, node) => compilerAssert(false, "Not implemented 'for' in BytecodeDefault"),
  opeq:      (out, node) => compilerAssert(false, "Not implemented 'opeq' in BytecodeDefault"),
  import:    (out, node) => compilerAssert(false, "Not implemented 'import' in BytecodeDefault"),
  bytecode:  (out, node) => compilerAssert(false, "Not implemented 'bytecode' in BytecodeDefault"),
  constructor: (out, node) => compilerAssert(false, "Not implemented 'constructor' in BytecodeDefault"),
  compileriden: (out, node) => compilerAssert(false, "Not implemented 'compileriden' in BytecodeDefault"),

  value:   (out, node) => pushBytecode(out, node.token, { type: "push", value: node.value }), 
  number:  (out, node) => pushBytecode(out, node.token, { type: "push", value: Number(node.token.value) }), 
  string:  (out, node) => pushBytecode(out, node.token, { type: "push", value: node.token.value }), 
  nil:     (out, node) => pushBytecode(out, node.token, { type: "push", value: null }), 
  boolean: (out, node) => pushBytecode(out, node.token, { type: "push", value: node.token.value !== 'false' }), 
  list:    (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'list', count: node.exprs.length })),
  tuple:   (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'tuple', count: node.exprs.length })),
  symbol:  (out, node) => pushBytecode(out, node.token, { type: "push", value: node.token.value }),

  freshiden:  (out, node) => pushBytecode(out, node.token, { type: "binding", name: node.freshBindingToken.identifier }),
  identifier: (out, node) => pushBytecode(out, node.token, { type: "binding", name: node.token.value }), 
  operator:   (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'operator', name: node.token.value, count: node.exprs.length })), 
  set:        (out, node) => (visitParseNode(out, node.value), pushBytecode(out, node.token, { type: 'setlocal', name: node.left.token.value })), 
  letconst:   (out, node) => (visitParseNode(out, node.value), pushBytecode(out, node.token, { type: 'letlocal', name: node.name.token.value, t: false, v: true })), 
  meta:       (out, node) => (visitParseNode(out, node.expr)),
  comptime:   (out, node) => (visitParseNode(out, node.expr)),
  not:        (out, node) => (visitParseNode(out, node.expr), pushBytecode(out, node.token, { type: 'not' })),

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

  quote: (out, node) => {
    visitParseNode({ bytecode: out.bytecode, instructionTable: BytecodeSecondOrder, globalCompilerState: out.globalCompilerState, state: out.state }, node.expr)
  },
  
  let: (out, node) => {
    if (node.value) visitParseNode(out, node.value);
    if (node.type) {
      writeMeta(out, node.type);
      pushBytecode(out, node.type.token, { type: 'totype' });
    }
    const name = node.name instanceof ParseFreshIden ? node.name.freshBindingToken.identifier : node.name.token.value
    pushBytecode(out, node.token, { type: 'letlocal', name, t: !!node.type, v: !!node.value }) 
  },

  function: (out, node) => {
    pushBytecode(out, node.token, { type: "closure", id: insertFunctionDefinition(out.globalCompilerState, node.functionDecl).id }) 
  },
  call: (out, node) => {
    visitAll(out, node.typeArgs)
    visitAll(out, node.args);
    if (node.left instanceof ParseIdentifier) {
      pushBytecode(out, node.token, { type: "call", name: node.left.token.value, count: node.args.length, tcount: node.typeArgs.length }); 
      return;
    }
    if (node.left instanceof ParseCompilerIden) {
      pushBytecode(out, node.token, { type: "compilerfn", name: node.left.value, count: node.args.length, tcount: node.typeArgs.length });
      return;
    }
    if (node.left instanceof ParseField) {
      compilerAssert(false, "Call with field not implemented yet")
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
    if (node.expr) visitParseNode(out, node.expr);
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
  cast:      (out, node) => compilerAssert(false, "Not implemented 'cast'"),
  forexpr:   (out, node) => compilerAssert(false, "Not implemented 'forexpr'"),
  whileexpr: (out, node) => compilerAssert(false, "Not implemented 'whileexpr'"),
  symbol:    (out, node) => compilerAssert(false, "Not implemented 'symbol'"),
  note:      (out, node) => compilerAssert(false, "Not implemented 'note'"),
  class:     (out, node) => compilerAssert(false, "Not implemented 'class'"),
  nil:       (out, node) => compilerAssert(false, "Not implemented 'nil'"),
  metafor:   (out, node) => compilerAssert(false, "Not implemented 'metafor'"),
  import:    (out, node) => compilerAssert(false, "Not implemented 'import'"),
  quote:     (out, node) => compilerAssert(false, "Not implemented 'quote'"),
  compileriden: (out, node) => compilerAssert(false, "Not implemented 'compileriden'"),

  constructor:  (out, node) => (visitParseNode(out, node.type), visitAll(out, node.args), pushBytecode(out, node.token, { type: 'constructorast', count: node.args.length })),
  identifier:   (out, node) => pushBytecode(out, node.token, { type: "bindingast", name: node.token.value }),

  value:    (out, node) => pushBytecode(out, node.token, { type: "push", value: node.value }), 
  number:   (out, node) => pushBytecode(out, node.token, { type: "numberast", value: Number(node.token.value) }),
  string:   (out, node) => pushBytecode(out, node.token, { type: "stringast", value: node.token.value }),
  boolean:  (out, node) => pushBytecode(out, node.token, { type: "boolast", value: node.token.value !== 'false' }),

  operator: (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'operatorast', name: node.token.value, count: node.exprs.length })),
  meta:     (out, node) => (writeMeta(out, node.expr), pushBytecode(out, node.token, { type: 'toast' })),
  comptime: (out, node) => writeMeta(out, node.expr),
  letconst: (out, node) => (writeMeta(out, node.value), pushBytecode(out, node.token, { type: 'letlocal', name: node.name.token.value, t: false, v: true })),
  tuple:    (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'tupleast', count: node.exprs.length })),
  not:      (out, node) => (visitParseNode(out, node.expr), pushBytecode(out, node.token, { type: 'notast' })),

  for:      (out, node) => forLoopSugar(out, node),
  expand:   (out, node) => expandLoopSugar(out, node),
  fold:     (out, node) => foldSugar(out, node),
  listcomp: (out, node) => listComprehensionSugar(out, node),
  slice:    (out, node) => sliceSugar(out, node),

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
    pushBytecode(out, node.token, { type: 'breakast', n: !!node.name, v: !!node.expr })
  },
  continue: (out, node) => {
    if (node.expr) visitParseNode(out, node.expr);
    pushBytecode(out, node.token, { type: 'continueast', v: !!node.expr })
  },
  field: (out, node) => {
    visitParseNode(out, node.expr)
    pushBytecode(out, node.token, { type: 'fieldast', name: node.field.token.value })
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
    }
    compilerAssert(false, "Not implemented")
  },
  subscript: (out, node) => {
    visitParseNode(out, node.expr)
    visitParseNode(out, node.subscript)
    pushBytecode(out, node.token, { type: 'subscriptast' })
  },
  opeq: (out, node) => {
    if (node.left instanceof ParseIdentifier || node.left instanceof ParseFreshIden) {
      visitParseNode(out, node.left)
      visitParseNode(out, node.right)
      const op = node.token.value.endsWith('=') ? node.token.value.substring(0, node.token.value.length - 1) : node.token.value
      pushBytecode(out, node.token, { type: 'operatorast', name: op, count: 2 })
      pushBytecode(out, node.token, { type: 'setlocalast', name: node.left instanceof ParseFreshIden ? node.left.freshBindingToken.identifier : node.left.token.value })
      return
    }
    compilerAssert(false, "Not implemented yet", { node })
  },

  postcall: (out, node) => {
    compilerAssert(false, "Not implemented 'postcall'", { node })
  },

  list: (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: 'listast', count: node.exprs.length })),

  and: (out, node) => (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: "andast", count: node.exprs.length })),
  or: (out, node) =>  (visitAll(out, node.exprs), pushBytecode(out, node.token, { type: "orast", count: node.exprs.length })),
  
  let: (out, node) => {
    if (node.value) visitParseNode(out, node.value);
    if (node.type) {
      writeMeta(out, node.type);
      pushBytecode(out, node.type.token, { type: 'totype' });
    }
    const name = node.name instanceof ParseFreshIden ? node.name.freshBindingToken.identifier : node.name.token.value
    pushBytecode(out, node.token, { type: 'letast', name, t: !!node.type, v: !!node.value })
  },

  call: (out, node) => {
    if (node.left instanceof ParseCompilerIden) {
      node.typeArgs.forEach(x => writeMeta(out, x));
      visitAll(out, node.args);
      pushBytecode(out, node.token, { type: "compilerfn", name: node.left.value, count: node.args.length, tcount: node.typeArgs.length });
      return
    }
    if (node.left instanceof ParseIdentifier) {
      node.typeArgs.forEach(x => writeMeta(out, x));
      visitAll(out, node.args);
      pushBytecode(out, node.token, { type: "callast", name: node.left.token.value, count: node.args.length, tcount: node.typeArgs.length });
      return;
    }
    if (node.left instanceof ParseField) {
      node.typeArgs.forEach(x => writeMeta(out, x));
      visitParseNode(out, node.left.expr)
      visitAll(out, node.args);
      pushBytecode(out, node.token, { type: "callast", name: node.left.field.token.value, count: node.args.length + 1, tcount: node.typeArgs.length, method: true });
      return;
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
    pushBytecode(out, node.token, { type: 'beginblockast', breakType: node.breakType, name: node.name?.token.value ?? null })
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
    if (node.isExpr) compilerAssert(node.falseBody, "If-expression needs false branch")
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
    bytecode: prototype.bytecode,
    instructionTable: prototype.initialInstructionTable,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNode(out, prototype.body);
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
      if (!(error.info as any).location) (error.info as any).location = vm.location;
      (error.info as any).subCompilerState = subCompilerState
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

const popValues = (vm: Vm, num: number) => {
  compilerAssert(vm.stack.length >= num, `Expected ${num} values on stack got ${vm.stack.length}`)
  return Array.from(new Array(num)).map(() => vm.stack.pop()).reverse() 
};
const popStack = (vm: Vm) => {
  compilerAssert(vm.stack.length > 0, `Expected 1 value on stack got ${vm.stack.length}`)
  return vm.stack.pop();
};


const operators: {[key:string]: { op: string, typeCheck: unknown, comptime: (a: number, b: number) => unknown, func: (location: SourceLocation, a: Ast, b: Ast) => Ast }} = {};

export const getOperatorTable = () => operators

const createOperator = (op: string, operatorName: string, typeCheck: (a: Type, b: Type) => Type, comptime: (a: number, b: number) => unknown) => {
  operators[op] = { 
    op, typeCheck, comptime,
    func: (location: SourceLocation, a: Ast, b: Ast) => {
      const metafunc = a.type.typeInfo.metaobject[operatorName]
      if (metafunc) {
        compilerAssert(metafunc instanceof ExternalFunction, "Not implemented yet", { metafunc })
        return metafunc.func(location, a, b)
      }
      compilerAssert(a.type === b.type, "Can't use operator $op on types $aType and $bType. Both types must be the same", { op, a, b, aType: a.type, bType: b.type  })
      const returnType = typeCheck(a.type, b.type)
      return new OperatorAst(returnType, location, op, [a, b])
    }
  }
}
const typecheckNumberOperator = (a: Type, b: Type) => { compilerAssert(a === IntType || a === FloatType || a === DoubleType, "Expected int, float or double type got $a", { a }); return a }
const typecheckNumberComparison = (a: Type, b: Type) => { compilerAssert(a === IntType || a === FloatType || a === DoubleType); return BoolType }
const typecheckEquality = (a: Type, b: Type) => { compilerAssert(a === IntType || a === FloatType || a === DoubleType); return BoolType }
createOperator("-",  "sub", typecheckNumberOperator,   (a, b) => a - b)
createOperator("*",  "mul", typecheckNumberOperator,   (a, b) => a * b)
createOperator("+",  "add", typecheckNumberOperator,   (a, b) => a + b)
createOperator("/",  "div", typecheckNumberOperator,   (a, b) => a / b)
createOperator(">",  "gt",  typecheckNumberComparison, (a, b) => a > b)
createOperator("<",  "lt",  typecheckNumberComparison, (a, b) => a < b)
createOperator(">=", "gte", typecheckNumberComparison, (a, b) => a >= b)
createOperator("<=", "lte", typecheckNumberComparison, (a, b) => a <= b)
createOperator("==", "eq",  typecheckEquality,         (a, b) => a == b)
createOperator("!=", "neq", typecheckEquality,         (a, b) => a != b)


const letLocalAst = (vm: Vm, name: string, type: Type | null, value: Ast | null) => {
  compilerAssert(type || value);
  compilerAssert(!Object.hasOwn(vm.scope, name), `Already defined $name`, { name });
  const inferType = type || value!.type
  compilerAssert(inferType !== VoidType, "Expected type for local $name but got $inferType", { name, inferType })
  const binding = (vm.scope[name] = new Binding(name, inferType));
  binding.definitionCompiler = vm.context.subCompilerState
  value ||= new DefaultConsAst(inferType.typeInfo.isReferenceType ? RawPointerType : inferType, vm.location)
  return new LetAst(VoidType, vm.location, binding, value);
}

function resolveScope(ctx: TaskContext, scope: Scope, name: string): Task<unknown, CompilerError> {
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

function callFunctionFromValueTask(ctx: TaskContext, vm: Vm, func: unknown, typeArgs: unknown[], values: Ast[]): Task<Unit, CompilerError> {
  if (func instanceof ExternalFunction) {
    const functionResult = func.func(...values);
    vm.stack.push(functionResult);
    return Task.success();
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
    vm.stack.push(createParameterizedExternalType(ctx.globalCompiler, func, typeArgs))
    return Task.success()
  }
  compilerAssert(!(func instanceof FunctionDefinition), "$func is not handled", { func })
  compilerAssert(false, "$func is not a function", { func })
}

const unknownToAst = (location: SourceLocation, value: unknown) => {
  if (typeof value === 'number') return new NumberAst(IntType, location, value);
  if (isAst(value)) return value;
  if (value === null) return new VoidAst(VoidType, location);
  if (value instanceof Binding) return new BindingAst(value.type, location, value);
  compilerAssert(false, "Type is not convertable to an AST: $value", { value })
}

const instructions: InstructionMapping = {
  halt: () => compilerAssert(false, "Handled elsewhere"),
  comment: () => {},
  push: (vm, { value }) => vm.stack.push(value),
  nil: (vm) => vm.stack.push(null),

  numberast: (vm, { value }) => vm.stack.push(new NumberAst(IntType, vm.location, value)),
  stringast: (vm, { value }) => vm.stack.push(new StringAst(StringType, vm.location, value)),
  boolast: (vm, { value }) =>   vm.stack.push(new BoolAst(BoolType, vm.location, value)),
  orast: (vm, { count }) =>     vm.stack.push(new OrAst(BoolType, vm.location, expectAsts(popValues(vm, count)))),
  andast: (vm, { count }) =>    vm.stack.push(new AndAst(BoolType, vm.location, expectAsts(popValues(vm, count)))),
  whileast: (vm) =>             vm.stack.push(new WhileAst(VoidType, vm.location, expectAst(popStack(vm)), expectAst(popStack(vm)))),
  returnast: (vm, { r }) =>     vm.stack.push(new ReturnAst(VoidType, vm.location, r ? expectAst(popStack(vm)) : null)),
  letast: (vm, { name, t, v }) => vm.stack.push(letLocalAst(vm, name, t ? expectType(popStack(vm)) : null, v ? expectAst(popStack(vm)) : null)),
  ifast: (vm, { f, e }) => {
    const cond = expectAst(popStack(vm))
    const trueBody = expectAst(popStack(vm))
    const falseBody = f ? expectAst(popStack(vm)) : null
    const resultType = e && falseBody ? falseBody.type : VoidType
    if (e) compilerAssert(falseBody && falseBody.type === trueBody.type, "If expression inferred to be of type $trueType but got $falseType", { trueType: trueBody.type, falseType: falseBody?.type })
    vm.stack.push(new IfAst(resultType, vm.location, cond, trueBody, falseBody))
  },
  listast: (vm, { count }) => {
    const values = expectAsts(popValues(vm, count));
    const elementType = getCommonType(values.map(x => x.type))
    const type = createParameterizedExternalType(vm.context.globalCompiler, ListTypeConstructor, [elementType]);
    
    vm.stack.push(new ListAst(type, vm.location, values))
  },
  callast: (vm, { name, count, tcount, method }) => {
    const args = popValues(vm, count)
    const typeArgs = popValues(vm, tcount || 0);
    const receiver = method ? args.shift() : null
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

  field: (vm, { name }) => {
    const expr = popStack(vm)
    if (isPlainObject(expr)) {
      compilerAssert(name in expr, "Not found $name in object $expr", { name, expr })
      vm.stack.push(expr[name])
      return
    }
    compilerAssert(false, "Not impl", { expr, name })
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
    compilerAssert(false, "Not impl", { expr, subscript })
  },

  constructorast: (vm, { count }) => {
    const args = expectAsts(popValues(vm, count))
    const type = expectType(popStack(vm))
    if (type instanceof ConcreteClassType) {
      type.compiledClass.fields.forEach((field, i) => {
        compilerAssert(args[i].type === field.fieldType, "Expected $expected but got $got for field $name of constructor for object $obj", { expected: field.fieldType, got: args[i].type, name: field.name, obj: type.compiledClass})
      })
    }
    vm.stack.push(new ConstructorAst(type, vm.location, args))
  },
  operatorast: (vm, { name, count }) => {
    const values = expectAsts(popValues(vm, count));
    compilerAssert(operators[name], "Unexpected operator $name", { name, values })
    vm.stack.push(operators[name].func(vm.location, values[0], values[1]))
  },
  notast: (vm, {}) => {
    let expr = expectAst(popStack(vm));
    if (expr.type !== BoolType) expr = new CastAst(BoolType, vm.location, expr)
    vm.stack.push(new NotAst(BoolType, vm.location, expr))
  },
  setlocalast: (vm, { name }) => {
    return TaskDef(resolveScope, vm.scope, name).chainFn((task, binding) => {
      compilerAssert(binding instanceof Binding, "Expected binding got $binding", { binding })
      vm.stack.push(new SetAst(VoidType, vm.location, binding, expectAst(popStack(vm))))
      return Task.success()
    });
  },
  fieldast: (vm, { name }) => {
    const value = expectAst(popStack(vm));
    const field = value.type.typeInfo.fields.find(x => x.name === name)
    if (field) { vm.stack.push(new FieldAst(field.fieldType, vm.location, value, field)); return }
    return createMethodCall(vm, value, name, [], [])
  },
  setfieldast: (vm, { name }) => {
    const value = expectAst(popStack(vm));
    const expr = expectAst(popStack(vm));
    
    const field = expr.type.typeInfo.fields.find(x => x.name === name)
    compilerAssert(field, "No field $name found on type $type", { name, type: expr.type })
    compilerAssert(field.fieldType === value.type, "Type $type does not match field $name type of $fieldType on object $objType", { name, objType: expr.type, type: value.type, fieldType: field.fieldType })
    vm.stack.push(new SetFieldAst(VoidType, vm.location, expr, field, value))
  },
  subscriptast: (vm, {}) => {
    const right = expectAst(popStack(vm));
    const left = expectAst(popStack(vm));
    compilerAssert(isParameterizedTypeOf(left.type, ListTypeConstructor), "Expected list got $x", { x: left.type })
    compilerAssert(right.type === IntType, "Expected int got $x", { x: right.type })
    compilerAssert(left.type instanceof ParameterizedType)
    const elemType = expectType(left.type.args[0])
    vm.stack.push(new SubscriptAst(elemType, vm.location, left, right))
  },
  list: (vm, { count }) => vm.stack.push(popValues(vm, count)),
  
  breakast: (vm, { v, n }) => {
    const expr = v ? expectAst(popStack(vm)) : null
    const name = n ? popStack(vm) : null
    compilerAssert(!name || name instanceof Binding, "Expected binding", { name })
    let block: LabelBlock
    if (name) {
      block = findLabelByBinding(vm.context.subCompilerState.labelBlock, name as Binding)
      if (expr) {
        if (block.type) compilerAssert(block.type === expr.type, "Block type is already inferred to be $blockType but got an expression of type $exprType", { blockType: block.type, exprType: expr.type })
        block.type = expr.type
      }
    } else block = findLabelBlockByType(vm.context.subCompilerState.labelBlock, 'break');
    vm.stack.push(new BreakAst(VoidType, vm.location, block.binding!, expr))
  },
  continueast: (vm, { v }) => {
    const block = findLabelBlockByType(vm.context.subCompilerState.labelBlock, 'continue');
    vm.stack.push(new BreakAst(VoidType, vm.location, block.binding!, v ? expectAst(popStack(vm)) : null))
  },

  bindingast: (vm, { name }) => {
    return (
      TaskDef(resolveScope, vm.scope, name)
      .chainFn((task, value) => {
        if (value instanceof Binding) ensureBindingIsNotClosedOver(vm.context.subCompilerState, name, value);
        if (isPlainObject(value)) vm.stack.push(value)
        else if (value instanceof Module) vm.stack.push(value)
        else vm.stack.push(unknownToAst(vm.location, value))
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
    if (name) vm.scope[name] = binding
    vm.context.subCompilerState.labelBlock = new LabelBlock(vm.context.subCompilerState.labelBlock, null, breakType, binding)
  },
  endblockast: (vm, {}) => {
    const labelBlock = vm.context.subCompilerState.labelBlock;
    vm.context.subCompilerState.nextLabelBlockDepth --
    compilerAssert(labelBlock, "Invalid endblockast")
    const binding = labelBlock.binding
    compilerAssert(binding, "Expected binding", { labelBlock: labelBlock })
    const body = expectAst(vm.stack.pop())
    const blockType = labelBlock.type ?? body.type
    if (labelBlock.type) compilerAssert(labelBlock.type === VoidType || labelBlock.type === body.type, "Block type inferred to be of type $blockType but the result expression was type $bodyType", { blockType: labelBlock.type, bodyType: body.type })
    vm.stack.push(new BlockAst(blockType, vm.location, binding, body))
    vm.context.subCompilerState.labelBlock = labelBlock.parent
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
      return vm.stack.push(createParameterizedExternalType(vm.context.globalCompiler, TupleTypeConstructor, type.values))
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
    compilerAssert(!Object.hasOwn(vm.scope, name), `$name is already in scope`, { name });
    setScopeValueAndResolveEvents(vm.scope, name, popStack(vm))
    vm.stack.push(null) // statement expression
  },
  setlocal: (vm, { name }) => {
    compilerAssert(Object.hasOwn(vm.scope, name), `$name not existing in scope`, { name });
    vm.scope[name] = popStack(vm);
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

  compilerfn: (vm, { name, count, tcount }) => {
    const values = popValues(vm, count)
    const typeArgs = popValues(vm, tcount || 0);

    if (name === 'iteratefn') {
      const iterator = expectAst(values[0])
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
    const values = expectAsts(popValues(vm, count))
    const argTypes = values.map(x => x.type)
    const type = createParameterizedExternalType(vm.context.globalCompiler, TupleTypeConstructor, argTypes)
    vm.stack.push(new ConstructorAst(type, vm.location, values))
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
    const instr = instructions[current.type] as (vm: Vm, instr: BytecodeInstr) => void;
    compilerAssert(instr, "Not inplemented yet instruction $type", { type: current.type, current })
    let res;
    try {
      res = instr(vm, current);
    } catch(ex) {
      if (ex instanceof CompilerError) Object.assign(ex.info, { ip: vm.ip, current, location: vm.location, subCompilerState: vm.context.subCompilerState })
      throw ex;
    }

    if (isTaskResult(res) || !isTask(res)) {
      if (vm.ip === startIp) vm.ip++;
      current = code[vm.ip];
      vm.location = locations[vm.ip];
    } else {
      return (res as Task<string, CompilerError>).chainFn(() => {
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

  const typeParamHash = hashValues(typeArgs)
  const existing = classDef.compiledClasses.find(compiledClass => {
    if (compiledClass.typeArgHash === typeParamHash) {
      if (compiledClass.typeArguments.every((x, i) => x === typeArgs[i])) return true
    }
  })
  if (existing) return Task.of(existing.type);

  const templateScope = Object.create(classDef.parentScope);
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${classDef.debugName} class template`, lexicalParent: ctx.subCompilerState, scope: templateScope })
  subCompilerState.functionCompiler = undefined;
  
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

      const typeInfo: TypeInfo = { fields: compiledClass.fields, metaobject: compiledClass.metaobject, isReferenceType: true }
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
              func.func(compiledClass);
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

const getCommonType = (types: Type[]): Type => {
  compilerAssert(types.every(x => x === types[0]), "Expected types to be the same")
  return types[0];
}
const createParameterizedExternalType = (globalCompiler: GlobalCompilerState, typeConstructor: ExternalTypeConstructor, argTypes: unknown[]): Type => {
  const newArgTypes = argTypes.map(value => {
    // TODO: Do this more rigorously?
    if (isType(value)) return value;
    if (value instanceof ClassDefinition && value.concreteType) return value.concreteType
    if (value instanceof Tuple) compilerAssert(false, "Not implemented yet")
    compilerAssert(false, "Expected types got $expected", { value }); 
  })
  const newType = typeConstructor.createType(newArgTypes)
  return globalCompiler.typeTable.getOrInsert(newType)
}


function topLevelFunctionDefinitionTask(ctx: TaskContext, funcDecl: ParserFunctionDecl, scope: Scope) {
  const funcDef = insertFunctionDefinition(ctx.globalCompiler, funcDecl)

  if (funcDef.keywords.includes('method')) {
    compilerAssert(funcDecl.args[0]?.[0], "Expected type for first argument")
    let t = funcDecl.args[0][1]!;
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
    bytecode: { code: [], locations: [] },
    instructionTable: BytecodeDefault,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null, expansion: null }
  }
  visitParseNode(out, expr.value);
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(textColors.cyan("Compiled top level def"))
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

export const loadModule = (ctx: TaskContext, location: SourceLocation, moduleName: string, rootScope: Scope): Task<Module, CompilerError> => {
  const loader = ctx.globalCompiler.moduleLoader
  if (loader.cache[moduleName]) return Task.of(loader.cache[moduleName])
  const parsedModule = loader.loadModule(moduleName)
  const moduleScope = createScope({ ...rootScope }, undefined)

  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${moduleName} module`, lexicalParent: undefined, scope: moduleScope })
  ;(subCompilerState as any).location = location

  return (
    TaskDef(runTopLevelTask, parsedModule.rootNode, rootScope, moduleScope)
    .chainFn((task, _) => {
      const module = new Module(moduleName, subCompilerState, parsedModule)
      loader.cache[moduleName] = module
      return Task.of(module)
    }) as Task<Module, CompilerError>
  )
}

export const importModule = (ctx: TaskContext, importNode: ParseImport, rootScope: Scope, existingScope: Scope) => {
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
    TaskDef(loadModule, importNode.token.location, moduleName, rootScope)
    .chainFn((task, module) => {
      ctx.subCompilerState = loader.cache[moduleName].compilerState
      return expandIntoScope(module)
    })
  )
}

export const runTopLevelTask = (ctx: TaskContext, stmts: ParseStatements, rootScope: Scope, moduleScope: Scope) => {
  const tasks: Task<unknown, unknown>[] = []

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
      tasks.push(TaskDef(importModule, node, rootScope, moduleScope));
      return
    }
    if (node.key === 'letconst') {
      tasks.push(TaskDef(topLevelLetConst, node, moduleScope));
      return
    }
    if (node.key === 'function') {
      tasks.push(TaskDef(topLevelFunctionDefinitionTask, node.functionDecl, moduleScope ));
      return
    }
    if (node.key === 'class') {
      tasks.push(TaskDef(topLevelClassDefinitionTask, node.classDecl, moduleScope ));
      return
    }
    compilerAssert(false, `Not supported at top level $key`, { key: node.key })
  })

  return Task.concurrency(tasks);
}
