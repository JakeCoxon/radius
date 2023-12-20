import { isParseVoid, BytecodeOut, FunctionDefinition, Type, Binding, LetAst, UserCallAst, CallAst, Ast, NumberAst, OperatorAst, SetAst, OrAst, AndAst, ListAst, IfAst, StatementsAst, Scope, createScope, Closure, ExternalFunction, compilerAssert, VoidType, IntType, FunctionPrototype, Vm, MetaInstructionTable, Token, expect, createStatements, DoubleType, FloatType, StringType, expectMap, bytecodeToString, ParseCall, ParseIdentifier, ParseNode, CompiledFunction, AstRoot, isAst, pushSubCompilerState, addFunctionDefinition, ParseNil, createToken, ParseStatements, FunctionType, StringAst, WhileAst, BoolAst, BindingAst, SourceLocation, BytecodeInstr, ReturnAst, BytecodeGen, ParserFunctionDecl, ScopeEventsSymbol, BoolType, Tuple, ParseTuple, hashValues, TaskContext, ParseElse, ParseIf, InstructionMapping, GlobalCompilerState, expectType, expectAst, expectAll, expectAsts, BreakAst, LabelBlock, BlockAst, findLabelBlockByType, findLabelBlockAstByType, ParserClassDecl, ClassDefinition, isType, CompiledClass, ConcreteClassType, ClassField, FieldAst, ParseField, SetFieldAst, makeCyan, CompilerError, VoidAst, SubCompilerState, ParseLetConst, PrimitiveType, CastAst, ParseFunction, ListType, SubscriptAst, ExternalType, GenericType, isGenericTypeOf, ParseMeta, createAnonymousParserFunctionDecl, ArgumentTypePair } from "./defs";
import { Event, Task, TaskDef, isTask, isTaskResult } from "./tasks";

const pushBytecode = <T extends BytecodeInstr>(out: BytecodeOut, token: Token, instr: T) => {
  out.bytecode.locations.push(token.location);
  out.bytecode.code.push(instr)
  return instr;
}

const visitParseNode = (out: BytecodeOut, expr: ParseNode) => {
  compilerAssert(expr.key, "$expr not found", { expr })
  const table = out.table
  const instrWriter = expectMap(table, expr.key, `Not implemented parser node $key in ${table === bytecodeDefault ? 'default' : 'second order'} table`)
  instrWriter(out, expr)
}
const visitAll = (out: BytecodeOut, exprs: ParseNode[]) => {
  exprs.forEach(expr => visitParseNode(out, expr))
}
const writeMeta = (out: BytecodeOut, expr: ParseNode) => {
  visitParseNode({ bytecode: out.bytecode, table: bytecodeDefault, globalCompilerState: out.globalCompilerState, state: out.state }, expr)
}
const pushGeneratedBytecode = <T extends BytecodeInstr>(out: BytecodeOut, instr: T) => {
  out.bytecode.code.push(instr);
  out.bytecode.locations.push(new SourceLocation(-1, -1));
  return instr;
}

const bytecodeDefault: MetaInstructionTable = {
  identifier: (out, ast) => pushBytecode(out, ast.token, { type: "binding", name: ast.token.value }), // prettier-ignore
  number:  (out, ast) => pushBytecode(out, ast.token, { type: "push", value: Number(ast.token.value) }), // prettier-ignore
  string:  (out, ast) => pushBytecode(out, ast.token, { type: "push", value: ast.token.value }), // prettier-ignore
  nil:     (out, ast) => pushBytecode(out, ast.token, { type: "push", value: null }), // prettier-ignore
  boolean: (out, ast) => pushBytecode(out, ast.token, { type: "push", value: ast.token.value !== 'false' }), // prettier-ignore

  operator: (out, ast) => (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operator', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  set:      (out, ast) => (visitParseNode(out, ast.value), pushBytecode(out, ast.token, { type: 'setlocal', name: ast.left.token.value })), // prettier-ignore
  letconst: (out, ast) => (visitParseNode(out, ast.value), pushBytecode(out, ast.token, { type: 'letlocal', name: ast.name.token.value, t: false, v: true })), // prettier-ignore
  meta:     (out, ast) => (visitParseNode(out, ast.expr)),
  comptime: (out, ast) => (visitParseNode(out, ast.expr)),

  list:  (out, ast) => (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'list', count: ast.exprs.length })),
  tuple: (out, ast) => (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'tuple', count: ast.exprs.length })),
  
  let: (out, ast) => {
    if (ast.value) visitParseNode(out, ast.value);
    if (ast.type) {
      writeMeta(out, ast.type);
      pushBytecode(out, ast.type.token, { type: 'totype' });
    }
    pushBytecode(out, ast.token, { type: 'letlocal', name: ast.name.token.value, t: !!ast.type, v: !!ast.value }) // prettier-ignore
  },

  function: (out, ast) => {
    pushBytecode(out, ast.token, { type: "closure", id: addFunctionDefinition(out.globalCompilerState, ast.functionDecl).id }) // prettier-ignore
  },
  call: (out, ast) => {
    // compilerAssert(ast.typeArgs.length === 0, "Not implemented", { ast })
    visitAll(out, ast.typeArgs)
    visitAll(out, ast.args);
    if (ast.left instanceof ParseIdentifier) {
      pushBytecode(out, ast.token, { type: "call", name: ast.left.token.value, count: ast.args.length, tcount: ast.typeArgs.length }); // prettier-ignore
      return;
    }
    compilerAssert(false, "Call with non-identifier not implemented yet")
  },
  return: (out, ast) => {
    if (ast.expr) visitParseNode(out, ast.expr);
    pushBytecode(out, ast.token, { type: 'return', r: !!ast.expr })
  },
  break: (out, ast) => {
    if (ast.expr) visitParseNode(out, ast.expr);
    const instr = pushBytecode(out, ast.token, { type: 'jump', address: 0 })
    findLabelBlockByType(out.state.labelBlock, "break").completion.push((address: number) => { instr.address = address })
  },
  continue: (out, ast) => {
    if (ast.expr) visitParseNode(out, ast.expr);
    const instr = pushBytecode(out, ast.token, { type: 'jump', address: 0 })
    findLabelBlockByType(out.state.labelBlock, "continue").completion.push((address: number) => { instr.address = address })
  },

  statements: (out, ast) => {
    ast.exprs.forEach((stmt, i) => {
      visitParseNode(out, stmt);
      if (i !== ast.exprs.length - 1) pushBytecode(out, ast.token, { type: "pop" });
    });
  },
  block: (out, ast) => visitParseNode(out, ast.statements),
  
  and: (out, ast) => {
    visitParseNode(out, ast.exprs[0]);
    const jump1 = { type: "jumpf" as const, address: 0 };
    pushBytecode(out, ast.exprs[0].token, jump1);
    pushBytecode(out, ast.exprs[0].token, { type: 'pop' });
    visitParseNode(out, ast.exprs[1])
    jump1.address = out.bytecode.code.length;
  },

  or: (out, ast) => {
    visitParseNode(out, ast.exprs[0]);
    const jump1 = { type: "jumpf" as const, address: 0 };
    pushBytecode(out, ast.exprs[0].token, jump1);
    const jump2 = { type: "jump" as const, address: 0 };
    pushBytecode(out, ast.exprs[0].token, jump2);
    jump1.address = out.bytecode.code.length;
    pushBytecode(out, ast.exprs[0].token, { type: 'pop' });
    visitParseNode(out, ast.exprs[1])
    jump2.address = out.bytecode.code.length;
  },

  else: (out, ast) => visitParseNode(out, ast.body),
  if: (out, ast) => {
    visitParseNode(out, ast.condition);
    const jump1 = { type: "jumpf" as const, address: 0 };
    pushBytecode(out, ast.condition.token, jump1);
    visitParseNode(out, ast.trueBody);
    if (ast.falseBody) {
      const jump2 = { type: "jump" as const, address: 0 };
      pushBytecode(out, ast.trueBody.token, jump2);
      jump1.address = out.bytecode.code.length;
      visitParseNode(out, ast.falseBody);
      jump2.address = out.bytecode.code.length;
    } else {
      jump1.address = out.bytecode.code.length;
    }
  },
  metaif: (out, ast) => {
    // Same as if
    const if_ = ast.expr;
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
  while: (out, ast) => {
    pushBytecode(out, ast.condition.token, { type: "comment", comment: "while begin" });
    const breakBlock = new LabelBlock(out.state.labelBlock, "labelblock", 'break', null)
    const continueBlock = new LabelBlock(breakBlock, "labelblock", 'continue', null)
    out.state.labelBlock = continueBlock;
    const loopTarget = out.bytecode.code.length
    visitParseNode(out, ast.condition);
    const jump1 = pushBytecode(out, ast.condition.token, { type: "jumpf", address: 0 });
    visitParseNode(out, ast.body);
    continueBlock.completion.forEach(f => f(out.bytecode.code.length))
    continueBlock.completion.length = 0
    pushBytecode(out, ast.condition.token, { type: "jump", address: loopTarget });
    jump1.address = out.bytecode.code.length
    breakBlock.completion.forEach(f => f(out.bytecode.code.length))
    breakBlock.completion.length = 0
    out.state.labelBlock = breakBlock.parent;
    pushBytecode(out, ast.condition.token, { type: "comment", comment: "while end" });
  }
};

const bytecodeSecond: MetaInstructionTable = {
  identifier: (out, ast) => pushBytecode(out, ast.token, { type: "bindingast", name: ast.token.value }), // prettier-ignore
  number:  (out, ast) => pushBytecode(out, ast.token, { type: "numberast", value: Number(ast.token.value) }), // prettier-ignore
  string:  (out, ast) => pushBytecode(out, ast.token, { type: "stringast", value: ast.token.value }), // prettier-ignore
  boolean: (out, ast) => pushBytecode(out, ast.token, { type: "boolast", value: ast.token.value !== 'false' }), // prettier-ignore

  operator: (out, ast) => (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'operatorast', name: ast.token.value, count: ast.exprs.length })), // prettier-ignore
  meta:     (out, ast) => (writeMeta(out, ast.expr), pushBytecode(out, ast.token, { type: 'toast' })),
  comptime: (out, ast) => writeMeta(out, ast.expr),
  letconst: (out, ast) => (writeMeta(out, ast.value), pushBytecode(out, ast.token, { type: 'letlocal', name: ast.name.token.value, t: false, v: true })),
  tuple:    (out, ast) => (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'tuple', count: ast.exprs.length })),

  while: (out, ast) => {
    pushBytecode(out, ast.token, { type: 'beginblockast', breakType: 'break' })
    pushBytecode(out, ast.token, { type: 'beginblockast', breakType: 'continue' })
    visitParseNode(out, ast.body);
    pushBytecode(out, ast.token, { type: 'endblockast' })
    visitParseNode(out, ast.condition)
    pushBytecode(out, ast.token, { type: 'whileast' })
    pushBytecode(out, ast.token, { type: 'endblockast' })
  },
  for: (out, ast) => {
    const fnArgs: ArgumentTypePair[] = [[ast.identifier, null]]
    const decl = createAnonymousParserFunctionDecl("for", ast.token, fnArgs, ast.body)
    const fn = new ParseFunction(ast.token, decl)
    const elemType = new ParseIdentifier(createToken('int')) // Hard code int for now
    const iterate = new ParseIdentifier(createToken('iterate'))
    const call = new ParseCall(ast.token, iterate, [ast.expr], [fn, elemType])
    visitParseNode(out, call)
  },

  function: (out, ast) => {
    pushBytecode(out, ast.token, { type: "closure", id: addFunctionDefinition(out.globalCompilerState, ast.functionDecl).id })
  },
  return: (out, ast) => {
    if (ast.expr) visitParseNode(out, ast.expr);
    pushBytecode(out, ast.token, { type: 'returnast', r: !!ast.expr })
  },
  break: (out, ast) => {
    if (ast.expr) visitParseNode(out, ast.expr);
    pushBytecode(out, ast.token, { type: 'breakast', v: !!ast.expr })
  },
  continue: (out, ast) => {
    if (ast.expr) visitParseNode(out, ast.expr);
    pushBytecode(out, ast.token, { type: 'continueast', v: !!ast.expr })
  },
  field: (out, ast) => {
    visitParseNode(out, ast.expr)
    pushBytecode(out, ast.token, { type: 'fieldast', name: ast.field.token.value })
  },

  set: (out, ast) => {
    if (ast.left instanceof ParseIdentifier) {
      visitParseNode(out, ast.value);
      pushBytecode(out, ast.token, { type: 'setlocalast', name: ast.left.token.value })
      return
    } else if (ast.left instanceof ParseField) {
      visitParseNode(out, ast.left.expr);
      visitParseNode(out, ast.value);
      pushBytecode(out, ast.token, { type: 'setfieldast', name: ast.left.field.token.value })
      return
    }
    compilerAssert(false, "Not implemented")
  },
  subscript: (out, ast) => {
    visitParseNode(out, ast.expr)
    visitParseNode(out, ast.subscript)
    pushBytecode(out, ast.token, { type: 'subscriptast' })
  },
  opeq: (out, ast) => {
    if (ast.left instanceof ParseIdentifier) {
      visitParseNode(out, ast.left)
      visitParseNode(out, ast.right)
      const op = ast.token.value.endsWith('=') ? ast.token.value.substring(0, ast.token.value.length - 1) : ast.token.value
      pushBytecode(out, ast.token, { type: 'operatorast', name: op, count: 2 })
      pushBytecode(out, ast.token, { type: 'setlocalast', name: ast.left.token.value })
      return
    }
    compilerAssert(false, "Not implemented yet", { ast })
  },

  list: (out, ast) => (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: 'listast', count: ast.exprs.length })),

  and: (out, ast) => (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "andast", count: ast.exprs.length })),
  or: (out, ast) =>  (visitAll(out, ast.exprs), pushBytecode(out, ast.token, { type: "orast", count: ast.exprs.length })),
  
  let: (out, ast) => {
    if (ast.value) visitParseNode(out, ast.value);
    if (ast.type) {
      writeMeta(out, ast.type);
      pushBytecode(out, ast.type.token, { type: 'totype' });
    }
    pushBytecode(out, ast.token, { type: 'letast', name: ast.name.token.value, t: !!ast.type, v: !!ast.value })
  },

  call: (out, ast) => {
    ast.typeArgs.forEach(x => writeMeta(out, x));
    visitAll(out, ast.args);
    if (ast.left instanceof ParseIdentifier) {
      pushBytecode(out, ast.token, { type: "callast", name: ast.left.token.value, count: ast.args.length, tcount: ast.typeArgs.length }); // prettier-ignore
      return;
    }
    compilerAssert(false, "Call with non-identifier not implemented yet")
  },

  statements: (out, ast) => {
    pushBytecode(out, ast.token, { type: "pushqs" });
    ast.exprs.forEach((stmt, i) => {
      visitParseNode(out, stmt);
      if (!isParseVoid(stmt)) pushBytecode(out, ast.token, { type: "appendq" });
      pushBytecode(out, ast.token, { type: "pop" }); // Even pop the final value
    });
    pushBytecode(out, ast.token, { type: "popqs" });
  },
  block: (out, ast) => visitParseNode(out, ast.statements),

  else: (out, ast) => visitParseNode(out, ast.body),

  metaif: (out, ast) => {
    const if_ = ast.expr
    writeMeta(out, if_.condition);
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

  if: (out, ast) => {
    if (ast.falseBody) visitParseNode(out, ast.falseBody)
    visitParseNode(out, ast.trueBody)
    visitParseNode(out, ast.condition)
    pushBytecode(out, ast.token, { type: "ifast", f: !!ast.falseBody });
  }
};

const compileFunctionPrototype = (ctx: TaskContext, prototype: FunctionPrototype) => {
  if (prototype.bytecode) return prototype.bytecode;

  prototype.bytecode = { code: [], locations: [] }
  const out: BytecodeOut = {
    bytecode: prototype.bytecode,
    table: prototype.instructionTable,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null }
  }
  visitParseNode(out, prototype.body);
  pushGeneratedBytecode(out, { type: "halt" })

  ctx.globalCompiler.logger.log(makeCyan(`Compiled ${prototype.name}`))
  ctx.globalCompiler.logger.log(bytecodeToString(prototype.bytecode))
  ctx.globalCompiler.logger.log("")
  return prototype.bytecode;
};

function executeBytecodeTask(ctx: TaskContext, subCompilerState: SubCompilerState, bytecode: BytecodeGen, scope: Scope): Task<unknown, never> {
  compilerAssert(ctx.subCompilerState === subCompilerState, "Subcompiler state must have been set already", { fatal: true, subCompilerState, ctxSubCompilerState: ctx.subCompilerState })
  compilerAssert(scope, "Expected scope", { fatal: true })

  const vm: Vm = { ip: 0, stack: [], scope, location: undefined!, bytecode: bytecode!, context: ctx };
  compilerAssert(bytecode, "", { fatal: true })

  return (
    TaskDef(executeVmTask, { vm })
    .chainFn((task, arg) => {

      compilerAssert(vm.stack.length === 1, "Expected 1 value on stack at end of function. Got $num", { num: vm.stack.length, vm })

      const result = vm.stack.pop();
      // vm = compilerState ? compilerState.vm : undefined!;
      return Task.of(result);
    })
  );
};


type TypeCheckHeaderArg = {
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  concreteTypes: Type[] // output
}

function compileAndExecuteFunctionHeaderTask(ctx: TaskContext, { func, args, typeArgs, parentScope, concreteTypes }: TypeCheckHeaderArg): Task<string, never> {

  compilerAssert(typeArgs.length === func.typeArgs.length, "Expected $expected type parameters, got $got", { expected: func.typeArgs.length, got: typeArgs.length })
  compilerAssert(args.length === func.args.length, 'Expected $x args got $y', { x: func.args.length, y: args.length })
  func.args.forEach((arg, i) => {
    // expectArg(arg[0], args[i].type, arg[1])
  })
  if (func.args.length === 0) return Task.of("success");
  
  if (!func.headerPrototype) {

    func.headerPrototype = { name: `${func.debugName} header`, body: null!, instructionTable: bytecodeDefault }; // prettier-ignore
    func.headerPrototype.bytecode = { code: [], locations: [] }
    const out: BytecodeOut = {
      bytecode: func.headerPrototype.bytecode,
      table: func.headerPrototype.instructionTable,
      globalCompilerState: ctx.globalCompiler,
      state: { labelBlock: null }
    }
    // visitParseNode(out, func.headerPrototype.body);
    func.args.forEach(([name, type], i) => {
      if (type === null) return visitParseNode(out, new ParseNil(createToken('')));
      visitParseNode(out, type)
      pushBytecode(out, type.token, { type: 'totype' });
    })

    pushGeneratedBytecode(out, { type: "tuple", count: func.args.length })
    pushGeneratedBytecode(out, { type: "halt" })

    ctx.globalCompiler.logger.log(makeCyan(`Compiled ${func.headerPrototype.name}`))
    ctx.globalCompiler.logger.log(bytecodeToString(func.headerPrototype.bytecode))
    ctx.globalCompiler.logger.log("")
  }

  const scope = createScope({})
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} header`, scope, lexicalParent: ctx.subCompilerState });
  subCompilerState.functionCompiler = subCompilerState

  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier)
    scope[typeArg.token.value] = typeArgs[i];
  })

  return (
    TaskDef(executeBytecodeTask, subCompilerState, func.headerPrototype!.bytecode!, scope)
    .chainFn((task, compiledArgTypes) => {

      compilerAssert(compiledArgTypes instanceof Tuple, "Expected tuple")
      compiledArgTypes.values.forEach((type, i) => {
        if (type === null) {
          concreteTypes.push(args[i].type)
          return
        }
        compilerAssert(isType(type));
        compilerAssert(args[i].type === type, "Argument $name of type $value does not match $expected", { name: func.args[i][0].token, value: args[i].type, expected: type })
        concreteTypes.push(type)
      })
      return Task.of("success")
    })
  )
}

type TypeCheckAndCompileArg = {
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  concreteTypes: Type[]
}

export function functionTemplateTypeCheckAndCompileTask(ctx: TaskContext, { func, typeArgs, args, parentScope, concreteTypes }: TypeCheckAndCompileArg): Task<CompiledFunction, never> {

  const argBindings: Binding[] = [];

  compilerAssert(func.body)

  if (!func.templatePrototype)  {
    func.templatePrototype = { name: `${func.debugName} template bytecode`, body: func.body, instructionTable: bytecodeSecond }; // prettier-ignore
    compileFunctionPrototype(ctx, func.templatePrototype);
  }
  compilerAssert(func.templatePrototype);

  const typeParamHash = hashValues(typeArgs)
  const existing = func.compiledFunctions.find(compiledFunc => {
    if (compiledFunc.typeParamHash === typeParamHash) {
      if (compiledFunc.typeParameters.every((x, i) => x === typeArgs[i])) return true
    }
  })
  if (existing) return Task.of(existing);

  const templateScope = createScope({})
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} template`, scope: templateScope, lexicalParent: ctx.subCompilerState });
  subCompilerState.functionCompiler = subCompilerState
  
  func.args.forEach(([iden, type], i) => {
    const binding = new Binding(iden.token.value, concreteTypes[i]);
    templateScope[iden.token.value] = binding;
    argBindings.push(binding);
  });
  
  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    templateScope[typeArg.token.value] = typeArgs[i];
  });

  return (
    TaskDef(executeBytecodeTask, subCompilerState, func.templatePrototype.bytecode!, templateScope)
    .chainFn((task, ast) => {

      const concreteTypes = []

      ctx.globalCompiler.logger.log(makeCyan(`Compiled template ${func.debugName}`))
      
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      const id = func.compiledFunctions.length;
      const binding = new Binding(`${func.debugName} compiled ${id}`, FunctionType);
      const returnType = ast.type;
      const compiledFunction = new CompiledFunction(
          binding, func, returnType, concreteTypes, ast, argBindings, typeArgs, typeParamHash);
      ctx.globalCompiler.compiledFunctions.set(binding, compiledFunction);
      func.compiledFunctions.push(compiledFunction)
      
      return Task.of(compiledFunction)
    })
  )

}

type FunctionCallArg = {
  vm: Vm
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: Ast[],
  parentScope: Scope
  concreteTypes: Type[]
}

function functionInlineTask(ctx: TaskContext, { vm, func, typeArgs, args, parentScope, concreteTypes }: FunctionCallArg): Task<string, never> {

  const argBindings: Binding[] = [];
  const statements: Ast[] = []
  const location = vm.location;

  compilerAssert(func.body)

  if (!func.templatePrototype)  {
    func.templatePrototype = { name: `${func.debugName} inline bytecode`, body: func.body, instructionTable: bytecodeSecond }; // prettier-ignore
    compileFunctionPrototype(ctx, func.templatePrototype);
  }
  compilerAssert(func.templatePrototype);
  
  const templateScope = createScope({})
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} inline`, lexicalParent: ctx.subCompilerState, scope: templateScope })
  
  func.args.forEach(([iden, type], i) => {
    compilerAssert(concreteTypes[i], `Expected type`, { args, concreteTypes })
    const binding = new Binding(iden.token.value, concreteTypes[i]);
    templateScope[iden.token.value] = binding;
    statements.push(new LetAst(VoidType, location, binding, args[i]))
    argBindings.push(binding);
  });
  
  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    templateScope[typeArg.token.value] = typeArgs[i];
  });

  return (
    TaskDef(executeBytecodeTask, subCompilerState, func.templatePrototype.bytecode!, templateScope)
    .chainFn((task, ast) => {

      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      ctx.globalCompiler.logger.log(makeCyan(`Compiled inline ${func.debugName}`))
      vm.stack.push(new StatementsAst(ast.type, location, [...statements, ast]));
      
      return Task.of("success")
    })
  )

}

type CompileTimeFunctionCallArg = {
  vm: Vm
  func: FunctionDefinition,
  typeArgs: unknown[],
  args: unknown[],
  parentScope: Scope
}

function functionCompileTimeCompileTask(ctx: TaskContext, { vm, func, typeArgs, args, parentScope }: CompileTimeFunctionCallArg): Task<string, never> {
  compilerAssert(func.body, "Expected body");
  compilerAssert(args.length === func.args.length, "Expected $expected arguments got $got", { expected: func.args.length, got: func.args.length, func })

  if (!func.compileTimePrototype) 
    func.compileTimePrototype = { name: `${func.debugName} comptime bytecode`, body: func.body, instructionTable: bytecodeDefault }; // prettier-ignore
  compilerAssert(func.compileTimePrototype)

  const scope = Object.create(parentScope);
  const subCompilerState = pushSubCompilerState(ctx, { debugName: `${func.debugName} comptime`, lexicalParent: ctx.subCompilerState, scope })
  subCompilerState.functionCompiler = subCompilerState

  args.forEach((arg, i) => {
    scope[func.args[i][0].token.value] = arg;
  });
  func.typeArgs.forEach((typeArg, i) => {
    compilerAssert(typeArg instanceof ParseIdentifier, "Not implemented")
    scope[typeArg.token.value] = typeArgs[i];
  });
  compileFunctionPrototype(ctx, func.compileTimePrototype);

  return (
    TaskDef(executeBytecodeTask, subCompilerState, func.compileTimePrototype.bytecode!, scope)
    .chainFn((task, res) => { vm.stack.push(res); return Task.of("success") })
  );
};

const popValues = (vm: Vm, num: number) => {
  compilerAssert(vm.stack.length >= num, `Expected ${num} values on stack got ${vm.stack.length}`)
  return Array.from(new Array(num)).map(() => vm.stack.pop()).reverse() // prettier-ignore
};
const popStack = (vm: Vm) => {
  compilerAssert(vm.stack.length > 0, `Expected 1 value on stack got ${vm.stack.length}`)
  return vm.stack.pop();
};


const operators = {
  "-": (a, b) => a - b,
  "*": (a, b) => a * b,
  "+": (a, b) => a + b,
  "/": (a, b) => a / b,
  "<": (a, b) => a < b,
  ">": (a, b) => a > b,
  ">=": (a, b) => a >= b,
  "<=": (a, b) => a <= b,
  "==": (a, b) => a == b,
  "!=": (a, b) => a != b,
};


const letLocal = (vm: Vm, name: string, type: Type | null, value: Ast | null) => {
  compilerAssert(type || value);
  compilerAssert(!Object.hasOwn(vm.scope, name), `Already defined $name`, { name });
  const inferType = type || value!.type
  const binding = (vm.scope[name] = new Binding(name, inferType));
  return new LetAst(VoidType, vm.location, binding, value);
}

type CallArgs = { vm: Vm, name: string, count: number, tcount: number }

function createCallAstTask(ctx: TaskContext, { vm, name, count, tcount }: CallArgs): Task<string, never> {
  const args = popValues(vm, count)
  const typeArgs = popValues(vm, tcount || 0);
  compilerAssert(args.every(isAst), "Expected ASTs for function call $name", { name, args })

  return (
    TaskDef(resolveScope, vm.scope, name)
    .chainFn((task, value) => {

      if (value instanceof PrimitiveType) {
        compilerAssert(count === 1 && tcount === 0, "Expected 1 arg got $count", { count})
        vm.stack.push(new CastAst(value, vm.location, args[0]))
        return Task.of("success")
      }

      if (value instanceof ExternalFunction) {
        vm.stack.push(new CallAst(IntType, vm.location, value, args));
        return Task.of("success")
      }

      if (value instanceof Closure) {

        const { func, scope: parentScope } = value
        const call: FunctionCallArg = { vm, func, typeArgs, args, parentScope, concreteTypes: [] }
        
        if (func.inline) return (
          TaskDef(compileAndExecuteFunctionHeaderTask, call)
          .chain(TaskDef(functionInlineTask, call))
        )
        return (
          TaskDef(compileAndExecuteFunctionHeaderTask, call)
          .chain(TaskDef(functionTemplateTypeCheckAndCompileTask, call))
          .chainFn((task, compiledFunction) => {
            const binding = compiledFunction.binding;
            const returnType = compiledFunction.returnType;
            vm.stack.push(new UserCallAst(returnType, vm.location, binding, args));
            return Task.of("success")
          })
        )
      }
      compilerAssert(false, "Not supported value $value", { value })

    })
  )
}

function resolveScope(ctx: TaskContext, scope: Scope, name: string): Task<unknown, never> {
  let compilerState: SubCompilerState | undefined = ctx.subCompilerState;
  compilerAssert(scope === compilerState.scope, "Expected scope of current compiler state", { fatal: true })
  while (compilerState) {
    if (compilerState.scope[name] !== undefined) return Task.of(compilerState.scope[name])
    compilerState = compilerState.lexicalParent
  }

  // TODO: This should attach events to every ancestor in the scope chain
  if (!scope[ScopeEventsSymbol]) scope[ScopeEventsSymbol] = {}
  if (!scope[ScopeEventsSymbol][name]) scope[ScopeEventsSymbol][name] = new Event<string, never>()
  ctx.globalCompiler.allWaitingEvents.push(scope[ScopeEventsSymbol][name])
  return Task.waitForOrError(scope[ScopeEventsSymbol][name], (f) => {
    compilerAssert(false, "Binding $name not found in scope", { name, scope })
  })
}

function callFunctionTask(ctx: TaskContext, { vm, name, count, tcount }: CallArgs): Task<string, never> {
  const values = popValues(vm, count);
  const typeArgs = popValues(vm, tcount || 0);
  return (
    TaskDef(resolveScope, vm.scope, name)
    .chainFn((task, func) => {

      if (func instanceof ExternalFunction) {
        const functionResult = func.func(...values);
        vm.stack.push(functionResult);
        return Task.of('success');
      }

      if (func instanceof Closure) {
        const call: CompileTimeFunctionCallArg = { vm, func: func.func, typeArgs, args: values, parentScope: func.scope };
        return TaskDef(functionCompileTimeCompileTask, call)
      }

      if (func instanceof ClassDefinition) {
        compilerAssert(func.abstract, "Expected abstract class got $func", { func });
        compilerAssert(values.length === 0, "Not implemented", { values })
        return (
          TaskDef(compileClassTask, { classDef: func, typeArgs })
          .chainFn((task, type) => { vm.stack.push(type); return Task.of('success') })
        )
      }

      if (func instanceof ExternalType) {
        compilerAssert(values.length === 0, "Expected no args", { values })
        compilerAssert(typeArgs.length === 1, "Expected one type arg", { typeArgs })
        vm.stack.push(createGenericType(ctx.globalCompiler, func, [expectType(typeArgs[0])]))
        return Task.of('success')
      }

      compilerAssert(!(func instanceof FunctionDefinition), "$func is not handled", { func })
      compilerAssert(false, "$func is not a function", { func })
    })
  )
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

  operatorast: (vm, { name, count }) => {
    const values = expectAsts(popValues(vm, count));
    compilerAssert(values.every(x => x.type === values[0].type), "Can't use operator $name on types $a and $b. Both types must be the same", { name, a: values[0].type, b: values[1].type, values })
    vm.stack.push(new OperatorAst(values[0].type, vm.location, name, values))
  },
  numberast: (vm, { value }) => vm.stack.push(new NumberAst(IntType, vm.location, value)),
  stringast: (vm, { value }) => vm.stack.push(new StringAst(StringType, vm.location, value)),
  boolast: (vm, { value }) =>   vm.stack.push(new BoolAst(BoolType, vm.location, value)),
  orast: (vm, { count }) =>     vm.stack.push(new OrAst(IntType, vm.location, expectAsts(popValues(vm, count)))),
  andast: (vm, { count }) =>    vm.stack.push(new AndAst(IntType, vm.location, expectAsts(popValues(vm, count)))),
  listast: (vm, { count }) => {
    const values = expectAsts(popValues(vm, count));
    const elementType = getCommonType(values.map(x => x.type))
    const type = createGenericType(vm.context.globalCompiler, ListType, [elementType]);
    vm.stack.push(new ListAst(type, vm.location, values))
  },
  ifast: (vm, { f }) =>         vm.stack.push(new IfAst(IntType, vm.location, expectAst(popStack(vm)), expectAst(popStack(vm)), f ? expectAst(popStack(vm)) : null)),
  whileast: (vm) =>             vm.stack.push(new WhileAst(VoidType, vm.location, expectAst(popStack(vm)), expectAst(popStack(vm)))),
  returnast: (vm, { r }) =>     vm.stack.push(new ReturnAst(VoidType, vm.location, r ? expectAst(popStack(vm)) : null)),
  letast: (vm, { name, t, v }) => vm.stack.push(letLocal(vm, name, t ? expectType(popStack(vm)) : null, v ? expectAst(popStack(vm)) : null)),
  callast: (vm, { name, count, tcount }) => TaskDef(createCallAstTask, { vm, name, count, tcount }),
  toast: (vm) => vm.stack.push(unknownToAst(vm.location, popStack(vm))),
  setlocalast: (vm, { name }) => {
    const binding = expectMap(vm.scope, name, `No binding $key`);
    vm.stack.push(new SetAst(VoidType, vm.location, binding, expectAst(popStack(vm))))
  },
  fieldast: (vm, { name }) => {
    const value = expectAst(popStack(vm));
    if (value.type instanceof ConcreteClassType) {
      const field = value.type.compiledClass.fields.find(x => x.name === name)
      compilerAssert(field, "No field $name found on type $type", { name, type: value.type })
      vm.stack.push(new FieldAst(field.type, vm.location, value, field.binding))
      return
    }
    if (value.type instanceof GenericType) {
      if (value.type.baseType === ListType) {
        const binding = (value.type.baseType as typeof ListType).lengthBinding
        vm.stack.push(new FieldAst(IntType, vm.location, value, binding))
        return
      }
    }
    compilerAssert(false, "No field $name found on type $type", { name, type: value.type })
  },
  setfieldast: (vm, { name }) => {
    const value = expectAst(popStack(vm));
    const expr = expectAst(popStack(vm));
    if (expr.type instanceof ConcreteClassType) {
      const field = expr.type.compiledClass.fields.find(x => x.name === name)
      compilerAssert(field, "No field $name found on type $type", { name, type: value.type })
      compilerAssert(field.type === value.type, "Type $type does not match $exprType field $name type of $type", { name, exprType: expr.type, type: value.type })
      vm.stack.push(new SetFieldAst(VoidType, vm.location, expr, field.binding, value))
      return
    }
    compilerAssert(false, "No field $name found on type $type", { name, type: value.type })
  },
  subscriptast: (vm, {}) => {
    const right = expectAst(popStack(vm));
    const left = expectAst(popStack(vm));
    compilerAssert(isGenericTypeOf(left.type, ListType), "Expected list got $x", { x: left.type })
    compilerAssert(right.type === IntType, "Expected int got $x", { x: right.type })
    vm.stack.push(new SubscriptAst(IntType, vm.location, left, right))
  },
  list: (vm, { count }) => vm.stack.push(popValues(vm, count)),
  
  breakast: (vm, { v }) => {
    const block = findLabelBlockByType(vm.context.subCompilerState.labelBlock, 'break');
    vm.stack.push(new BreakAst(VoidType, vm.location, block.binding!, v ? expectAst(popStack(vm)) : null))
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
        vm.stack.push(unknownToAst(vm.location, value))
        return Task.of('success')
      })
    )
  },
  return: (vm, { r }) => { 
    const ret = r ? vm.stack[vm.stack.length - 1] : null;
    vm.stack.length = 0
    vm.stack.push(ret);
    vm.ip = vm.bytecode.code.length - 1;
  },
  beginblockast: (vm, { breakType }) => {
    const binding = new Binding(`labelbreakast`, VoidType);
    vm.context.subCompilerState.labelBlock = new LabelBlock(vm.context.subCompilerState.labelBlock, null, breakType, binding)
  },
  endblockast: (vm, {}) => {
    compilerAssert(vm.context.subCompilerState.labelBlock, "Invalid endblockast")
    vm.context.subCompilerState.labelBlock = vm.context.subCompilerState.labelBlock.parent
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
    if (type instanceof ClassDefinition) {
      if (type.concreteType) return vm.stack.push(type.concreteType)
      return (
        TaskDef(compileClassTask, { classDef: type, typeArgs: [] })
        .chainFn((task, res) => { vm.stack.push(res); return Task.of('success') })
      );
    }
    compilerAssert(false, "Expected Type got $type", { type })
  },
  
  pop: (vm) => vm.stack.pop(),
  jumpf: (vm, { address }) => {
    if (!vm.stack.pop()) vm.ip = address;
  },
  letlocal: (vm, { name }) => {
    expect(!Object.hasOwn(vm.scope, name), `$name is already in scope`, { name });
    setScopeValueAndResolveEvents(vm.scope, name, popStack(vm))
    vm.stack.push(null) // statement expression
  },
  setlocal: (vm, { name }) => {
    expect(Object.hasOwn(vm.scope, name), `$name not existing in scope`, { name });
    vm.scope[name] = popStack(vm);
  },
  jump: (vm, { address }) => void (vm.ip = address),
  call: (vm, { name, count, tcount }) => TaskDef(callFunctionTask, { vm, name, count, tcount }),
  operator: (vm, { name, count }) => {
    const values = popValues(vm, count);
    compilerAssert(operators[name], `Invalid operator $name`, { name });
    const operatorResult = operators[name](...values);
    vm.stack.push(operatorResult);
  },
  closure: (vm, { id }) => {
    compilerAssert(typeof id === 'number')
    const globalCompiler = (vm.context as TaskContext).globalCompiler
    compilerAssert(id in globalCompiler.functionDefinitions, "Not found in func $id", { id })
    const func = globalCompiler.functionDefinitions[id];
    const closure = new Closure(func, vm.scope);
    vm.stack.push(closure);
    if (func.name) {
      compilerAssert(!Object.hasOwn(vm.scope, func.name.token.value), "$name already in scope", { name: func.name, value: vm.scope[func.name.token.value] })
      vm.scope[func.name.token.value] = closure;
    }
  },
  tuple: (vm, { count }) => vm.stack.push(new Tuple(popValues(vm, count))),
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
  let compilerState: SubCompilerState | undefined = subCompilerState
  let found: SubCompilerState | undefined = undefined!
  while (compilerState) {
    if (compilerState.scope[name] === value) { found = compilerState; break }
    compilerState = compilerState.lexicalParent
  }
  compilerAssert(found?.functionCompiler === subCompilerState.functionCompiler, "Name $name is declared in an external function which isn't supported in this compiler. You might want to use an inline function", { name, found, subCompilerState })
}

function executeVmTask(ctx: TaskContext, { vm } : { vm: Vm }, p: void): Task<string, never> {
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
      return (res as Task<string, never>).chainFn(() => {
        if (vm.ip === startIp) vm.ip++;
        current = code[vm.ip];
        vm.location = locations[vm.ip];

        return TaskDef(executeVmTask, { vm })
      })
    }
  }
  return Task.of('success')
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

function topLevelFunctionDefinitionTask(ctx: TaskContext, funcDecl: ParserFunctionDecl, scope: Scope) {
  const funcDef = addFunctionDefinition(ctx.globalCompiler, funcDecl)

  compilerAssert(!Object.hasOwn(scope, funcDef.name!.token.value), "$name already in scope", { name: funcDef.name!.token.value, value: scope[funcDef.name!.token.value] })

  setScopeValueAndResolveEvents(scope, funcDef.name!.token.value, new Closure(funcDef, scope))

  return Task.of("Success")
}
function topLevelClassDefinitionTask(ctx: TaskContext, decl: ParserClassDecl, scope: Scope) {

    const g = ctx.globalCompiler
    if (decl.id !== undefined) return Task.of("Success");

    decl.id = g.functionDefinitions.length;
    const classDef = new ClassDefinition(
      decl.id, decl.token.location, scope, decl.debugName,
      decl.name, decl.typeArgs, decl.body)

    g.classDefinitions.push(classDef);

    setScopeValueAndResolveEvents(scope, decl.name!.token.value, classDef)


    return Task.of("Success")

}

function compileClassTask(ctx: TaskContext, { classDef, typeArgs }: { classDef: ClassDefinition, typeArgs: unknown[] }) {
  // console.log("Compiling class", classDef)

  const type = new ConcreteClassType(null as any) // add it below
  const binding = new Binding(classDef.debugName, type);
  const body = null as any
  compilerAssert(typeArgs.length === classDef.typeArgs.length, "Expected $x type parameters for class $classDef, got $y", { x: classDef.typeArgs.length, y: typeArgs.length, classDef })
  
  if (!classDef.templatePrototype)  {
    compilerAssert(classDef.body, "Expected class body");
    classDef.templatePrototype = { name: `${classDef.debugName} class template bytecode`, body: classDef.body, instructionTable: bytecodeSecond }; // prettier-ignore
    compileFunctionPrototype(ctx, classDef.templatePrototype);
  }

  const typeParamHash = hashValues(typeArgs)
  const existing = classDef.compiledClasses.find(compiledClass => {
    if (compiledClass.typeParamHash === typeParamHash) {
      if (compiledClass.typeParameters.every((x, i) => x === typeArgs[i])) return true
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
    TaskDef(executeBytecodeTask, subCompilerState, classDef.templatePrototype!.bytecode!, templateScope)
    .chainFn((task, ast) => {
      compilerAssert(isAst(ast), "Expected ast got $ast", { ast });

      const fields: ClassField[] = []
      for (const name of Object.getOwnPropertyNames(templateScope)) {
        fields.push(new ClassField(null, name, templateScope[name].type, templateScope[name]))
      }
      const debugName = typeArgs.length === 0 ? classDef.debugName :
        `${classDef.debugName}!(...)`
      const compiledClass = new CompiledClass(
          classDef.location, debugName,
          binding, classDef, type, body, fields, [], typeParamHash)
      type.compiledClass = compiledClass
      classDef.compiledClasses.push(compiledClass)
      classDef.concreteType = type;

      return Task.of(type)

    })
  )

  
}

const getCommonType = (types: Type[]): Type => {
  compilerAssert(types.every(x => x === types[0]), "Expected types to be the same")
  return types[0];
}
const createGenericType = (globalCompiler: GlobalCompilerState, baseType: Type, argTypes: Type[]): Type => {
  const newType = new GenericType(baseType, argTypes);
  return globalCompiler.typeTable.getOrInsert(newType)
}

const topLevelLetConst = (ctx: TaskContext, expr: ParseLetConst, rootScope: Scope) => {
  const out: BytecodeOut = {
    bytecode: { code: [], locations: [] },
    table: bytecodeDefault,
    globalCompilerState: ctx.globalCompiler,
    state: { labelBlock: null }
  }
  visitParseNode(out, expr.value);
  pushGeneratedBytecode(out, { type: "halt" })
  const subCompilerState = pushSubCompilerState(ctx, { debugName: 'top level const', lexicalParent: ctx.subCompilerState, scope: ctx.subCompilerState.scope })

  ctx.globalCompiler.logger.log(makeCyan("Compiled top level def"))
  ctx.globalCompiler.logger.log(bytecodeToString(out.bytecode))
  ctx.globalCompiler.logger.log("")

  return (
    TaskDef(executeBytecodeTask, subCompilerState, out.bytecode, rootScope)
    .chainFn((task, result) => {
      setScopeValueAndResolveEvents(rootScope, expr.name.token.value, result)
      return Task.of('success')
    })
  );
}

export const runTopLevelTask = (ctx: TaskContext, stmts: ParseStatements, rootScope: Scope) => {
  const tasks: Task<unknown, unknown>[] = []
  
  stmts.exprs.forEach(expr => {
    if (expr.key === 'letconst') {
      tasks.push(TaskDef(topLevelLetConst, expr, rootScope));
      return
    }
    if (expr.key === 'function') {
      tasks.push(TaskDef(topLevelFunctionDefinitionTask, expr.functionDecl, rootScope ));
      return

    }
    if (expr.key === 'class') {
      tasks.push(TaskDef(topLevelClassDefinitionTask, expr.classDecl, rootScope ));
      return
    }
    compilerAssert(false, `Not supported at top level $key`, { key: expr.key })
  })

  return Task.concurrency(tasks);
}
