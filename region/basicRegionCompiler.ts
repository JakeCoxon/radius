import { externalBuiltinBindings } from "../src/compiler_sugar";
import { CompiledFunction, IntType, BoolType, VoidType, RawPointerType, TypeField, TypeInfo, CompiledClass, SourceLocation, ConcreteClassType, FunctionParameter, SetFieldAst, PrimitiveType, VoidAst, LetAst, OperatorAst, ConstructorAst, FunctionDefinition, ReturnAst, IfAst, AndAst, WhileAst, isType, Binding, Type, BindingAst, StatementsAst, FieldAst, CallAst, Ast, NumberAst, SetAst, Capability, BreakAst, NeverType, BlockAst, compilerAssert, ParseNode, ParseFunction, ParserFunctionDecl, ParseStatements, ParseCall, ParseIdentifier, ParseString, ParseIf, ParseOperator, ParseNumber, ParseElse, ParseLet, insertTypeInfoFields, ParseSet, ParseField, ParseOpEq, ParseWhile } from "../src/defs";
import { createParameter, generateConstructor, generateMoveFunction } from "../borrow/codegen_ast";
import { ASTNode, ProgramNode, BlockStatementNode, FunctionDeclarationNode, LetConstNode, VariableDeclarationNode, LiteralNode, ExpressionStatementNode, BinaryExpressionNode, AssignmentNode, IdentifierNode, CreateStructNode, MemberExpressionNode, ReturnNode, CallExpressionNode, BuiltinNode, IfStatementNode, AndNode, WhileStatementNode, BreakStatementNode, ContinueStatementNode } from "../borrow/defs";
import { Codegen, IfRegion, IrFunction, IrInstruction, Region } from "./region_codegen";

class Variable {
  constructor(public name: string, public type: Type, public binding: Binding, public register: string) {}
}
class Scope {
  constants: Record<string, any> = {};
  variables: Record<string, Variable> = {};
  functions: Record<string, CompiledFunction> = {};
}

export class BasicRegionCompiler {

  scopeStack: Scope[] = [new Scope()];
  rootScope: Scope = this.scopeStack[0];
  scope: Scope = this.rootScope;

  allFunctions: Map<Binding, CompiledFunction> = new Map();

  breakBinding: Binding | null = null;
  continueBinding: Binding | null = null;

  registerIndex = 0
  constantIndex = 0

  irFunction: IrFunction
  codegen: Codegen
  regionSequence: number = -1
  blockSequence: number = -1
  allocBlock: number = -1

  constantStrings: Record<string, string> = {}
  constantStringsReverse: Record<string, string> = {}

  constructor() {
    this.defineConstant('int', IntType);
    this.defineConstant('bool', BoolType);
    this.defineConstant('void', VoidType);
    this.defineConstant('ptr', RawPointerType);
    this.defineType('Point', [
      { name: 'x', type: 'int' },
      { name: 'y', type: 'int' },
    ]);
    this.defineType('Line', [
      { name: 'p1', type: 'Point' },
      { name: 'p2', type: 'Point' },
    ]);
  }

  newRegister() {
    return `r${this.registerIndex++}`
  }

  defineConstant(name: string, value: any) {
    this.scope.constants[name] = value;
  }

  getConstant(name: string) {
    let i = this.scopeStack.length - 1;
    while (i >= 0) {
      if (this.scopeStack[i].constants[name]) { return this.scopeStack[i].constants[name]; }
      i--;
    }
  }

  getVariable(name: string) {
    let i = this.scopeStack.length - 1;
    while (i >= 0) {
      if (this.scopeStack[i].variables[name]) { return this.scopeStack[i].variables[name]; }
      i--;
    }
  }

  getFunction(name: string) {
    let i = this.scopeStack.length - 1;
    while (i >= 0) {
      if (this.scopeStack[i].functions[name]) { return this.scopeStack[i].functions[name]; }
      if (this.scopeStack[i].variables[name]) { return this.scopeStack[i].variables[name]; }
      i--;
    }
  }

  defineType(name: string, fields: { name: string; type: string; }[]) {
    const typeFields: TypeField[] = [];
    const typeInfo: TypeInfo = { sizeof: 0, alignment: 0, fields: typeFields, metaobject: {}, isReferenceType: false };
    const binding = new Binding(name, VoidType);
    const compiledClass = new CompiledClass(SourceLocation.anon, name, binding, null!, null!, null!, typeFields, [], 0);
    let structType = new ConcreteClassType(compiledClass, typeInfo);

    const sourceLocation = SourceLocation.anon;
    insertTypeInfoFields(structType, fields.map(f => ({ name: f.name, sourceLocation, fieldType: this.getType(f.type) })));

    this.defineConstant(name, structType);
    const constructor = generateConstructor(name, structType);
    typeInfo.metaobject.constructorBinding = constructor.binding;

    // const moveInit = generateMoveFunction(structType, `moveInit${name}`, Capability.Set, Capability.Sink);
    // const moveAssign = generateMoveFunction(structType, `moveAssign${name}`, Capability.Inout, Capability.Sink);
    // const copyConstructor = generateMoveFunction(structType, `copy${name}`, Capability.Set, Capability.Let);

    // Object.assign(typeInfo.metaobject, { 
    //   moveInitBinding: moveInit.binding,
    //   moveAssignBinding: moveAssign.binding,
    //   copyConstructorBinding: copyConstructor.binding,
    // })

    // this.allFunctions.set(constructor.binding, constructor)
    // this.allFunctions.set(moveInit.binding, moveInit)
    // this.allFunctions.set(moveAssign.binding, moveAssign)
    // this.allFunctions.set(copyConstructor.binding, copyConstructor)

    return structType;
  }

  parseFunction(decl: ParserFunctionDecl) {
    compilerAssert(decl.body, 'Function body not found', { decl });

    this.irFunction = new IrFunction();
    this.codegen = new Codegen(this.irFunction);
    this.regionSequence = this.codegen.insertNewSequenceRegion();
    this.allocBlock = this.codegen.insertNewBlockRegion();
    this.codegen.insertSequenceChild(this.regionSequence, this.allocBlock);
    this.irFunction.root = this.regionSequence;

    this.compile(decl.body)
  }

  eval(node: ParseNode): any {
    if (node instanceof ParseIdentifier) {
      return node.token.value
    }
    compilerAssert(false, 'Not implemented eval', { node });
  }

  ensureBlock() {
    compilerAssert(this.regionSequence !== -1, 'No regionSequence', { regionSequence: this.regionSequence });
    if (this.blockSequence === -1) {
      this.blockSequence = this.codegen.insertNewBlockRegion();
      this.codegen.insertSequenceChild(this.regionSequence, this.blockSequence);
    }
  }

  insertCall(binding: Binding, args: string[]) {
    this.ensureBlock()
    const reg = this.newRegister()
    this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'call', [binding.name, ...args]));
    return reg
  }

  createConstantString(value: string) {
    if (this.constantStringsReverse[value]) {
      return this.constantStringsReverse[value]
    }
    const constant = `c${this.constantIndex++}`
    this.constantStringsReverse[value] = constant
    this.constantStrings[constant] = value
    return constant
  }

  enterNewSequenceRegion() {
    const region = this.codegen.insertNewSequenceRegion();
    this.regionSequence = region
    this.blockSequence = -1
    return region
  }

  newAlloc(type: Type) {
    const reg = this.newRegister()
    this.codegen.insertBlockInstruction(this.allocBlock, new IrInstruction(reg, 'alloc', [type.shortName]));
    return reg
  }

  constructorCall(type: Type, args: ParseNode[]) {
    const reg = this.newRegister()
    const ptr = this.newAlloc(type)
    const constructor = type.typeInfo.metaobject.constructorBinding as Binding
    const constructorArgs = args.map(x => this.compile(x))
    this.ensureBlock()
    this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction('', `// Constructor ${type.shortName}`, []));
    this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'call', [constructor.name, ptr, ...constructorArgs]));
    return reg
  }

  compile(node: ParseNode): string {

    if (node instanceof ParseStatements) {
      let r: string = 'null'
      node.exprs.forEach(expr => r = this.compile(expr));
      return r
    }

    if (node instanceof ParseCall) {
      const callee = this.eval(node.left)
      if (callee === 'print') {
        // compilerAssert(func, 'Function not found', { ast: node });
        const args = node.args.map(x => this.compile(x));
        // compilerAssert(func.parameters.length === args.length, 'Argument count mismatch', { func, args });
        // compilerAssert(func.parameters.every((param, i) => param.type === args[i].type), 'Argument type mismatch', { params: func.parameters.map(x => x.binding.type), args: args.map(x => x.type) });
        // const binding = func.binding;
        // compilerAssert(binding && binding instanceof Binding, 'Binding not found', { ast: node });
        const binding = new Binding('print', VoidType);
        this.ensureBlock()
        return this.insertCall(binding, args);
      }
      const type = this.getConstant(callee)
      if (isType(type)) {
        return this.constructorCall(type, node.args)
      }
      compilerAssert(false, 'Not implemented ParseCall', { node, callee });
    }

    if (node instanceof ParseString) {
      this.ensureBlock()
      const reg = this.newRegister()
      const stringValue = node.token.value;
      const constant = this.createConstantString(stringValue)
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction('', `// ${stringValue}`, []));
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'conststring', [constant]));
      return reg
    }

    if (node instanceof ParseIf) {
      const parentRegion = this.regionSequence

      const ifRegion = this.codegen.insertNewIfRegion();
      this.codegen.insertSequenceChild(parentRegion, ifRegion)

      const condRegion = this.enterNewSequenceRegion();
      this.codegen.insertIfCond(ifRegion, condRegion)

      this.compile(node.condition)

      const thenRegion = this.enterNewSequenceRegion();
      this.codegen.insertIfThen(ifRegion, thenRegion)

      this.compile(node.trueBody)

      if (node.falseBody) {
        const elseRegion = this.enterNewSequenceRegion();
        this.codegen.insertIfElse(ifRegion, elseRegion)

        this.compile(node.falseBody)
      }

      const reg = this.newRegister()
      ;(this.irFunction.regions[ifRegion] as IfRegion).result = reg

      this.regionSequence = parentRegion
      this.blockSequence = -1
      return reg
    }

    if (node instanceof ParseOperator) {
      const operands = node.exprs.map(x => this.compile(x));
      const reg = this.newRegister()
      this.ensureBlock()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'operator', [node.token.value, ...operands]));
      return reg
    }

    if (node instanceof ParseNumber) {
      this.ensureBlock()
      const reg = this.newRegister()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'constnum', [node.token.value]));
      return reg
    }

    if (node instanceof ParseElse) {
      return this.compile(node.body)
    }

    if (node instanceof ParseLet) {
      // const reg = this.newRegister()
      const ptr = this.newAlloc(VoidType)
      const name = this.eval(node.left)
      const valueReg = node.value ? this.compile(node.value) : 'null'
      const binding = new Binding(name, VoidType)
      const variable = new Variable(name, VoidType, binding, ptr)
      this.scope.variables[name] = variable
      this.ensureBlock()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction('', `// Let ${name}`, []));
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction('', 'store', [ptr, valueReg]));
      return ptr
    }

    if (node instanceof ParseSet) {
      const left = this.compileLValue(node.left)
      // const name = this.eval(node.left)
      // const variable = this.getVariable(name)
      // compilerAssert(variable, 'Variable not found', { ast: node });
      const valueReg = this.compile(node.value)
      this.ensureBlock()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction('', `// Set`, []));
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction('', 'move', [left, valueReg]));
      return '???'
    }

    if (node instanceof ParseOpEq) {
      const left = this.compileLValue(node.left)
      const right = this.compile(node.right)
      this.ensureBlock()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction('', 'operator', [node.token.value, left, right]));
      return '???'
    }

    if (node instanceof ParseIdentifier) {
      const variable = this.getVariable(node.token.value);
      compilerAssert(variable, 'Variable not found', { ast: node });
      // compilerAssert(false, 'Not implemented ParseIdentifier', { node, variable });
      this.ensureBlock()
      const reg = this.newRegister()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'load', [variable.register]));
      return reg
    }

    if (node instanceof ParseField) {
      const expr = this.compile(node.expr)
      const field = this.eval(node.field)
      const reg = this.newRegister()
      this.ensureBlock()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'field', [expr, field]));
      return reg
    }

    if (node instanceof ParseWhile) {
      const parentRegion = this.regionSequence

      const whileRegion = this.codegen.insertNewWhileRegion();
      this.codegen.insertSequenceChild(parentRegion, whileRegion)

      const condRegion = this.enterNewSequenceRegion();
      this.codegen.insertWhileCond(whileRegion, condRegion)

      this.compile(node.condition)

      const bodyRegion = this.enterNewSequenceRegion();
      this.codegen.insertWhileBody(whileRegion, bodyRegion)

      this.compile(node.body)

      this.regionSequence = parentRegion
      this.blockSequence = -1
      return '???'
    }

    compilerAssert(false, 'compile ParseNode Not implemented', { node });
  }

  compileLValue(node: ParseNode): string {
    if (node instanceof ParseIdentifier) {
      const variable = this.getVariable(node.token.value);
      compilerAssert(variable, 'Variable not found', { ast: node });
      return variable.register
    }
    if (node instanceof ParseField) {
      const expr = this.compileLValue(node.expr)
      const field = this.eval(node.field)
      const reg = this.newRegister()
      this.ensureBlock()
      this.codegen.insertBlockInstruction(this.blockSequence, new IrInstruction(reg, 'fieldoffset', [expr, field]));
      return reg
      // compilerAssert(false, 'Not implemented compileLValue', { node });
    }
    compilerAssert(false, 'Not implemented compileLValue', { node });
  }

    // if (node instanceof ProgramNode) {
    //   const body = new BlockStatementNode(node.body);
    //   return this.compile(new FunctionDeclarationNode('main', [], 'void', body));
    // }

    // if (node instanceof BlockStatementNode) {
    //   const statements = node.body.map(x => this.compile(x));
    //   const type = statements.length ? statements[statements.length - 1].type : VoidType;
    //   return new StatementsAst(type, SourceLocation.anon, statements);
    // }

    // if (node instanceof LetConstNode) {
    //   this.defineConstant(node.name, node.value);
    //   return new VoidAst(VoidType, SourceLocation.anon);
    // }

    // if (node instanceof VariableDeclarationNode) {
    //   const type = this.getType(node.type);
    //   const binding = new Binding(node.name, type);
    //   this.scope.variables[node.name] = binding;
    //   const value = node.initializer ? this.compile(node.initializer) : null;
    //   return new LetAst(VoidType, SourceLocation.anon, binding, value, node.mutable);
    // }

    // if (node instanceof LiteralNode) {
    //   if (typeof node.value === 'number') {
    //     return new NumberAst(IntType, SourceLocation.anon, node.value);
    //   }
    //   compilerAssert(false, 'Not implemented LiteralNode', { ast: node });
    // }

    // if (node instanceof ExpressionStatementNode) {
    //   const expression = this.compile(node.expression);
    //   return expression;
    // }

    // if (node instanceof BinaryExpressionNode) {
    //   const left = this.compile(node.left);
    //   const right = this.compile(node.right);
    //   compilerAssert(left.type === right.type, 'Type mismatch', { left, right });
    //   return new OperatorAst(left.type, SourceLocation.anon, node.operator, [left, right]);
    // }

    // if (node instanceof AssignmentNode) {
    //   const left = this.compile(node.left);
    //   const right = this.compile(node.right);
    //   compilerAssert(left.type === right.type, 'Type mismatch', { left, right, leftType: left.type, rightType: right.type });
    //   if (left instanceof BindingAst) {
    //     return new SetAst(left.type, SourceLocation.anon, left.binding, right);
    //   }
    //   if (left instanceof FieldAst) {
    //     return new SetFieldAst(VoidType, SourceLocation.anon, left.left, left.field, right);
    //   }
    //   compilerAssert(false, 'Not implemented AssignmentNode', { left, right });
    // }

    // if (node instanceof IdentifierNode) {
    //   const binding = this.getVariable(node.name);
    //   compilerAssert(binding, 'Variable not found', { ast: node });
    //   return new BindingAst(binding.type, SourceLocation.anon, binding);
    // }

    // if (node instanceof CreateStructNode) {
    //   const type = this.getType(node.name);
    //   const fields = node.fields.map(x => this.compile(x));
    //   // const binding = type.typeInfo.metaobject.constructorBinding as Binding
    //   // const call = new CallAst(type, SourceLocation.anon, binding, fields, [])
    //   // const letBinding = new Binding('temp', type)
    //   // const let_ = new LetAst(type, SourceLocation.anon, letBinding, null, true)
    //   // return new StatementsAst(type, SourceLocation.anon, [let_, call])
    //   return new ConstructorAst(type, SourceLocation.anon, fields);
    // }

    // if (node instanceof MemberExpressionNode) {
    //   const object = this.compile(node.object);
    //   const field = node.property;
    //   const type = object.type;
    //   const fieldIndex = type.typeInfo.fields.findIndex(x => x.name === field);
    //   compilerAssert(fieldIndex >= 0, 'Field not found', { object, field, type });
    //   return new FieldAst(type.typeInfo.fields[fieldIndex].fieldType, SourceLocation.anon, object, type.typeInfo.fields[fieldIndex]);
    // }

    // if (node instanceof FunctionDeclarationNode) {
    //   compilerAssert(!this.getFunction(node.name), 'Function already defined', { ast: node });
    //   this.scopeStack.push(new Scope());
    //   this.scope = this.scopeStack[this.scopeStack.length - 1];

    //   const concreteTypes = node.params.map(x => this.getType(x.type));
    //   const argBindings = node.params.map((x, i) => new Binding(x.name, concreteTypes[i]));

    //   argBindings.forEach((param, i) => {
    //     this.scope.variables[param.name] = param;
    //   });
    //   const body = this.compile(node.body);

    //   this.scopeStack.pop();
    //   this.scope = this.scopeStack[this.scopeStack.length - 1];

    //   const binding = new Binding(node.name, VoidType);
    //   const returnType = this.getType(node.returnType);
    //   const funcDef: FunctionDefinition = { debugName: node.name } as any; // Just need debugName for now

    //   const parameters = argBindings.map((argBinding, i) => {
    //     return createParameter(argBinding, node.params[i].capability);
    //   });

    //   const compiledFunc = new CompiledFunction(binding, funcDef, returnType, concreteTypes, body, argBindings, parameters, [], 0);
    //   this.scope.functions[node.name] = compiledFunc;
    //   this.allFunctions.set(binding, compiledFunc);

    //   return new VoidAst(VoidType, SourceLocation.anon);
    // }

    // if (node instanceof ReturnNode) {
    //   const value = node.argument ? this.compile(node.argument) : null;
    //   return new ReturnAst(VoidType, SourceLocation.anon, value);
    // }

    // if (node instanceof CallExpressionNode) {
    //   const func = this.getFunction(node.callee);
    //   compilerAssert(func, 'Function not found', { ast: node });
    //   const args = node.args.map(x => this.compile(x));
    //   compilerAssert(func.parameters.length === args.length, 'Argument count mismatch', { func, args });
    //   compilerAssert(func.parameters.every((param, i) => param.type === args[i].type), 'Argument type mismatch', { params: func.parameters.map(x => x.binding.type), args: args.map(x => x.type) });
    //   const binding = func.binding;
    //   compilerAssert(binding && binding instanceof Binding, 'Binding not found', { ast: node });
    //   return new CallAst(func.returnType, SourceLocation.anon, binding, args, []);
    // }

    // if (node instanceof BuiltinNode) {
    //   const value = this.compile(node.value);
    //   if (node.name === 'print') {
    //     if (value.type === IntType) return new CallAst(VoidType, SourceLocation.anon, externalBuiltinBindings.printInt, [value], []);
    //     return new CallAst(VoidType, SourceLocation.anon, externalBuiltinBindings.print, [value], []);
    //   } else if (node.name === 'copy') {
    //     return new CallAst(value.type, SourceLocation.anon, externalBuiltinBindings.copy, [value], []);
    //   } else compilerAssert(false, 'Builtin not found', { ast: node });
    // }

    // if (node instanceof IfStatementNode) {
    //   const test = this.compile(node.condition);
    //   const consequent = this.compile(node.consequent);
    //   const alternate = node.alternate ? this.compile(node.alternate) : null;
    //   return new IfAst(VoidType, SourceLocation.anon, test, consequent, alternate);
    // }

    // if (node instanceof AndNode) {
    //   const left = this.compile(node.left);
    //   const right = this.compile(node.right);
    //   return new AndAst(BoolType, SourceLocation.anon, [left, right]);
    // }

    // if (node instanceof WhileStatementNode) {
    //   const prevBreak = this.breakBinding
    //   const prevContinue = this.continueBinding
    //   const breakBinding = this.breakBinding = new Binding('break', VoidType)
    //   const continueBinding = this.continueBinding = new Binding('continue', VoidType)
    //   const test = this.compile(node.condition)
    //   const body = this.compile(node.body)
    //   this.breakBinding = prevBreak
    //   this.continueBinding = prevContinue
    //   return new BlockAst(VoidType, SourceLocation.anon, breakBinding, null,
    //      new WhileAst(VoidType, SourceLocation.anon, test, 
    //         new BlockAst(VoidType, SourceLocation.anon, continueBinding, null, body)
    //      )
    //   )
    // }

    // if (node instanceof BreakStatementNode) {
    //   compilerAssert(this.breakBinding, 'Break outside of loop', { ast: node });
    //   return new BreakAst(NeverType, SourceLocation.anon, this.breakBinding, null)
    // }

    // if (node instanceof ContinueStatementNode) {
    //   compilerAssert(this.continueBinding, 'Continue outside of loop', { ast: node });
    //   return new BreakAst(NeverType, SourceLocation.anon, this.continueBinding, null)
    // }

  // }

  getType(typeName: string) {
    const constant = this.getConstant(typeName);
    compilerAssert(constant, 'Constant not found', { type: typeName });
    compilerAssert(isType(constant), 'Not a type', { type: typeName });
    return constant;
  }
}
