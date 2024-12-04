import { externalBuiltinBindings } from "../src/compiler_sugar";
import { CompiledFunction, IntType, BoolType, VoidType, RawPointerType, TypeField, TypeInfo, CompiledClass, SourceLocation, ConcreteClassType, FunctionParameter, SetFieldAst, PrimitiveType, VoidAst, LetAst, OperatorAst, ConstructorAst, FunctionDefinition, ReturnAst, IfAst, AndAst, WhileAst, isType, Binding, Type, BindingAst, StatementsAst, FieldAst, CallAst, Ast, NumberAst, SetAst, Capability, BreakAst, NeverType, BlockAst, compilerAssert, ParseNode, ParseFunction, ParserFunctionDecl, ParseStatements, ParseCall, ParseIdentifier, ParseString, ParseIf, ParseOperator, ParseNumber, ParseElse, ParseLet, insertTypeInfoFields, ParseSet, ParseField, ParseOpEq, ParseWhile, StringType } from "../src/defs";
import { createParameter, generateConstructor, generateMoveFunction } from "../borrow/codegen_ast";
import { ASTNode, ProgramNode, BlockStatementNode, FunctionDeclarationNode, LetConstNode, VariableDeclarationNode, LiteralNode, ExpressionStatementNode, BinaryExpressionNode, AssignmentNode, IdentifierNode, CreateStructNode, MemberExpressionNode, ReturnNode, CallExpressionNode, BuiltinNode, IfStatementNode, AndNode, WhileStatementNode, BreakStatementNode, ContinueStatementNode, CallInstruction, AllocInstruction, CommentInstruction, LoadConstantInstruction, BinaryOperationInstruction, StoreToAddressInstruction, MoveInstruction, LoadFromAddressInstruction, GetFieldPointerInstruction, IRInstruction } from "../borrow/defs";
import { RegionCodegen, IfRegion, IrFunction, Region, RegionId, SequenceId } from "./region_codegen";

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
  codegen: RegionCodegen
  
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
    this.codegen = new RegionCodegen(this.irFunction);
    this.codegen.createRootSequenceRegion();

    this.compile(decl.body)
  }

  eval(node: ParseNode): any {
    if (node instanceof ParseIdentifier) {
      return node.token.value
    }
    compilerAssert(false, 'Not implemented eval', { node });
  }


  insertCall(binding: Binding, args: string[]) {
    this.codegen.ensureBlock()
    const reg = this.newRegister()
    
    this.codegen.insertInstruction(new CallInstruction(reg, VoidType, binding, args, [], []));
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

  // enterNewSequenceRegion() {
  //   const sequence = this.codegen.insertNewSequenceRegion(this.currentRegion);
  //   this.regionSequence = sequence
  //   this.blockRegion = null
  //   return sequence
  // }

  newAlloc(type: Type) {
    const reg = this.newRegister()

    compilerAssert(this.codegen.allocBlock !== null, 'No allocBlock', { allocBlock: this.codegen.allocBlock });
    this.codegen.insertBlockInstruction(this.codegen.allocBlock, new AllocInstruction(reg, type));
    return reg
  }

  getType(typeName: string) {
    const constant = this.getConstant(typeName);
    compilerAssert(constant, 'Constant not found', { type: typeName });
    compilerAssert(isType(constant), 'Not a type', { type: typeName });
    return constant;
  }

  constructorCall(type: Type, args: ParseNode[]) {
    const reg = this.newRegister()
    const ptr = this.newAlloc(type)
    const constructor = type.typeInfo.metaobject.constructorBinding as Binding
    const constructorArgs = args.map(x => this.compile(x))
    this.codegen.ensureBlock()
    this.codegen.insertInstruction(new CommentInstruction(`Constructor ${type.shortName}`));
    this.codegen.insertInstruction(new CallInstruction(reg, VoidType, constructor, [ptr, ...constructorArgs], [], []));
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
        this.codegen.ensureBlock()
        return this.insertCall(binding, args);
      }
      const type = this.getConstant(callee)
      if (isType(type)) {
        return this.constructorCall(type, node.args)
      }
      compilerAssert(false, 'Not implemented ParseCall', { node, callee });
    }

    if (node instanceof ParseString) {
      this.codegen.ensureBlock()
      const reg = this.newRegister()
      const stringValue = node.token.value;
      const constant = this.createConstantString(stringValue)
      // this.insertInstruction(new CommentInstruction(`String ${stringValue}`));
      this.codegen.insertInstruction(new LoadConstantInstruction(reg, StringType, stringValue));
      // this.codegen.insertInstruction(new IrInstruction(reg, 'conststring', [constant]));
      return reg
    }

    if (node instanceof ParseOperator) {
      const operands = node.exprs.map(x => this.compile(x));
      const reg = this.newRegister()
      this.codegen.ensureBlock()
      this.codegen.insertInstruction(new BinaryOperationInstruction(reg, IntType, node.token.value, operands[0], operands[1], IntType));
      return reg
    }

    if (node instanceof ParseNumber) {
      this.codegen.ensureBlock()
      const reg = this.newRegister()
      this.codegen.insertInstruction(new LoadConstantInstruction(reg, IntType, Number(node.token.value)));
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
      this.codegen.ensureBlock()
      this.codegen.insertInstruction(new CommentInstruction(`Let ${name}`));
      this.codegen.insertInstruction(new StoreToAddressInstruction(ptr, VoidType, valueReg));
      return ptr
    }

    if (node instanceof ParseSet) {
      const left = this.compileLValue(node.left)
      // const name = this.eval(node.left)
      // const variable = this.getVariable(name)
      // compilerAssert(variable, 'Variable not found', { ast: node });
      const valueReg = this.compile(node.value)
      this.codegen.ensureBlock()
      this.codegen.insertInstruction(new CommentInstruction(`Set ${left}`));
      this.codegen.insertInstruction(new MoveInstruction(left, valueReg, VoidType));
      return '???'
    }

    if (node instanceof ParseOpEq) {
      const left = this.compileLValue(node.left)
      const right = this.compile(node.right)
      this.codegen.ensureBlock()
      this.codegen.insertInstruction(new BinaryOperationInstruction(left, IntType, node.token.value, left, right, IntType));
      return '???'
    }

    if (node instanceof ParseIdentifier) {
      const variable = this.getVariable(node.token.value);
      compilerAssert(variable, 'Variable not found', { ast: node });
      // compilerAssert(false, 'Not implemented ParseIdentifier', { node, variable });
      this.codegen.ensureBlock()
      const reg = this.newRegister()
      this.codegen.insertInstruction(new LoadFromAddressInstruction(reg, variable.type, variable.register));
      return reg
    }

    if (node instanceof ParseField) {
      const expr = this.compile(node.expr)
      const field = this.eval(node.field)
      const reg = this.newRegister()
      this.codegen.ensureBlock()
      this.codegen.insertInstruction(new GetFieldPointerInstruction(reg, expr, field));
      return reg
    }

    if (node instanceof ParseWhile) {
      const whileRegion = this.codegen.insertNewWhileRegion();
      this.codegen.insertChildSequenceAndPushState(whileRegion)

      this.codegen.enterRegionSequence(whileRegion, this.codegen.getWhileRegion(whileRegion).conditionSequence)
      this.compile(node.condition)

      this.codegen.enterRegionSequence(whileRegion, this.codegen.getWhileRegion(whileRegion).bodySequence)
      this.compile(node.body)

      this.codegen.popRegionState()
      return '???'
    }

    if (node instanceof ParseIf) {
      const ifRegion = this.codegen.insertNewIfRegion();
      this.codegen.insertChildSequenceAndPushState(ifRegion)

      this.codegen.enterRegionSequence(ifRegion, this.codegen.getIfRegion(ifRegion).conditionSequence)
      this.compile(node.condition)

      this.codegen.enterRegionSequence(ifRegion, this.codegen.getIfRegion(ifRegion).thenSequence)
      this.compile(node.trueBody)

      if (node.falseBody) {
        this.codegen.enterRegionSequence(ifRegion, this.codegen.getIfRegion(ifRegion).elseSequence)
        this.compile(node.falseBody)
      }

      const reg = this.newRegister()
      this.codegen.getIfRegion(ifRegion).result = reg

      this.codegen.popRegionState()
      return reg
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
      this.codegen.ensureBlock()
      this.codegen.insertInstruction(new GetFieldPointerInstruction(reg, expr, field));
      return reg
      // compilerAssert(false, 'Not implemented compileLValue', { node });
    }
    compilerAssert(false, 'Not implemented compileLValue', { node });
  }

}
