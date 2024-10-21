import { externalBuiltinBindings } from "../src/compiler_sugar";
import { AndAst, Ast, Binding, BindingAst, BlockAst, BoolAst, BreakAst, CallAst, Capability, CastAst, CompiledFunction, ConstructorAst, DefaultConsAst, FieldAst, FunctionParameter, IfAst, IntType, LetAst, NotAst, NumberAst, OperatorAst, OrAst, ParameterizedType, PrimitiveType, RawPointerType, ReturnAst, SetAst, SetFieldAst, SetSubscriptAst, SetValueFieldAst, SourceLocation, StatementsAst, StringAst, SubscriptAst, Type, UserCallAst, ValueFieldAst, VoidAst, VoidType, WhileAst } from "../src/defs";
import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, Pointer, Value, ReturnInstruction, ReturnNode, StoreToAddressInstruction, Variable, VariableDeclarationNode, WhileStatementNode, compilerAssert, GetFieldPointerInstruction, AndNode, OrNode, PhiInstruction, CommentInstruction, MoveInstruction, EndAccessInstruction, printIR, MarkInitializedInstruction, InstructionId, PhiSource, DeallocStackInstruction, PointerOffsetInstruction } from "./defs";

type ExpressionContext = {
  valueCategory: 'rvalue' | 'lvalue';
}

class Scope {
  allocs: [string, Type][] = []
  breakBlockLabel: string | null = null
}

export class CodeGenerator {
  functionBlocks: FunctionBlock[] = [];
  // currentFunction: FunctionBlock
  labelCount: number = 0;
  registerCount: number = 0;
  functions: Map<Binding, CompiledFunction> = new Map();

  functionGenerator(compiledFunction: CompiledFunction) {
    return new FunctionCodeGenerator(this, compiledFunction)
  }

  newLabel(): string {
    return `L${this.labelCount++}`;
  }

  newRegister(): string {
    return `r${this.registerCount++}`;
  }

}

export class FunctionCodeGenerator {
  variableMap: Map<Binding, Variable> = new Map(); // variable bidning -> register name
  blocks: BasicBlock[] = [];
  currentBlock: BasicBlock;
  functionInstructions: IRInstruction[] = [];
  scopes: Scope[] = [];
  blockScopeDepth: Map<Binding, number> = new Map(); // block binding -> scope depth

  currentFunction: FunctionBlock;

  unusedBlocks: Set<string> = new Set();

  constructor(
    public codegen: CodeGenerator,
    public compiledFunction: CompiledFunction
  ) {}

  newLabel(): string {
    return this.codegen.newLabel();
  }
  newRegister(): string {
    return this.codegen.newRegister();
  }
  
  newBlock(label: string): BasicBlock {
    const block = new BasicBlock(label, []);
    this.blocks.push(block);
    this.currentBlock = block;
    return block;
  }

  addInstruction(instr: IRInstruction) {
    this.currentBlock.instructions.push(instr);
  }

  // TODO: Clean this up because it's not clear exactly why it's needed
  // Is it only primitive types? If it's copying a value, make it explicit
  toValue(type: Type, value: IRValue): Value {
    if (value instanceof Value) { return value; }
    // compilerAssert(type instanceof PrimitiveType, 'Only allowed on primitive types', { type });
    const reg = this.newRegister();
    const accessReg = this.newRegister();
    compilerAssert(value.address, 'Value must have an address', { type, value });
    this.addInstruction(new CommentInstruction(`Convert to value ${value.address}`));
    this.addInstruction(new AccessInstruction(accessReg, value.address, [Capability.Let], type));
    this.addInstruction(new LoadFromAddressInstruction(reg, type, accessReg));
    // this.addInstruction(new MarkInitializedInstruction(value.address, type, false));
    return new Value(reg);
  }

  // Entry point
  generateFunction(binding: Binding, params: FunctionParameter[], returnType: Type, body: Ast) {
    compilerAssert(!this.currentFunction, 'Already generating in a function');
    console.log("Begin generating function", binding.name);

    const entryLabel = this.newLabel();
    const entryBlock = new BasicBlock(entryLabel, []);
    this.blocks.push(entryBlock);
    this.currentBlock = entryBlock;

    this.scopes.push(new Scope());

    const paramRegs = params.map((param) => {
      const paramReg = this.newRegister()
      this.variableMap.set(param.binding, new Variable(param.binding.name, param.type, paramReg, param.capability));

      return paramReg
    });

    this.currentFunction = new FunctionBlock(binding.name, binding, params, paramRegs, this.blocks);

    if (returnType !== VoidType) {
      this.generate(new ReturnAst(body.type, SourceLocation.anon, body))
    } else {
      this.generate(body);
      this.finalizeScope();
      this.addInstruction(new ReturnInstruction(null));
    }

    this.blocks[0].instructions.unshift(...this.functionInstructions);

    // Remove unused blocks
    this.currentFunction.blocks = this.blocks.filter(block => !this.unusedBlocks.has(block.label));

    this.codegen.functionBlocks.push(this.currentFunction)

    return this.currentFunction
  }

  generate(ast: Ast): void {

    if (ast instanceof StatementsAst) {
      for (const stmt of ast.statements) {
        this.generate(stmt);
      }
      return
    }
    if (ast instanceof VoidAst) return

    if (ast instanceof LetAst)      { return this.generateVariableDeclaration(ast) }
    if (ast instanceof SetAst)      { return this.generateAssignmentStatement(ast) }
    if (ast instanceof IfAst)       { return this.generateIfStatement(ast) }
    if (ast instanceof WhileAst)    { return this.generateWhileStatement(ast) }
    if (ast instanceof ReturnAst)   { return this.generateReturnStatement(ast) }
    if (ast instanceof SetFieldAst) { return this.generateAssignmentField(ast) }
    if (ast instanceof SetValueFieldAst) { return this.generateAssignmentValueField(ast) }
    if (ast instanceof BreakAst)    { return this.generateBreakStatement(ast) }
    if (ast instanceof BlockAst)    { return this.generateBlockStatement(ast) }
    if (ast instanceof SetSubscriptAst) { return this.generateAssignmentSubscript(ast) }

    this.generateExpression(ast, { valueCategory: 'rvalue' })
    
  }

  generateExpression(ast: Ast, context: ExpressionContext): IRValue {
    if (ast instanceof StringAst)      { return this.generateStringLiteral(ast, context) }
    if (ast instanceof NumberAst)      { return this.generateNumberLiteral(ast, context) }
    if (ast instanceof BoolAst)        { return this.generateBoolLiteral(ast, context) }
    if (ast instanceof CallAst)        { return this.generateCallExpression(ast, context) }
    if (ast instanceof UserCallAst)    { return this.generateUserCallExpression(ast, context) }
    if (ast instanceof ConstructorAst) { return this.generateCreateStructExpression(ast, context) }
    if (ast instanceof DefaultConsAst) { return this.generateDefaultConstructorExpression(ast, context) }
    if (ast instanceof BindingAst)     { return this.generateBinding(ast, context) }
    if (ast instanceof FieldAst)       { return this.generateMemberExpression(ast, context) }
    if (ast instanceof ValueFieldAst)  { return this.generateValueFieldExpression(ast, context) }
    if (ast instanceof SubscriptAst)   { return this.generateSubscriptExpression(ast, context) }
    if (ast instanceof AndAst)         { return this.generateAndExpression(ast, context) }
    if (ast instanceof OrAst)          { return this.generateOrExpression(ast, context) }
    if (ast instanceof OperatorAst)    { return this.generateBinaryExpression(ast, context) }
    if (ast instanceof IfAst)          { return this.generateIfExpression(ast, context) }
    if (ast instanceof NotAst)         { return this.generateNotExpression(ast, context) }
    if (ast instanceof BlockAst)       { return this.generateBlockExpression(ast, context) }
    if (ast instanceof CastAst)        { return this.generateCastExpression(ast, context) }
    if (ast instanceof StatementsAst) {
      for (const stmt of ast.statements.slice(0, -1)) {
        this.generate(stmt);
      }
      if (ast.type !== VoidType) {
        return this.generateExpression(ast.statements[ast.statements.length - 1], context);
      } else {
        this.generate(ast.statements[ast.statements.length - 1])
        return new Pointer('')
      }
    }
    
    compilerAssert(false, 'Not implemented expression', { ast })
  }

  _createUnusedBlock() {
    // This is a trick to make sure that subsequent instructions
    // are generated but are not added to the final IR. This is
    // because no statements can come after a return/break/continue
    const afterLabel = this.newLabel();
    this.newBlock(afterLabel);
    this.addInstruction(new CommentInstruction('Unused block'));
    this.unusedBlocks.add(afterLabel);
  }

  generateAlloc(type: Type) {
    compilerAssert(!type.typeInfo.isReferenceType, "Not implemented reference type", { type })
    const reg = this.newRegister()
    this.functionInstructions.push(new AllocInstruction(reg, type))
    this.scopes[this.scopes.length - 1].allocs.push([reg, type])
    return reg
  }

  finalizeScope() {
    compilerAssert(this.scopes.length > 0, 'No scopes to close');
    const scope = this.scopes[this.scopes.length - 1]
    const allocs = [...scope.allocs].reverse()
    for (const alloc of allocs) {
      this.addInstruction(new DeallocStackInstruction(alloc[0], alloc[1]))
    }
  }

  // TODO: Fold these together
  generateBlockStatement(ast: BlockAst) {
    const label = this.newLabel()
    const scope = new Scope()
    scope.breakBlockLabel = label
    this.scopes.push(scope);
    this.blockScopeDepth.set(ast.binding, this.scopes.length - 1)
    this.generate(ast.body)
    this.finalizeScope()
    this.addInstruction(new JumpInstruction(label))
    this.scopes.pop()
    this.newBlock(label)
  }

  generateBlockExpression(ast: BlockAst, context: ExpressionContext): IRValue {
    const label = this.newLabel()
    const resultPtr = this.generateAlloc(ast.type)
    const scope = new Scope()
    scope.breakBlockLabel = label
    this.scopes.push(scope);
    this.blockScopeDepth.set(ast.binding, this.scopes.length - 1)
    const value = this.generateExpression(ast.body, context)
    // TODO: Figure this out cleanly
    if (value instanceof Value) {
      this.generateMoveInstruction(resultPtr, value, ast.type)
    } else {
      this.generateMovePointerInstruction(resultPtr, value, ast.type)
    }
    this.finalizeScope()
    this.addInstruction(new JumpInstruction(label))
    this.scopes.pop()
    this.newBlock(label)
    return new Pointer(resultPtr)
  }

  generateBreakStatement(ast: BreakAst) {
    const depth = this.blockScopeDepth.get(ast.binding)
    compilerAssert(depth !== undefined, `Block depth not found: ${ast.binding.name}`)
    const scope = this.scopes[depth]
    compilerAssert(scope, `Block scope not found: ${ast.binding.name}`)
    const label = scope.breakBlockLabel
    compilerAssert(label, `Break label not found: ${ast.binding.name}`)
    this.addInstruction(new JumpInstruction(label))
    this._createUnusedBlock()
  }

  generateReturnStatement(ast: ReturnAst) {
    
    if (!ast.expr) {
      this.finalizeScope()
      this.addInstruction(new ReturnInstruction(null));
    } else {
      compilerAssert(ast.type !== VoidType, 'Return type must not be void', { ast })
      const returnReg = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
      if (ast.expr.type instanceof PrimitiveType) {
        const value = this.toValue(ast.type, returnReg)
        this.finalizeScope()
        this.addInstruction(new ReturnInstruction(value.register));
      } else {
        // Not implemented yet. try it out with tests
        // const value = this.toValue(ast.type, returnReg)
        // this.finalizeScope()
        // this.addInstruction(new ReturnInstruction(value.register));
        compilerAssert(false, 'Not implemented return statement', { ast })
      }
    }
    this._createUnusedBlock();
  }

  generateCastExpression(ast: CastAst, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const v = this.toValue(ast.type, value)
    const reg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Cast ${ast.type.shortName}`))
    this.addInstruction(new BinaryOperationInstruction(reg, ast.type, 'cast', v.register, '', ast.expr.type));
    return new Value(reg);
  }

  generateVariableDeclaration(ast: LetAst) {
    const mutable = ast.mutable
    this.addInstruction(new CommentInstruction(`${mutable ? 'var' : 'let'} ${ast.binding.name}`))
    const value = ast.value ? this.generateExpression(ast.value, { valueCategory: 'rvalue' }) : null

    if (mutable) {
      this.generateMutableVariableDeclaration(ast, value);
    } else {
      this.generateProjection(ast, value);
    }
  }

  generateProjection(ast: LetAst, value: IRValue | null) {
    const type = ast.binding.type
    const reg = this.newRegister();
    const capability = Capability.Let
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, capability));
    compilerAssert(value, 'Let binding must have an initializer');
    const ptr = this.storeResult(type, value)
    // compilerAssert(value instanceof Pointer, 'Let binding must have an lvalue initializer');
    this.addInstruction(new CommentInstruction(`Projection ${ast.binding.name}`))
    this.addInstruction(new AccessInstruction(reg, ptr.address, [Capability.Let], type));
  }

  generateMutableVariableDeclaration(ast: LetAst, value: IRValue | null) {
    const type = ast.binding.type
    const mutable = ast.mutable
    const capability = mutable ? Capability.Inout : Capability.Let
    const reg = this.generateAlloc(type);
    
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, capability));
    
    if (!value) return
    if (value instanceof Value) {
      this.generateMoveInstruction(reg, value, type)
    } else {
      this.generateMovePointerInstruction(reg, value, type)
    }
  }

  // TODO: Fold these together
  generateIfStatement(ast: IfAst) {
    const conditionReg = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    compilerAssert(conditionReg instanceof Value, 'If condition must be an RValue');
    const thenLabel = this.newLabel();
    const elseLabel = this.newLabel();
    const afterLabel = this.newLabel();

    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, thenLabel, elseLabel));

    this.newBlock(thenLabel);
    this.generate(ast.trueBody);
    this.addInstruction(new JumpInstruction(afterLabel));

    this.newBlock(elseLabel);
    if (ast.falseBody) this.generate(ast.falseBody);
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
  }

  generateIfExpression(ast: IfAst, context: ExpressionContext): IRValue {
    const conditionValue = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const conditionReg = this.toValue(ast.type, conditionValue)
    const outReg = this.newRegister();
    const thenLabel = this.newLabel();
    const elseLabel = this.newLabel();
    const afterLabel = this.newLabel();
    compilerAssert(ast.falseBody, 'If expression must have a false body');

    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, thenLabel, elseLabel));

    this.newBlock(thenLabel);
    const expr1 = this.generateExpression(ast.trueBody, context);
    const value1 = this.toValue(ast.type, expr1)
    const phiSource1 = new PhiSource(value1.register, this.currentBlock.label)
    this.addInstruction(new JumpInstruction(afterLabel));

    this.newBlock(elseLabel);
    const expr2 = this.generateExpression(ast.falseBody, context);
    const value2 = this.toValue(ast.type, expr2)
    const phiSource2 = new PhiSource(value2.register, this.currentBlock.label)
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);

    this.addInstruction(new PhiInstruction(outReg, ast.type, [phiSource1, phiSource2]))
    return new Value(outReg);
  }

  generateWhileStatement(ast: WhileAst) {
    const conditionLabel = this.newLabel();
    const bodyLabel = this.newLabel();
    const afterLabel = this.newLabel();

    // Jump to condition check
    this.addInstruction(new JumpInstruction(conditionLabel));

    const conditionBlock = new BasicBlock(conditionLabel, []);
    this.blocks.push(conditionBlock);
    this.currentBlock = conditionBlock;
    const conditionReg = this.generateExpression(ast.condition, { valueCategory: 'rvalue' });
    compilerAssert(conditionReg instanceof Value, 'While condition must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, bodyLabel, afterLabel));

    this.newBlock(bodyLabel);
    this.generate(ast.body);
    this.addInstruction(new JumpInstruction(conditionLabel));
    this.newBlock(afterLabel);
  }

  generateAndExpression(ast: AndAst, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'and-expression must be an RValue');
    const rhsLabel = this.newLabel();
    const afterLabel = this.newLabel();
    const outReg = this.newRegister();
    const lhsReg = this.generateExpression(ast.args[0], { valueCategory: 'rvalue' });
    compilerAssert(lhsReg instanceof Value, 'Left-hand side of && must be an RValue');
    const phiSource1 = new PhiSource(lhsReg.register, this.currentBlock.label)
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, rhsLabel, afterLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(ast.args[1], { valueCategory: 'rvalue' });
    compilerAssert(rhsReg instanceof Value, 'Right-hand side of && must be an RValue');
    const phiSource2 = new PhiSource(rhsReg.register, this.currentBlock.label)
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
    this.addInstruction(new PhiInstruction(outReg, ast.type, [phiSource1, phiSource2]))
    return new Value(outReg);
  }

  generateOrExpression(ast: OrAst, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'or-expression must be an RValue');
    const rhsLabel = this.newLabel();
    const afterLabel = this.newLabel();
    const outReg = this.newRegister();
    const lhsReg = this.generateExpression(ast.args[0], { valueCategory: 'rvalue' });
    compilerAssert(lhsReg instanceof Value, 'Left-hand side of || must be an RValue');
    const phiSource1 = new PhiSource(lhsReg.register, this.currentBlock.label)
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, afterLabel, rhsLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(ast.args[1], { valueCategory: 'rvalue' });
    compilerAssert(rhsReg instanceof Value, 'Right-hand side of || must be an RValue');
    const phiSource2 = new PhiSource(rhsReg.register, this.currentBlock.label)
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
    this.addInstruction(new PhiInstruction(outReg, ast.type, [phiSource1, phiSource2]))
    return new Value(outReg);
  }

  generateCreateStructExpression(ast: ConstructorAst, context: ExpressionContext): IRValue {
    // compilerAssert(context.valueCategory === 'rvalue', 'Struct creation must be an RValue');
    const structType = ast.type
    compilerAssert(structType, `Struct type not found`);
    compilerAssert(ast.args.length === structType.typeInfo.fields.length, 'Field count mismatch');
    const fnBinding = structType.typeInfo.metaobject.constructorBinding
    compilerAssert(fnBinding && fnBinding instanceof Binding, `Constructor not found for ${structType.shortName}`);

    const structReg = this.generateAlloc(structType)
    
    // this.generateCallExpression(new CallAst(VoidType, SourceLocation.anon, fnBinding, [...ast.args], []), context)
    const fn = this.codegen.functions.get(fnBinding)
    compilerAssert(fn, `Function ${fnBinding.name} not found`);

    const fields = structType.typeInfo.fields
    const argRegs: string[] = [];

    const structAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(structAccessReg, structReg, [Capability.Set], structType));
    argRegs.push(structAccessReg)

    for (let i = 0; i < fields.length; i++) {
      const param = fn.parameters[i + 1]
      const reg = this.generateFunctionArgument(ast.args[i], param.reference, param.passingType, Capability.Sink)
      argRegs.push(reg)
    }
    this.addInstruction(new CallInstruction(null, VoidType, fnBinding, argRegs, fn.parameters.map(b => b.type), fn.parameters.map(b => b.capability)))
    // this.addInstruction(new MarkInitializedInstruction(structReg, true));
    return new Pointer(structReg)
  }

  generateDefaultConstructorExpression(ast: DefaultConsAst, context: ExpressionContext): IRValue {
    const structType = ast.type
    compilerAssert(structType, `Struct type not found`);
    const structReg = this.generateAlloc(structType)
    this.addInstruction(new CommentInstruction(`Default constructor ${structType.shortName}`))
    return new Pointer(structReg)
  }

  storeResult(type: Type, value: IRValue) {
    if (value instanceof Pointer) return value
    const reg = this.generateAlloc(type);
    this.generateMoveInstruction(reg, value, type)
    return new Pointer(reg)
  }

  generateFunctionArgument(ast: Ast, reference: boolean, passingType: Type, capability: Capability): string {

    // @ParameterPassing
    const newReg = this.newRegister();
    if (reference) {
      const argReg = this.generateExpression(ast, { valueCategory: 'lvalue' });
      compilerAssert(argReg instanceof Pointer, 'Function argument must be an pointer', { ast, capability, passingType });
      this.addInstruction(new AccessInstruction(newReg, argReg.address, [capability], passingType));
      return newReg
    }

    const argReg = this.generateExpression(ast, { valueCategory: 'rvalue' });
    // if (ast.type instanceof PrimitiveType) {
      const value = this.toValue(ast.type, argReg)
      this.addInstruction(new AccessInstruction(newReg, value.register, [capability], passingType));
    // } else {
      // compilerAssert(argReg instanceof Pointer, "Expected pointer")
      // this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Let]));
    // }

    return newReg

  }

  generateCopy(ast: Ast, context: ExpressionContext): IRValue {
    const binding = new Binding('copy', ast.type)
    if (ast.type instanceof PrimitiveType) {
      const targetAccessReg = this.newRegister();
      const type = ast.type
      const source = this.generateExpression(ast, { valueCategory: 'rvalue' })
      let valueReg = source instanceof Value ? source.register : this.newRegister()
      if (source instanceof Pointer) {
        const sourceAccessReg = this.newRegister();
        // Load from pointer, using a let capability because it is a primitive type and a copy is safe
        this.addInstruction(new AccessInstruction(sourceAccessReg, source.address, [Capability.Let], type));
        this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg));
      }
      const targetPointer = this.generateAlloc(type);
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type));
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg));
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]));
      return new Pointer(targetPointer)
    }
    
    this.generate(new LetAst(VoidType, SourceLocation.anon, binding, null, true))
    const bindingAst = new BindingAst(binding.type, SourceLocation.anon, binding)
    const copyConstructor = ast.type.typeInfo.metaobject.copyConstructorBinding
    compilerAssert(copyConstructor && copyConstructor instanceof Binding, `Copy constructor not found for ${ast.type.shortName}`);
    this.generateCallExpression(new CallAst(ast.type, SourceLocation.anon, copyConstructor, [bindingAst, ast], []), context)
    return this.generateBinding(new BindingAst(binding.type, SourceLocation.anon, binding), context)
  }

  generatePrint(ast: Ast, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast, { valueCategory: 'rvalue' });
    const valueReg = this.toValue(ast.type, value)
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.print, [valueReg.register], [ast.type], [Capability.Let]));
    return new Pointer('')
  }

  generatePrintf(args: Ast[], context: ExpressionContext): IRValue {
    const format = this.generateExpression(args[0], { valueCategory: 'rvalue' });
    const formatReg = this.toValue(args[0].type, format)
    const argRegs: string[] = [];
    for (let i = 1; i < args.length; i++) {
      const arg = this.generateExpression(args[i], { valueCategory: 'rvalue' });
      const argReg = this.toValue(args[i].type, arg)
      argRegs.push(argReg.register)
    }
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.printf, [formatReg.register, ...argRegs], args.map(a => a.type), args.map(a => Capability.Let)));
    return new Pointer('')
  }

  generateCallExpression(ast: CallAst, context: ExpressionContext): IRValue {
    if (ast.binding === externalBuiltinBindings.copy) {
      return this.generateCopy(ast.args[0], context)
    } else if (ast.binding === externalBuiltinBindings.print) {
      return this.generatePrint(ast.args[0], context)
    } else if (ast.binding === externalBuiltinBindings.printf) {
      return this.generatePrintf(ast.args, context)
    } else if (ast.binding === externalBuiltinBindings.initializer) {
      return new Pointer('')
    }
    compilerAssert(ast.binding instanceof Binding, 'Expected binding', { ast });

    // Generate code for arguments
    const fn = this.codegen.functions.get(ast.binding)
    compilerAssert(fn, `Function ${ast.binding.name} not found`, {
      c: this.codegen.functions
    });
    this.addInstruction(new CommentInstruction(`Call ${ast.binding.name}`))

    compilerAssert(ast.args.length === fn.argBindings.length, 'Argument count mismatch', { binding: ast.binding, got: ast.args.length, expected: fn.argBindings.length });
    
    const argRegs: string[] = [];
    let i = 0
    for (const givenArg of ast.args) {
      const argIndex = i++;
      const param = fn.parameters[argIndex]
      compilerAssert(param.capability, `Capability not found for ${param.binding.name}`);
      const argReg = this.generateFunctionArgument(givenArg, param.reference, param.passingType, param.capability);
      argRegs.push(argReg);
    }

    // Call the function
    if (fn.returnType === VoidType) {
      this.addInstruction(new CallInstruction(null, fn.returnType, ast.binding, argRegs, fn.parameters.map(b => b.type), fn.parameters.map(b => b.capability)))
      return new Pointer('') // hack. Make sure this is not used
    }
    const accessReg = this.newRegister()
    const resultReg = this.generateAlloc(fn.returnType);
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set], fn.returnType));
    this.addInstruction(new CallInstruction(accessReg, fn.returnType, ast.binding, argRegs, fn.parameters.map(b => b.type), fn.parameters.map(b => b.capability)))
    return new Pointer(resultReg)
  }

  generateUserCallExpression(ast: UserCallAst, context: ExpressionContext): IRValue {
    return this.generateCallExpression(new CallAst(ast.type, SourceLocation.anon, ast.binding, ast.args, []), context)
  }

  generateAssignmentStatement(ast: SetAst) {
    let variable = this.variableMap.get(ast.binding)
    compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
    const type = variable.type

    const newLocal = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const lvalue = this.storeResult(type, newLocal)

    this.generateMovePointerInstruction(variable.register, lvalue, type)
    compilerAssert(variable.capability === Capability.Inout || variable.capability === Capability.Set, 'Cannot assign to a let variable');
  }

  ensureMutable(ast: Ast): boolean {
    if (ast instanceof BindingAst) {
      const variable = this.variableMap.get(ast.binding);
      compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
      return variable.capability === Capability.Inout || variable.capability === Capability.Set
    } else if (ast instanceof FieldAst) {
      return this.ensureMutable(ast.left)
    } else if (ast instanceof ValueFieldAst) {
      return this.ensureMutable(ast.left)
    }
    compilerAssert(false, 'Not implemented mutable check', { ast })
  }

  generateAssignmentField(ast: SetFieldAst) {
    compilerAssert(this.ensureMutable(ast.left), 'Cannot assign to a member of an immutable struct');
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    const fieldType = ast.field.fieldType
    const reg = this.newRegister();

    const newValue = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const rightReg = this.storeResult(fieldType, newValue)
    this.addInstruction(new GetFieldPointerInstruction(reg, objReg.address, ast.field));
    this.generateMovePointerInstruction(reg, rightReg, fieldType);
  }

  generateAssignmentValueField(ast: SetValueFieldAst) {
    // return this.generateAssignmentField(new SetFieldAst(ast.type, SourceLocation.anon, ast.left, ast.fieldPath, ast.value))
    this.addInstruction(new CommentInstruction(`Set value field ${ast.fieldPath.map(x => x.name).join(", ")}`))
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    let destReg: string = objReg.address
    ast.fieldPath.forEach(field => {
      const source = destReg
      destReg = this.newRegister();
      this.addInstruction(new GetFieldPointerInstruction(destReg, source, field));
    })
    const newValue = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const rightReg = this.storeResult(ast.value.type, newValue)
    this.generateMovePointerInstruction(destReg, rightReg, ast.value.type);
  }

  generateAssignmentSubscript(ast: SetSubscriptAst) {
    compilerAssert(this.ensureMutable(ast.left), 'Cannot assign to a member of an immutable struct');
    const objReg = this.toValue(ast.left.type,
      this.generateExpression(ast.left, { valueCategory: 'lvalue' }))
    
    const elementType = ast.value.type
    const reg = this.newRegister();

    const offset = this.toValue(ast.right.type, this.generateExpression(ast.right, { valueCategory: 'rvalue' }))
    const newValue = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const valueReg = this.storeResult(elementType, newValue)
    this.addInstruction(new PointerOffsetInstruction(reg, objReg.register, elementType, offset.register));
    this.generateMovePointerInstruction(reg, valueReg, elementType);
  }

  generateMovePointerInstruction(targetPointer: string, sourcePointer: Pointer, type: Type) {
    if (type instanceof PrimitiveType) {
      const targetAccessReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      const valueReg = this.newRegister();
      this.addInstruction(new AccessInstruction(sourceAccessReg, sourcePointer.address, [Capability.Sink], type));
      this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg));
      this.addInstruction(new MarkInitializedInstruction(sourcePointer.address, type, false));
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type));
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg));
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]));
    } else {
      this.addInstruction(new MoveInstruction(targetPointer, sourcePointer.address, type));
    }
  }

  generateMoveInstruction(destReg: string, value: Value, type: Type) {
    compilerAssert(type instanceof PrimitiveType, 'Not implemented for non-primitive types');
    const destAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(destAccessReg, destReg, [Capability.Set], type));
    this.addInstruction(new StoreToAddressInstruction(destAccessReg, type, value.register));
    this.addInstruction(new EndAccessInstruction(destAccessReg, [Capability.Set]));
  }

  generateBinaryExpression(ast: OperatorAst, context: ExpressionContext): IRValue {
    const leftReg = this.toValue(ast.args[0].type, this.generateExpression(ast.args[0], context))
    const rightReg = this.toValue(ast.args[1].type, this.generateExpression(ast.args[1], context))

    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, ast.operator, leftReg.register, rightReg.register, ast.args[0].type));
    if (context.valueCategory === 'lvalue') {
      return this.storeResult(ast.type, new Value(resultReg))
    }
    return new Value(resultReg)
  }

  generateNotExpression(ast: NotAst, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast.expr, context);
    compilerAssert(value instanceof Value, 'Not expression must be an RValue');
    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, '!', value.register, '', ast.expr.type));
    return new Value(resultReg);
  }

  generateBinding(ast: BindingAst, context: ExpressionContext): IRValue {
    const addressReg = this.variableMap.get(ast.binding);
    compilerAssert(addressReg, `Undefined variable: ${ast.binding.name}`);
    return new Pointer(addressReg.register);
  }

  generateNumberLiteral(ast: NumberAst, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'Literal must be an RValue');
    const value = ast.value;
    const destReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(destReg, ast.type, value));
    return new Value(destReg);
  }
  
  generateBoolLiteral(ast: BoolAst, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'Literal must be an RValue');
    const value = ast.value;
    const destReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(destReg, ast.type, value ? 1 : 0));
    return new Value(destReg);
  }

  generateStringLiteral(ast: StringAst, context: ExpressionContext): IRValue {
    const args = [
      new NumberAst(IntType, SourceLocation.anon, ast.value.length),
      new NumberAst(IntType, SourceLocation.anon, 0),
    ]
    return this.generateCreateStructExpression(new ConstructorAst(ast.type, SourceLocation.anon, args), context)
    // compilerAssert(context.valueCategory === 'rvalue', 'Literal must be an RValue');
    // const value = ast.value;
    // const destReg = this.newRegister();
    // this.addInstruction(new LoadConstantInstruction(destReg, ast.type, value));
    // return new Value(destReg);
  }

  generateMemberExpression(ast: FieldAst, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    const destReg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Get field ${ast.field.name}`))
    this.addInstruction(new GetFieldPointerInstruction(destReg, objReg.address, ast.field));
    return new Pointer(destReg);
  }

  generateValueFieldExpression(ast: ValueFieldAst, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    this.addInstruction(new CommentInstruction(`Get field ${ast.fieldPath.map(x => x.name).join(", ")}`))
    let destReg: string = objReg.address
    ast.fieldPath.forEach(field => {
      const source = destReg
      destReg = this.newRegister();
      this.addInstruction(new GetFieldPointerInstruction(destReg, source, field));
    })
    return new Pointer(destReg);
  }

  generateSubscriptExpression(ast: SubscriptAst, context: ExpressionContext): IRValue {
    const objReg = this.toValue(ast.left.type,
      this.generateExpression(ast.left, { valueCategory: 'lvalue' }))
    const destReg = this.newRegister();
    const offset = this.toValue(ast.right.type,
      this.generateExpression(ast.right, { valueCategory: 'rvalue' }))
    this.addInstruction(new PointerOffsetInstruction(destReg, objReg.register, ast.type, offset.register));
    return new Pointer(destReg);
  }

  ///////////////////////////


  replaceMoveInstruction(block: BasicBlock, instrId: InstructionId, instr: MoveInstruction, capability: Capability) {
    const sourceAccessReg = this.newRegister();
    const targetAccessReg = this.newRegister();
    compilerAssert(capability === Capability.Set || capability === Capability.Inout, 'Invalid capability');
    const metaobject = instr.type.typeInfo.metaobject;
    const moveFnBinding = capability === Capability.Set ? metaobject.moveInitBinding : metaobject.moveAssignBinding;
    compilerAssert(moveFnBinding && moveFnBinding instanceof Binding, `Move function not found for ${instr.type.shortName}`);
    const moveFn = this.codegen.functions.get(moveFnBinding);
    compilerAssert(moveFn, `Function not found: ${moveFnBinding.name}`);
    const instrs = [
      new CommentInstruction(`Replaced move with ${capability} to ${instr.target} from ${instr.source}`),
      new AccessInstruction(sourceAccessReg, instr.source, [Capability.Sink], instr.type),
      new AccessInstruction(targetAccessReg, instr.target, [capability], instr.type),
      new CallInstruction(null, VoidType, moveFn.binding, [targetAccessReg, sourceAccessReg], moveFn.parameters.map(p => p.type), moveFn.parameters.map(p => p.capability)),
      new MarkInitializedInstruction(targetAccessReg, instr.type, true),
      new MarkInitializedInstruction(sourceAccessReg, instr.type, false),
      new EndAccessInstruction(sourceAccessReg, [Capability.Sink]),
      new EndAccessInstruction(targetAccessReg, [capability]),
    ];
    block.instructions.splice(instrId.instrId, 1, ...instrs);
  }

  removeInstruction(block: BasicBlock, instrId: InstructionId) {
    block.instructions.splice(instrId.instrId, 1);
  }

  replaceInstruction(block: BasicBlock, instrId: InstructionId, instr: IRInstruction) {
    block.instructions.splice(instrId.instrId, 1, instr);
  }

  spliceInstructions(block: BasicBlock, instrId: InstructionId, deleteCount: number, instrs: IRInstruction[]) {
    block.instructions.splice(instrId.instrId, deleteCount, ...instrs);
  }
  
  createDeallocStackInstructions(source: string, type: Type) {
    const destructor = type.typeInfo.metaobject.destructorBinding;
    if (!destructor) {
      return [
        new CommentInstruction(`TODO: No destructor for dealloc stack ${source} of type ${type.shortName}`),
        new MarkInitializedInstruction(source, type, false),
      ]
    }
    compilerAssert(destructor && destructor instanceof Binding, `Destructor not found for ${type.shortName}`);

    return [
      // new CommentInstruction(`TODO: Replace dealloc stack ${instr.target} of type ${instr.type.shortName}`),
      new CallInstruction(null, VoidType, destructor, [source], [type], [Capability.Sink])
      // new AccessInstruction(accessReg, instrId.target, [Capability.Set]),
      // new DeallocStackInstruction(accessReg, type),
    ]
  }

  createParamDeallocStackInstructions(block: BasicBlock, instrId: InstructionId, argIndex: number, type: Type) {
    const target = this.currentFunction.parameterRegisters[argIndex];

    if (this.compiledFunction.isDestructor) {
      const fields = type.typeInfo.fields;
      const instrs = fields.flatMap((field, i) => {
        const fieldReg = this.newRegister();
        const getFieldPtr = new GetFieldPointerInstruction(fieldReg, target, field)
        const dealloc = this.createDeallocStackInstructions(fieldReg, field.fieldType)
        return [getFieldPtr, ...dealloc]
      })
      return instrs
    }

    const destructor = type.typeInfo.metaobject.destructorBinding;
    if (!destructor) {
      return [
        new CommentInstruction(`TODO: No destructor for dealloc stack param ${argIndex} of type ${type.shortName}`),
        new MarkInitializedInstruction(target, type, false),
      ]
    }

    compilerAssert(destructor && destructor instanceof Binding, `Destructor not found for ${type.shortName}`);
    
    return [
      // new CommentInstruction(`TODO: Insert dealloc stack param ${argIndex} of type ${type.shortName}`),
      new CallInstruction(null, VoidType, destructor, [target], [type], [Capability.Sink])
      // new AccessInstruction(accessReg, instr.value, [Capability.Set]),
      // new DeallocStackInstruction(accessReg, instr.value),
    ]
  }

}
