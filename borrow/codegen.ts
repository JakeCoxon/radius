import { AndAst, Ast, Binding, BindingAst, CallAst, Capability, CompiledFunction, ConstructorAst, FieldAst, FunctionParameter, IfAst, IntType, LetAst, NumberAst, OperatorAst, OrAst, PrimitiveType, ReturnAst, SetAst, SetFieldAst, StatementsAst, Type, VoidAst, VoidType, WhileAst } from "../src/defs";
import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, Pointer, Value, ReturnInstruction, ReturnNode, StoreToAddressInstruction, Variable, VariableDeclarationNode, WhileStatementNode, compilerAssert, GetFieldPointerInstruction, AndNode, OrNode, PhiInstruction, CommentInstruction, MoveInstruction, EndAccessInstruction, printIR, MarkInitializedInstruction, InstructionId, PhiSource } from "./defs";

type ExpressionContext = {
  valueCategory: 'rvalue' | 'lvalue';
}

export class CodeGenerator {
  blocks: BasicBlock[] = [];
  functionBlocks: FunctionBlock[] = [];
  currentBlock: BasicBlock;
  currentFunction: FunctionBlock
  labelCount: number = 0;
  registerCount: number = 0;
  variableMap: Map<Binding, Variable> = new Map(); // variable bidning -> register name
  constants: { [name: string]: any } = {};

  functionInstructions: IRInstruction[] = [];

  functions: Map<Binding, CompiledFunction> = new Map();
  unusedBlocks: Set<string> = new Set();

  constructor() { }

  newLabel(): string {
    return `L${this.labelCount++}`;
  }

  newRegister(): string {
    return `r${this.registerCount++}`;
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

  addFunctionInstruction(instr: IRInstruction) {
    this.functionInstructions.push(instr);
  }

  toValue(value: IRValue): Value {
    if (value instanceof Value) { return value; }
    const reg = this.newRegister();
    const accessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(accessReg, value.address, [Capability.Let, Capability.Sink]));
    this.addInstruction(new LoadFromAddressInstruction(reg, accessReg));
    return new Value(reg);
  }

  generateFunction(binding: Binding, params: FunctionParameter[], returnType: Type, body: Ast) {
    const blocks: BasicBlock[] = [];
    const entryLabel = this.newLabel();
    const entryBlock = new BasicBlock(entryLabel, []);
    blocks.push(entryBlock);
    this.currentBlock = entryBlock;
    this.blocks = blocks;
    this.variableMap = new Map();

    this.functionInstructions = [];

    const paramRegs = params.map((param) => {
      const paramReg = this.newRegister()
      this.variableMap.set(param.binding, new Variable(param.binding.name, param.type, paramReg, param.capability));

      return paramReg
    });

    this.currentFunction = new FunctionBlock(binding.name, binding, params, paramRegs, blocks!);

    this.generate(body);
    this.blocks[0].instructions.unshift(...this.functionInstructions);
    this.functionInstructions = []
    this.functionBlocks.push(this.currentFunction);

    // Remove unused blocks
    this.currentFunction.blocks = blocks.filter(block => !this.unusedBlocks.has(block.label));

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

    this.generateExpression(ast, { valueCategory: 'rvalue' })
    
  }

  generateExpression(ast: Ast, context: ExpressionContext): IRValue {
    if (ast instanceof NumberAst)      { return this.generateNumberLiteral(ast, context) }
    if (ast instanceof CallAst)        { return this.generateCallExpression(ast, context) }
    if (ast instanceof ConstructorAst) { return this.generateCreateStructExpression(ast, context) }
    if (ast instanceof BindingAst)     { return this.generateBinding(ast, context) }
    if (ast instanceof FieldAst)       { return this.generateMemberExpression(ast, context) }
    if (ast instanceof AndAst)         { return this.generateAndExpression(ast, context) }
    if (ast instanceof OrAst)          { return this.generateOrExpression(ast, context) }
    if (ast instanceof OperatorAst)    { return this.generateBinaryExpression(ast, context) }
    
    compilerAssert(false, 'Not implemented expression', { ast })
  }

  generateTopLevel(ast: Ast) {
    this.generate(ast)
    this.blocks[0].instructions.unshift(...this.functionInstructions);
    this.functionInstructions = []
  }

  generateReturnStatement(ast: ReturnAst) {
    if (!ast.expr) {
      this.addInstruction(new ReturnInstruction(null));
    } else {
      const returnReg = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
      compilerAssert(returnReg instanceof Value, 'Return argument must be an RValue');
      this.addInstruction(new ReturnInstruction(returnReg.register));
    }
    // This is a trick to make sure that subsequent instructions
    // are generated but are not added to the final IR
    const afterLabel = this.newLabel()
    this.newBlock(afterLabel)
    this.unusedBlocks.add(afterLabel)
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
    this.addInstruction(new CommentInstruction(`Initialize ${ast.binding.name}`))
    const ptr = this.storeResult(type, value)
    // compilerAssert(value instanceof Pointer, 'Let binding must have an lvalue initializer');
    this.addInstruction(new AccessInstruction(reg, ptr.address, [Capability.Let]));
  }

  generateMutableVariableDeclaration(ast: LetAst, value: IRValue | null) {
    const reg = this.newRegister();
    const type = ast.binding.type
    const mutable = ast.mutable
    const capability = mutable ? Capability.Inout : Capability.Let
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, capability));
    
    this.addFunctionInstruction(new AllocInstruction(reg, type));
    if (value) {
      if (value instanceof Value) {
        this.generateMoveInstruction(reg, value, type)
      } else {
        this.generateMovePointerInstruction(reg, value, type)
      }
      this.addInstruction(new CommentInstruction(`end Initialize ${ast.binding.name}`))
    }
  }

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
    compilerAssert(context.valueCategory === 'rvalue', 'Struct creation must be an RValue');
    const structType = ast.type
    compilerAssert(structType, `Struct type not found`);
    compilerAssert(ast.args.length === structType.typeInfo.fields.length, 'Field count mismatch');
    const fieldValues = ast.args.map((field) => {
      return this.generateExpression(field, context);
    });
    const structReg = this.newRegister();
    this.addFunctionInstruction(new AllocInstruction(structReg, structType));
    for (let i = 0; i < fieldValues.length; i++) {
      const field = structType.typeInfo.fields[i]
      const fieldValue = this.toValue(fieldValues[i])
      // compilerAssert(fieldValue instanceof Value, 'Struct field value must be an RValue');
      const fieldAccessReg = this.newRegister();
      const reg = this.newRegister();
      const fieldIndex = i;
      const fieldType = field.fieldType
      this.addInstruction(new CommentInstruction(`Store field ${field.name} of ${structReg}`))
      this.addInstruction(new GetFieldPointerInstruction(reg, fieldType, structReg, fieldIndex));
      this.addInstruction(new AccessInstruction(fieldAccessReg, reg, [Capability.Set]));
      this.addInstruction(new StoreToAddressInstruction(fieldAccessReg, fieldType, fieldValue.register));
      // this.addInstruction(new StoreFieldInstruction(structReg, field.name, fieldValue.register));
    }
    return new Pointer(structReg)
  }

  storeResult(type: Type, value: IRValue) {
    if (value instanceof Pointer) return value
    const reg = this.newRegister();
    this.addFunctionInstruction(new AllocInstruction(reg, type));
    this.generateMoveInstruction(reg, value, type)

    return new Pointer(reg)
  }

  generateCallExpression(ast: CallAst, context: ExpressionContext): IRValue {
    // Generate code for arguments
    const fn = this.functions.get(ast.binding)
    compilerAssert(fn, `Function ${ast.binding.name} not found`);
    this.addInstruction(new CommentInstruction(`Call ${ast.binding.name}`))

    compilerAssert(ast.args.length === fn.argBindings.length, 'Argument count mismatch');
    
    const argRegs: string[] = [];
    let i = 0
    for (const givenArg of ast.args) {
      const argIndex = i++;
      const newReg = this.newRegister();
      const inout = fn.parameters[argIndex].capability === Capability.Inout
      if (inout) {
        const argReg = this.generateExpression(givenArg, { valueCategory: 'lvalue' });
        compilerAssert(argReg instanceof Pointer, 'Function argument must be an pointer');
        this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Inout]));
        argRegs.push(newReg);
      } else {
        const argReg = this.generateExpression(givenArg, { valueCategory: 'rvalue' });
        if (argReg instanceof Value) {
          this.addInstruction(new AccessInstruction(newReg, argReg.register, [Capability.Let]));
        } else if (givenArg.type instanceof PrimitiveType) {
          const value = this.toValue(argReg)
          this.addInstruction(new AccessInstruction(newReg, value.register, [Capability.Let]));
        } else {
          this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Let]));
        }

        argRegs.push(newReg);
      }
    }

    // Call the function
    if (fn.returnType === VoidType) {
      this.addInstruction(new CallInstruction(null, fn.returnType, ast.binding, argRegs));
      return new Pointer('') // hack. Make sure this is not used
    }
    const accessReg = this.newRegister()
    const resultReg = this.newRegister()
    this.addFunctionInstruction(new AllocInstruction(resultReg, fn.returnType));
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set]));
    this.addInstruction(new CallInstruction(accessReg, fn.returnType, ast.binding, argRegs));
    return new Pointer(resultReg)
  }

  generateAssignmentStatement(ast: SetAst) {
    let variable = this.variableMap.get(ast.binding)
    compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
    const type = variable.type

    const newLocal = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const lvalue = this.storeResult(type, newLocal)

    this.generateMovePointerInstruction(variable.register, lvalue, type)
    compilerAssert(variable.capability === Capability.Inout, 'Cannot assign to a let variable');
  }

  generateAssignmentField(ast: SetFieldAst) {
    const checkMutable = (ast: Ast) => {
      if (ast instanceof BindingAst) {
        const variable = this.variableMap.get(ast.binding);
        compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
        return variable.capability === Capability.Inout
      } else if (ast instanceof FieldAst) {
        return checkMutable(ast.left)
      }
      compilerAssert(false, 'Not implemented mutable check', { ast })
    }
    compilerAssert(checkMutable(ast.left), 'Cannot assign to a member of an immutable struct');
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    const fieldType = ast.field.fieldType
    compilerAssert(fieldType, `Struct type not found`);
    const reg = this.newRegister();

    const newValue = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const rightReg = this.storeResult(fieldType, newValue)
    this.addInstruction(new GetFieldPointerInstruction(reg, fieldType, objReg.address, ast.field.index));
    this.generateMovePointerInstruction(reg, rightReg, fieldType);
  }

  generateMovePointerInstruction(targetPtr: string, pointer: Pointer, type: Type) {
    if (type instanceof PrimitiveType) {
      const targetAccessReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      const valueReg = this.newRegister();
      this.addInstruction(new AccessInstruction(sourceAccessReg, pointer.address, [Capability.Sink]));
      this.addInstruction(new LoadFromAddressInstruction(valueReg, sourceAccessReg));
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPtr, [Capability.Set]));
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg));
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]));
    } else {
      this.addInstruction(new MoveInstruction(targetPtr, pointer.address, type));
    }
  }

  generateMoveInstruction(destReg: string, value: Value, type: Type) {
    compilerAssert(type instanceof PrimitiveType, 'Not implemented for non-primitive types');
    const destAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(destAccessReg, destReg, [Capability.Set]));
    this.addInstruction(new StoreToAddressInstruction(destAccessReg, type, value.register));
    this.addInstruction(new EndAccessInstruction(destAccessReg, [Capability.Set]));
  }

  generateBinaryExpression(ast: OperatorAst, context: ExpressionContext): IRValue {
    const leftReg = this.toValue(this.generateExpression(ast.args[0], context))
    const rightReg = this.toValue(this.generateExpression(ast.args[1], context))

    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, ast.operator, leftReg.register, rightReg.register));
    if (context.valueCategory === 'lvalue') {
      return this.storeResult(ast.type, new Value(resultReg))
    }
    return new Value(resultReg)
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

  generateMemberExpression(ast: FieldAst, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    const destReg = this.newRegister();
    this.addInstruction(new GetFieldPointerInstruction(destReg, ast.field.fieldType, objReg.address, ast.field.index));
    return new Pointer(destReg);
  }

  replaceMoveInstruction(block: BasicBlock, instrId: InstructionId, instr: MoveInstruction, capability: Capability) {
    const sourceAccessReg = this.newRegister();
    const targetAccessReg = this.newRegister();
    const callReg = this.newRegister();
    compilerAssert(capability === Capability.Set || capability === Capability.Inout, 'Invalid capability');
    const moveFnName = capability === Capability.Set ? `moveInit${instr.type.shortName}` : `moveAssign${instr.type.shortName}`;
    const moveFn = Array.from(this.functions.values()).find(fn => fn.binding.name === moveFnName); // hacks
    compilerAssert(moveFn, `Function not found: ${moveFnName}`);
    const instrs = [
      new CommentInstruction(`Replaced move with ${capability} to ${instr.target} from ${instr.source}`),
      new AllocInstruction(callReg, instr.type),
      new AccessInstruction(sourceAccessReg, instr.source, [Capability.Sink]),
      new AccessInstruction(targetAccessReg, instr.target, [capability]),
      new CallInstruction(callReg, VoidType, moveFn.binding, [sourceAccessReg, targetAccessReg]),
      new MarkInitializedInstruction(targetAccessReg, true),
      new MarkInitializedInstruction(sourceAccessReg, false),
      new EndAccessInstruction(sourceAccessReg, [Capability.Sink]),
      new EndAccessInstruction(targetAccessReg, [capability]),
    ];
    block.instructions.splice(instrId.instrId, 1, ...instrs);
  }
}
