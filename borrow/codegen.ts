import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, FunctionParameter, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, Pointer, Value, ReturnInstruction, ReturnNode, StoreToAddressInstruction, StructType, Variable, VariableDeclarationNode, WhileStatementNode, compilerAssert, GetFieldPointerInstruction, Capability, AndNode, OrNode, PhiInstruction, Type, VoidType, CommentInstruction, MoveInstruction, IntType, EndAccessInstruction, printIR, MarkInitializedInstruction, InstructionId } from "./defs";

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
  variableMap: Map<string, Variable> = new Map(); // variable name -> register name
  constants: { [name: string]: any } = {};

  functionInstructions: IRInstruction[] = [];

  constructor() {
    // Initialize with an entry block
    const entryLabel = 'entry';
    this.currentBlock = new BasicBlock(entryLabel, []);
    this.currentFunction = new FunctionBlock('main', [], [this.currentBlock]);
    this.blocks = this.currentFunction.blocks
    this.functionBlocks.push(this.currentFunction);
  }

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
    // compilerAssert(false, 'Expected an RValue');
    const reg = this.newRegister();
    const accessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(accessReg, value.address, [Capability.Let, Capability.Sink]));
    this.addInstruction(new LoadFromAddressInstruction(reg, accessReg));
    return new Value(reg);
  }

  generate(node: ASTNode): void {
    if (node instanceof LetConstNode) {
      this.constants[node.name] = node.value;
    } else if (node instanceof ProgramNode) {
      this.generateProgram(node);
    } else if (node instanceof VariableDeclarationNode) {
      this.generateVariableDeclaration(node);
    } else if (node instanceof ExpressionStatementNode) {
      this.generateExpression(node.expression, { valueCategory: 'rvalue' });
    } else if (node instanceof IfStatementNode) {
      this.generateIfStatement(node);
    } else if (node instanceof WhileStatementNode) {
      this.generateWhileStatement(node);
    } else if (node instanceof BlockStatementNode) {
      this.generateBlockStatement(node);
    } else if (node instanceof FunctionDeclarationNode) {
      this.generateFunctionDeclaration(node);
    } else if (node instanceof ReturnNode) {
      this.generateReturnStatement(node);
    } else {
      throw new Error(`Unsupported AST node type: ${node.nodeType}`);
    }
  }

  generateProgram(node: ProgramNode) {
    for (const stmt of node.body) {
      this.generate(stmt);
    }
    this.blocks[0].instructions.unshift(...this.functionInstructions);
    this.functionInstructions = []
  }

  generateReturnStatement(node: ReturnNode) {
    if (!node.argument) {
      this.addInstruction(new ReturnInstruction(null));
    } else {
      const returnReg = this.generateExpression(node.argument, { valueCategory: 'rvalue' });
      compilerAssert(returnReg instanceof Value, 'Return argument must be an RValue');
      this.addInstruction(new ReturnInstruction(returnReg.register));
    }
    // After block
    const afterLabel = this.newLabel()
    const afterBlock = new BasicBlock(afterLabel, []);
    this.blocks.push(afterBlock);
    this.currentBlock = afterBlock;
  }

  generateFunctionDeclaration(node: FunctionDeclarationNode) {

    const functionLabel = this.newLabel();
    const endFunctionLabel = this.newLabel();

    // Add a function definition instruction
    // this.addInstruction(new FunctionDefInstruction(node.name, node.params, functionLabel));

    // Save the current block
    const savedBlock = this.currentBlock;
    const savedFunction = this.currentFunction;


    const functionBlock = new FunctionBlock(node.name, [], []);
    this.functionBlocks.push(functionBlock);
    this.currentFunction = functionBlock;
    this.blocks = functionBlock.blocks

    const initialBlock = this.newBlock(functionLabel);

    const savedFunctionInstructions = this.functionInstructions;
    this.functionInstructions = []

    // Enter a new scope for function parameters and local variables
    // this.enterScope();

    // Map parameters to registers
    let i = 0
    for (const param of node.params) {
      const paramReg = this.newRegister();
      const type = this.constants[param.type];
      compilerAssert(type, `Type not found: ${param.type}`);
      const capability = param.capability
      this.variableMap.set(param.name, new Variable(param.name, type, paramReg, capability));
      // this.declareVariable(param, paramReg);
      // Assume that the arguments are passed in registers named 'arg0', 'arg1', etc.
      const argIndex = i++
      const argReg = `arg${argIndex}`;
      functionBlock.params.push(new FunctionParameter(argReg, type, param.capability));
      this.addInstruction(new AssignInstruction(paramReg, argReg));
    }

    // Generate the function body
    this.generate(node.body);

    // If no explicit return, add a return instruction with 'null'
    this.addInstruction(new ReturnInstruction(null));

    // Exit the function scope
    // this.exitScope();

    initialBlock.instructions.unshift(...this.functionInstructions);
    this.functionInstructions = savedFunctionInstructions;

    // Restore the previous block
    this.currentBlock = savedBlock;
    this.currentFunction = savedFunction;
    this.blocks = savedFunction.blocks
  }

  generateVariableDeclaration(node: VariableDeclarationNode) {
    this.addInstruction(new CommentInstruction(`${node.mutable ? 'var' : 'let'} ${node.name}`))
    const value = node.initializer ? this.generateExpression(node.initializer, { valueCategory: 'rvalue' }) : null

    if (node.mutable) {
      this.generateMutableVariableDeclaration(node, value);
    } else {
      this.generateProjection(node, value);
    }
  }

  generateProjection(node: VariableDeclarationNode, value: IRValue | null) {
    const type = this.constants[node.type];
    const reg = this.newRegister();
    const capability = Capability.Let
    this.variableMap.set(node.name, new Variable(node.name, type, reg, capability));
    compilerAssert(value, 'Let binding must have an initializer');
    this.addInstruction(new CommentInstruction(`Initialize ${node.name}`))
    const ptr = this.storeResult(type, value)
    // compilerAssert(value instanceof Pointer, 'Let binding must have an lvalue initializer');
    this.addInstruction(new AccessInstruction(reg, ptr.address, [Capability.Let]));
  }

  generateMutableVariableDeclaration(node: VariableDeclarationNode, value: IRValue | null) {
    const reg = this.newRegister();
    const type = this.constants[node.type];
    compilerAssert(type, `Type not found: ${node.type}`);
    const capability = node.mutable ? Capability.Inout : Capability.Let
    this.variableMap.set(node.name, new Variable(node.name, type, reg, capability));
    
    this.addFunctionInstruction(new AllocInstruction(reg, type));
    if (value) {
      
      if (value instanceof Value) {
        this.generateMoveInstruction(reg, value, type)
      } else {
        this.generateMovePointerInstruction(reg, value, type)
      }
      this.addInstruction(new CommentInstruction(`end Initialize ${node.name}`))
    }
  }

  generateIfStatement(node: IfStatementNode) {
    const conditionReg = this.generateExpression(node.condition, { valueCategory: 'rvalue' });
    compilerAssert(conditionReg instanceof Value, 'If condition must be an RValue');
    const thenLabel = this.newLabel();
    const elseLabel = this.newLabel();
    const afterLabel = this.newLabel();

    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, thenLabel, elseLabel));

    this.newBlock(thenLabel);
    this.generate(node.consequent);
    this.addInstruction(new JumpInstruction(afterLabel));

    this.newBlock(elseLabel);
    if (node.alternate) {
      this.generate(node.alternate);
    }
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
  }

  generateWhileStatement(node: WhileStatementNode) {
    const conditionLabel = this.newLabel();
    const bodyLabel = this.newLabel();
    const afterLabel = this.newLabel();

    // Jump to condition check
    this.addInstruction(new JumpInstruction(conditionLabel));

    const conditionBlock = new BasicBlock(conditionLabel, []);
    this.blocks.push(conditionBlock);
    this.currentBlock = conditionBlock;
    const conditionReg = this.generateExpression(node.condition, { valueCategory: 'rvalue' });
    compilerAssert(conditionReg instanceof Value, 'While condition must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, bodyLabel, afterLabel));

    this.newBlock(bodyLabel);
    this.generate(node.body);
    this.addInstruction(new JumpInstruction(conditionLabel));
    this.newBlock(afterLabel);
  }

  generateAndExpression(node: AndNode, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'and-expression must be an RValue');
    const rhsLabel = this.newLabel();
    const afterLabel = this.newLabel();
    const outReg = this.newRegister();
    const lhsReg = this.generateExpression(node.left, { valueCategory: 'rvalue' });
    compilerAssert(lhsReg instanceof Value, 'Left-hand side of && must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, rhsLabel, afterLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(node.right, { valueCategory: 'rvalue' });
    compilerAssert(rhsReg instanceof Value, 'Right-hand side of && must be an RValue');
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
    this.addInstruction(new PhiInstruction(outReg, [lhsReg.register, rhsReg.register]))
    return new Value(outReg);
  }

  generateOrExpression(node: OrNode, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'or-expression must be an RValue');
    const rhsLabel = this.newLabel();
    const afterLabel = this.newLabel();
    const outReg = this.newRegister();
    const lhsReg = this.generateExpression(node.left, { valueCategory: 'rvalue' });
    compilerAssert(lhsReg instanceof Value, 'Left-hand side of || must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, afterLabel, rhsLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(node.right, { valueCategory: 'rvalue' });
    compilerAssert(rhsReg instanceof Value, 'Right-hand side of || must be an RValue');
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
    this.addInstruction(new PhiInstruction(outReg, [lhsReg.register, rhsReg.register]))
    return new Value(outReg);
  }

  generateBlockStatement(node: BlockStatementNode) {
    for (const stmt of node.body) {
      this.generate(stmt);
    }
  }

  generateExpression(node: ExpressionNode, context: ExpressionContext): IRValue {
    
    if (node instanceof AssignmentNode) {
      return this.generateAssignmentExpression(node, context);
    } else if (node instanceof BinaryExpressionNode) {
      return this.generateBinaryExpression(node, context);
    } else if (node instanceof IdentifierNode) {
      return this.generateIdentifier(node, context);
    } else if (node instanceof LiteralNode) {
      return this.generateLiteral(node, context);
    } else if (node instanceof MemberExpressionNode) {
      return this.generateMemberExpression(node, context);
    } else if (node instanceof CallExpressionNode) {
      return this.generateCallExpression(node, context);
    } else if (node instanceof CreateStructNode) {
      return this.generateCreateStructExpression(node, context);
    } else if (node instanceof AndNode) {
      return this.generateAndExpression(node, context);
    } else if (node instanceof OrNode) {
      return this.generateOrExpression(node, context);
    } else {
      throw new Error(`Unsupported expression type: ${node.nodeType}`);
    }
  }

  generateCreateStructExpression(node: CreateStructNode, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'Struct creation must be an RValue');
    const structName = node.name;
    const structType = this.constants[structName];
    compilerAssert(structType && structType instanceof StructType, `Struct type not found: ${structName}`);
    const fieldValues = node.fields.map((field) => {
      return this.generateExpression(field, context);
    });
    const structReg = this.newRegister();
    this.addFunctionInstruction(new AllocInstruction(structReg, structType));
    for (let i = 0; i < fieldValues.length; i++) {
      const field = structType.fields[i]
      const fieldValue = this.toValue(fieldValues[i])
      // compilerAssert(fieldValue instanceof Value, 'Struct field value must be an RValue');
      const fieldAccessReg = this.newRegister();
      const reg = this.newRegister();
      const fieldIndex = i;
      const type = field.type
      this.addInstruction(new CommentInstruction(`Store field ${field.name} of ${structReg}`))
      this.addInstruction(new GetFieldPointerInstruction(reg, structReg, fieldIndex));
      this.addInstruction(new AccessInstruction(fieldAccessReg, reg, [Capability.Set]));
      this.addInstruction(new StoreToAddressInstruction(fieldAccessReg, type, fieldValue.register));
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

  generateCallExpression(node: CallExpressionNode, context: ExpressionContext): IRValue {
    // Generate code for arguments
    const fn = this.functionBlocks.find((fn) => fn.name === node.callee);
    compilerAssert(fn, `Function ${node.callee} not found`);
    this.addInstruction(new CommentInstruction(`Call ${node.callee}`))
    
    const argRegs: string[] = [];
    let i = 0
    for (const arg of node.args) {
      const argIndex = i++;
      const newReg = this.newRegister();
      if (fn.params[argIndex].capability === Capability.Inout) {
        const argReg = this.generateExpression(arg, { valueCategory: 'lvalue' });
        compilerAssert(argReg instanceof Pointer, 'Function argument must be an pointer');
        this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Inout]));
        argRegs.push(newReg);
      } else {
        const argReg = this.generateExpression(arg, { valueCategory: 'rvalue' });
        const reg = argReg instanceof Value ? argReg.register : argReg.address;
        // const value = this.toValue(argReg)
        this.addInstruction(new AccessInstruction(newReg, reg, [Capability.Let]));
        argRegs.push(newReg);
      }
    }

    const functionName = node.callee;

    // Call the function
    const resultReg = this.newRegister();
    const accessReg = this.newRegister();
    this.addFunctionInstruction(new AllocInstruction(resultReg, VoidType));
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set]));
    this.addInstruction(new CallInstruction(accessReg, functionName, argRegs));
    return new Pointer(resultReg)
  }

  generateAssignmentExpression(node: AssignmentNode, context: ExpressionContext): IRValue {
    const left = node.left;
    

    if (left instanceof IdentifierNode) {
      // Simple assignment to variable
      const varName = left.name;
      let variable = this.variableMap.get(varName);
      compilerAssert(variable, `Undefined variable: ${varName}`);
      const type = variable.type

      const newLocal = this.generateExpression(node.right, { valueCategory: 'rvalue' });
      // compilerAssert(newLocal instanceof Pointer, 'Assignment right-hand side must be an lvalue', { right: node.right });
      const lvalue = this.storeResult(type, newLocal)
      // compilerAssert(lvalue instanceof Pointer, 'Assignment right-hand side must be an lvalue');

      // const val = this.toValue(rightVal)
      this.generateMovePointerInstruction(variable.register, lvalue, type)
      compilerAssert(variable.capability === Capability.Inout, 'Cannot assign to a let variable');
      return new Pointer(variable.register);
    } else if (left instanceof MemberExpressionNode) {
      const checkMutable = (node: MemberExpressionNode) => {
        if (node.object instanceof IdentifierNode) {
          const variable = this.variableMap.get(node.object.name);
          compilerAssert(variable, `Undefined variable: ${node.object.name}`);
          return variable.capability === Capability.Inout
        } else if (node.object instanceof MemberExpressionNode) {
          return checkMutable(node.object)
        }
      }
      compilerAssert(checkMutable(left), 'Cannot assign to a member of an immutable struct');
      // Assignment to object field
      const objReg = this.generateExpression(left.object, { valueCategory: 'lvalue' });
      compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
      
      const fieldName = left.property;
      const type = this.constants[left.type];
      compilerAssert(type && type instanceof StructType, `Struct type not found: ${left.type}`);
      const fieldIndex = type.fields.findIndex((field) => field.name === fieldName);
      const reg = this.newRegister();
      const fieldType = type.fields[fieldIndex].type

      const newValue = this.generateExpression(node.right, { valueCategory: 'rvalue' });
      const rightReg = this.storeResult(fieldType, newValue)
      this.addInstruction(new GetFieldPointerInstruction(reg, objReg.address, fieldIndex));
      this.generateMovePointerInstruction(reg, rightReg, type);
      return rightReg;
    } else {
      throw new Error(`Unsupported left-hand side in assignment`);
    }
  }

  generateMovePointerInstruction(targetPtr: string, pointer: Pointer, type: Type) {
    if (type === IntType) {
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
    if (type === IntType) {
      const destAccessReg = this.newRegister();
      this.addInstruction(new AccessInstruction(destAccessReg, destReg, [Capability.Set]));
      this.addInstruction(new StoreToAddressInstruction(destAccessReg, type, value.register));
      this.addInstruction(new EndAccessInstruction(destAccessReg, [Capability.Set]));
    } else {
      compilerAssert(false, 'Not implemented for type', { value, type })
    }
  }

  generateBinaryExpression(node: BinaryExpressionNode, context: ExpressionContext): IRValue {
    const leftReg = this.toValue(this.generateExpression(node.left, context))
    const rightReg = this.toValue(this.generateExpression(node.right, context))

    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, node.operator, leftReg.register, rightReg.register));
    if (context.valueCategory === 'lvalue') {
      return this.storeResult(IntType, new Value(resultReg))
    }
    return new Value(resultReg)
  }

  generateIdentifier(node: IdentifierNode, context: ExpressionContext): IRValue {
    const varName = node.name;
    const addressReg = this.variableMap.get(varName);

    compilerAssert(addressReg, `Undefined variable: ${varName}`);

    if (context.valueCategory === 'lvalue') {
      return new Pointer(addressReg.register);
    } else {
      return new Pointer(addressReg.register);
    }
  }

  generateLiteral(node: LiteralNode, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'Literal must be an RValue');
    const value = node.value;
    const destReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(destReg, value));
    return new Value(destReg);
  }

  generateMemberExpression(node: MemberExpressionNode, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(node.object, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    const fieldName = node.property;
    const destReg = this.newRegister();
    const type = this.constants[node.type];
    compilerAssert(type && type instanceof StructType, `Struct type not found: ${node.type}`);
    const fieldIndex = type.fields.findIndex((field) => field.name === fieldName);
    this.addInstruction(new GetFieldPointerInstruction(destReg, objReg.address, fieldIndex));
    return new Pointer(destReg);
  }

  replaceMoveInstruction(block: BasicBlock, instrId: InstructionId, instr: MoveInstruction, capability: Capability) {
    const sourceAccessReg = this.newRegister();
    const targetAccessReg = this.newRegister();
    const callReg = this.newRegister();
    compilerAssert(capability === Capability.Set || capability === Capability.Inout, 'Invalid capability');
    const moveFn = capability === Capability.Set ? `moveInit${instr.type.shortName}` : `moveAssign${instr.type.shortName}`;
    const instrs = [
      new CommentInstruction(`Replaced move with ${capability} to ${instr.target} from ${instr.source}`),
      new AllocInstruction(callReg, instr.type),
      new AccessInstruction(sourceAccessReg, instr.source, [Capability.Sink]),
      new AccessInstruction(targetAccessReg, instr.target, [capability]),
      new CallInstruction(callReg, moveFn, [sourceAccessReg, targetAccessReg]),
      new MarkInitializedInstruction(targetAccessReg, true),
      new MarkInitializedInstruction(sourceAccessReg, false),
      new EndAccessInstruction(sourceAccessReg, [Capability.Sink]),
      new EndAccessInstruction(targetAccessReg, [capability]),
    ];
    block.instructions.splice(instrId.instrId, 1, ...instrs);
  }
}
