import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, FunctionParameter, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LValue, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, RValue, ReturnInstruction, ReturnNode, StoreToAddressInstruction, StructType, Variable, VariableDeclarationNode, WhileStatementNode, compilerAssert, GetFieldPointerInstruction, Capability, AndNode, OrNode, PhiInstruction, Type, VoidType, CommentInstruction, MoveInstruction } from "./defs";

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
      compilerAssert(returnReg instanceof RValue, 'Return argument must be an RValue');
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

    this.newBlock(functionLabel);

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
      this.variableMap.set(param.name, new Variable(param.name, type, paramReg, param.byReference));
      // this.declareVariable(param, paramReg);
      // Assume that the arguments are passed in registers named 'arg0', 'arg1', etc.
      const argIndex = i++
      const argReg = `arg${argIndex}`;
      functionBlock.params.push(new FunctionParameter(argReg, type, param.byReference));
      this.addInstruction(new AssignInstruction(paramReg, argReg));
    }

    // Generate the function body
    this.generate(node.body);

    // If no explicit return, add a return instruction with 'null'
    this.addInstruction(new ReturnInstruction(null));

    // Exit the function scope
    // this.exitScope();

    this.currentBlock.instructions.unshift(...this.functionInstructions);
    this.functionInstructions = savedFunctionInstructions;

    // Restore the previous block
    this.currentBlock = savedBlock;
    this.currentFunction = savedFunction;
    this.blocks = savedFunction.blocks
  }

  generateVariableDeclaration(node: VariableDeclarationNode) {
    // Map the variable name to a new register
    const reg = this.newRegister();
    const type = this.constants[node.type];
    compilerAssert(type, `Type not found: ${node.type}`);
    this.variableMap.set(node.name, new Variable(node.name, type, reg, true));
    this.addFunctionInstruction(new AllocInstruction(reg, type));
  }

  generateIfStatement(node: IfStatementNode) {
    const conditionReg = this.generateExpression(node.condition, { valueCategory: 'rvalue' });
    compilerAssert(conditionReg instanceof RValue, 'If condition must be an RValue');
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
    compilerAssert(conditionReg instanceof RValue, 'While condition must be an RValue');
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
    compilerAssert(lhsReg instanceof RValue, 'Left-hand side of && must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, rhsLabel, afterLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(node.right, { valueCategory: 'rvalue' });
    compilerAssert(rhsReg instanceof RValue, 'Right-hand side of && must be an RValue');
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
    this.addInstruction(new PhiInstruction(outReg, [lhsReg.register, rhsReg.register]))
    return new RValue(outReg);
  }

  generateOrExpression(node: OrNode, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'or-expression must be an RValue');
    const rhsLabel = this.newLabel();
    const afterLabel = this.newLabel();
    const outReg = this.newRegister();
    const lhsReg = this.generateExpression(node.left, { valueCategory: 'rvalue' });
    compilerAssert(lhsReg instanceof RValue, 'Left-hand side of || must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, afterLabel, rhsLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(node.right, { valueCategory: 'rvalue' });
    compilerAssert(rhsReg instanceof RValue, 'Right-hand side of || must be an RValue');
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
    this.addInstruction(new PhiInstruction(outReg, [lhsReg.register, rhsReg.register]))
    return new RValue(outReg);
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
      const fieldValue = fieldValues[i];
      compilerAssert(fieldValue instanceof RValue, 'Struct field value must be an RValue');
      const fieldAccessReg = this.newRegister();
      const reg = this.newRegister();
      const fieldIndex = i;
      const type = field.type
      this.addInstruction(new CommentInstruction(`Store field ${field.name} to ${structReg}`))
      this.addInstruction(new GetFieldPointerInstruction(reg, structReg, fieldIndex));
      this.addInstruction(new AccessInstruction(fieldAccessReg, reg, [Capability.Set]));
      this.addInstruction(new StoreToAddressInstruction(fieldAccessReg, type, fieldValue.register));
      // this.addInstruction(new StoreFieldInstruction(structReg, field.name, fieldValue.register));
    }
    return new RValue(structReg)
  }

  storeResult(resultReg: string, type: Type, context: ExpressionContext) {
    compilerAssert(context.valueCategory === 'rvalue', 'Result must be an RValue');
    const reg = this.newRegister();
    this.addFunctionInstruction(new AllocInstruction(reg, type));

    return new RValue(resultReg)
  }

  generateCallExpression(node: CallExpressionNode, context: ExpressionContext): IRValue {
    // Generate code for arguments
    const fn = this.functionBlocks.find((fn) => fn.name === node.callee);
    compilerAssert(fn, `Function ${node.callee} not found`);
    
    const argRegs: string[] = [];
    let i = 0
    for (const arg of node.args) {
      const argIndex = i++;
      const valueCategory = fn.params[argIndex].byReference ? 'lvalue' : 'rvalue';
      const argContext: ExpressionContext = { valueCategory };
      const argReg = this.generateExpression(arg, argContext);
      const newReg = this.newRegister();
      if (fn.params[argIndex].byReference) {
        compilerAssert(argReg instanceof LValue, 'Function argument must be an LValue');
        this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Inout]));
        argRegs.push(newReg);
      } else {
        compilerAssert(argReg instanceof RValue, 'Function argument must be an RValue');
        this.addInstruction(new AccessInstruction(newReg, argReg.register, [Capability.Let]));
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
    // return this.storeResult(resultReg, VoidType, context)
    return new RValue(resultReg)
  }

  generateAssignmentExpression(node: AssignmentNode, context: ExpressionContext): IRValue {
    const left = node.left;
    

    if (left instanceof IdentifierNode) {
      // Simple assignment to variable
      const varName = left.name;
      let destReg = this.variableMap.get(varName);
      compilerAssert(destReg, `Undefined variable: ${varName}`);
      // if (!destReg) {
        // Variable not declared
        // destReg = new Variable(varName, this.newRegister(), false);
        // this.variableMap.set(varName, destReg);
      // }
      // compilerAssert(destReg., 'Variable must be an RValue');
      // Assign rightReg to destReg
      const type = destReg.type

      const rightReg = this.generateExpression(node.right, context);
      compilerAssert(rightReg instanceof RValue, 'Assignment right-hand side must be an RValue');

      const newReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      this.addInstruction(new AccessInstruction(newReg, destReg.register, [Capability.Set]));
      this.addInstruction(new AccessInstruction(sourceAccessReg, rightReg.register, [Capability.Let, Capability.Sink]));
      // this.addInstruction(new CommentInstruction(`Move ${sourceAccessReg} to ${newReg}`))
      this.addInstruction(new MoveInstruction(newReg, sourceAccessReg));
      // if (type instanceof StructType) {
      //   this.addInstruction(new CallInstruction(newReg, 'move', [newReg, sourceAccessReg]));
      // } else {
      //   this.addInstruction(new StoreToAddressInstruction(newReg, type, sourceAccessReg));
      // }
      return rightReg
    } else if (left instanceof MemberExpressionNode) {
      // Assignment to object field
      const objReg = this.generateExpression(left.object, context);
      compilerAssert(objReg instanceof RValue, 'Object must be an RValue');
      
      const fieldName = left.property;
      // Store rightReg into objReg.fieldName
      const type = this.constants[left.type];
      compilerAssert(type && type instanceof StructType, `Struct type not found: ${left.type}`);
      const fieldIndex = type.fields.findIndex((field) => field.name === fieldName);
      const reg = this.newRegister();
      const fieldType = type.fields[fieldIndex].type

      const rightReg = this.generateExpression(node.right, context);
      compilerAssert(rightReg instanceof RValue, 'Assignment right-hand side must be an RValue');

      this.addInstruction(new GetFieldPointerInstruction(reg, objReg.register, fieldIndex));
      this.addInstruction(new StoreToAddressInstruction(reg, fieldType, rightReg.register));
      // this.addInstruction(new StoreFieldInstruction(objReg.register, fieldName, rightReg.register));
      return rightReg;
    } else {
      throw new Error(`Unsupported left-hand side in assignment`);
    }
  }

  generateBinaryExpression(node: BinaryExpressionNode, context: ExpressionContext): IRValue {
    const leftReg = this.generateExpression(node.left, context)
    const rightReg = this.generateExpression(node.right, context)
    compilerAssert(leftReg instanceof RValue && rightReg instanceof RValue, 'BinaryExpression operands must be RValues');
    const resultReg = this.newRegister();
    // Create a BinaryOperationInstruction
    this.addInstruction(new BinaryOperationInstruction(resultReg, node.operator, leftReg.register, rightReg.register));
    this.storeResult(resultReg, VoidType, context)
    return new RValue(resultReg)
  }

  generateIdentifier(node: IdentifierNode, context: ExpressionContext): IRValue {
    const varName = node.name;
    const addressReg = this.variableMap.get(varName);

    compilerAssert(addressReg, `Undefined variable: ${varName}`);

    if (context.valueCategory === 'lvalue') {
      if (addressReg.isReference) {
        return new LValue(addressReg.register);
      }
      return new LValue(addressReg.register);
      // compilerAssert(false, 'Cannot take the address of a non-reference variable');
      // return addressReg;
    } else {
      if (!addressReg.isReference) {
        return new RValue(addressReg.register);
      } else {
        const accessReg = this.newRegister();
        const valueReg = this.newRegister();
        // this.addInstruction(new LoadFromAddressInstruction(valueReg, addressReg.register));
        // return new RValue(valueReg);
        this.addInstruction(new AccessInstruction(accessReg, addressReg.register, [Capability.Let]));
        this.addInstruction(new LoadFromAddressInstruction(valueReg, accessReg));
        return new RValue(valueReg);
      }
    }
  }

  generateLiteral(node: LiteralNode, context: ExpressionContext): IRValue {
    const value = node.value;
    const destReg = this.newRegister();
    const pointerReg = this.newRegister();
    this.addFunctionInstruction(new AllocInstruction(pointerReg, VoidType));
    this.addInstruction(new LoadConstantInstruction(destReg, value));
    this.addInstruction(new StoreToAddressInstruction(pointerReg, VoidType, destReg));
    return new RValue(pointerReg);
  }

  generateMemberExpression(node: MemberExpressionNode, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(node.object, context) as RValue;
    const fieldName = node.property;
    const destReg = this.newRegister();
    // Load objReg.fieldName into destReg
    const type = this.constants[node.type];
    compilerAssert(type && type instanceof StructType, `Struct type not found: ${node.type}`);
    const fieldIndex = type.fields.findIndex((field) => field.name === fieldName);
    this.addInstruction(new GetFieldPointerInstruction(destReg, objReg.register, fieldIndex));
    if (context.valueCategory === 'lvalue') {
      return new LValue(destReg);
    } else {
      const valueReg = this.newRegister();
      this.addInstruction(new LoadFromAddressInstruction(valueReg, destReg));
      return new RValue(valueReg);
    }
    // this.addInstruction(new LoadFieldInstruction(destReg, objReg.register, fieldName));
    // return new RValue(destReg);
  }
}