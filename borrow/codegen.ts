import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, CheckInitializedInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, FunctionParameter, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LValue, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, LoadFieldInstruction, MemberExpressionNode, ProgramNode, RValue, ReturnInstruction, ReturnNode, StoreFieldInstruction, StoreToAddressInstruction, StructType, Variable, VariableDeclarationNode, WhileStatementNode, compilerAssert, GetFieldPointerInstruction } from "./defs";

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

  constructor() {
    // Initialize with an entry block
    const entryLabel = 'entry';
    this.currentBlock = new BasicBlock(entryLabel, []);
    this.blocks.push(this.currentBlock);
    this.currentFunction = new FunctionBlock('main', [], [this.currentBlock]);
    this.functionBlocks.push(this.currentFunction);
  }

  newLabel(): string {
    return `L${this.labelCount++}`;
  }

  newRegister(): string {
    return `r${this.registerCount++}`;
  }

  addInstruction(instr: IRInstruction) {
    this.currentBlock.instructions.push(instr);
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
  }

  generateReturnStatement(node: ReturnNode) {
    if (!node.argument) return this.addInstruction(new ReturnInstruction(null));
    const returnReg = this.generateExpression(node.argument, { valueCategory: 'rvalue' });
    compilerAssert(returnReg instanceof RValue, 'Return argument must be an RValue');
    this.addInstruction(new ReturnInstruction(returnReg.register));
  }

  generateFunctionDeclaration(node: FunctionDeclarationNode) {

    const functionLabel = this.newLabel();
    const endFunctionLabel = this.newLabel();

    // Add a function definition instruction
    // this.addInstruction(new FunctionDefInstruction(node.name, node.params, functionLabel));

    // Save the current block
    const savedBlock = this.currentBlock;
    const savedFunction = this.currentFunction;

    // Function body block
    const block = new BasicBlock(functionLabel, []);
    this.blocks.push(block);
    this.currentBlock = block;

    const functionBlock = new FunctionBlock(node.name, [], [block]);
    this.functionBlocks.push(functionBlock);
    this.currentFunction = functionBlock;

    // Enter a new scope for function parameters and local variables
    // this.enterScope();

    // Map parameters to registers
    let i = 0
    for (const param of node.params) {
      const paramReg = this.newRegister();
      const type = this.constants[param.type];
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

    // Restore the previous block
    this.currentBlock = savedBlock;
    this.currentFunction = savedFunction;
  }

  generateVariableDeclaration(node: VariableDeclarationNode) {
    // Map the variable name to a new register
    const reg = this.newRegister();
    const type = this.constants[node.type];
    this.variableMap.set(node.name, new Variable(node.name, type, reg, false));
    this.addInstruction(new AllocInstruction(reg, type));
  }

  generateIfStatement(node: IfStatementNode) {
    const conditionReg = this.generateExpression(node.condition, { valueCategory: 'rvalue' });
    const thenLabel = this.newLabel();
    const elseLabel = this.newLabel();
    const afterLabel = this.newLabel();

    // Conditional jump based on conditionReg
    this.addInstruction(new ConditionalJumpInstruction(conditionReg, thenLabel));

    // Jump to elseLabel if condition is false
    this.addInstruction(new JumpInstruction(elseLabel));

    // Then block
    const thenBlock = new BasicBlock(thenLabel, []);
    this.blocks.push(thenBlock);
    this.currentBlock = thenBlock;
    this.generate(node.consequent);
    // After then block, jump to afterLabel
    this.addInstruction(new JumpInstruction(afterLabel));

    // Else block
    const elseBlock = new BasicBlock(elseLabel, []);
    this.blocks.push(elseBlock);
    this.currentBlock = elseBlock;
    if (node.alternate) {
      this.generate(node.alternate);
    }
    // After else block, jump to afterLabel
    this.addInstruction(new JumpInstruction(afterLabel));

    // After block
    const afterBlock = new BasicBlock(afterLabel, []);
    this.blocks.push(afterBlock);
    this.currentBlock = afterBlock;
  }

  generateWhileStatement(node: WhileStatementNode) {
    const conditionLabel = this.newLabel();
    const bodyLabel = this.newLabel();
    const afterLabel = this.newLabel();

    // Jump to condition check
    this.addInstruction(new JumpInstruction(conditionLabel));

    // Condition block
    const conditionBlock = new BasicBlock(conditionLabel, []);
    this.blocks.push(conditionBlock);
    this.currentBlock = conditionBlock;
    const conditionReg = this.generateExpression(node.condition, { valueCategory: 'rvalue' });
    // Conditional jump to body if condition is true
    this.addInstruction(new ConditionalJumpInstruction(conditionReg, bodyLabel));
    // Jump to afterLabel if condition is false
    this.addInstruction(new JumpInstruction(afterLabel));

    // Body block
    const bodyBlock = new BasicBlock(bodyLabel, []);
    this.blocks.push(bodyBlock);
    this.currentBlock = bodyBlock;
    this.generate(node.body);
    // After body, jump back to condition check
    this.addInstruction(new JumpInstruction(conditionLabel));

    // After block
    const afterBlock = new BasicBlock(afterLabel, []);
    this.blocks.push(afterBlock);
    this.currentBlock = afterBlock;
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
    this.addInstruction(new AllocInstruction(structReg, structType));
    for (let i = 0; i < fieldValues.length; i++) {
      const field = structType.fields[i]
      const fieldValue = fieldValues[i];
      compilerAssert(fieldValue instanceof RValue, 'Struct field value must be an RValue');
      this.addInstruction(new GetFieldPointerInstruction(fieldValue.register, structReg, field.name));
      this.addInstruction(new StoreToAddressInstruction(fieldValue.register, fieldValue.register));
      // this.addInstruction(new StoreFieldInstruction(structReg, field.name, fieldValue.register));
    }
    return new RValue(structReg)
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
      if (fn.params[argIndex].byReference) {
        compilerAssert(argReg instanceof LValue, 'Function argument must be an LValue');
        this.addInstruction(new CheckInitializedInstruction(argReg.address));
        argRegs.push(argReg.address);
      } else {
        compilerAssert(argReg instanceof RValue, 'Function argument must be an RValue');
        argRegs.push(argReg.register);
      }
    }

    const functionName = node.callee;

    // Call the function
    const resultReg = this.newRegister();
    this.addInstruction(new CallInstruction(resultReg, functionName, argRegs));

    return new RValue(resultReg);
  }

  generateAssignmentExpression(node: AssignmentNode, context: ExpressionContext): IRValue {
    const left = node.left;
    const rightReg = this.generateExpression(node.right, context);
    compilerAssert(rightReg instanceof RValue, 'Assignment right-hand side must be an RValue');

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
      this.addInstruction(new StoreToAddressInstruction(destReg.register, rightReg.register));
      return rightReg
    } else if (left instanceof MemberExpressionNode) {
      // Assignment to object field
      const objReg = this.generateExpression(left.object, context);
      compilerAssert(objReg instanceof RValue, 'Object must be an RValue');
      
      const fieldName = left.property;
      // Store rightReg into objReg.fieldName
      this.addInstruction(new GetFieldPointerInstruction(objReg.register, objReg.register, fieldName));
      this.addInstruction(new StoreToAddressInstruction(objReg.register, rightReg.register));
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
    const instr = new BinaryOperationInstruction(resultReg, node.operator, leftReg.register, rightReg.register);
    this.addInstruction(instr);
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
        const valueReg = this.newRegister();
        this.addInstruction(new LoadFromAddressInstruction(valueReg, addressReg.register));
        return new RValue(valueReg);
      }
    }
  }

  generateLiteral(node: LiteralNode, context: ExpressionContext): IRValue {
    const value = node.value;
    const destReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(destReg, value));
    return new RValue(destReg);
  }

  generateMemberExpression(node: MemberExpressionNode, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(node.object, context) as RValue;
    const fieldName = node.property;
    const destReg = this.newRegister();
    // Load objReg.fieldName into destReg
    this.addInstruction(new GetFieldPointerInstruction(destReg, objReg.register, fieldName));
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