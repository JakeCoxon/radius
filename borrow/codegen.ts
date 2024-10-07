import { AndAst, Ast, Binding, BindingAst, CallAst, CompiledFunction, ConstructorAst, FieldAst, IfAst, IntType, LetAst, NumberAst, OperatorAst, OrAst, ReturnAst, SetAst, SetFieldAst, StatementsAst, Type, VoidAst, WhileAst } from "../src/defs";
import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, FunctionParameter, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, Pointer, Value, ReturnInstruction, ReturnNode, StoreToAddressInstruction, Variable, VariableDeclarationNode, WhileStatementNode, compilerAssert, GetFieldPointerInstruction, Capability, AndNode, OrNode, PhiInstruction, CommentInstruction, MoveInstruction, EndAccessInstruction, printIR, MarkInitializedInstruction, InstructionId } from "./defs";

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

  generate(ast: Ast): void {

    if (ast instanceof StatementsAst) {
      for (const stmt of ast.statements) {
        this.generate(stmt);
      }
      return
    }
    if (ast instanceof VoidAst) return

    if (ast instanceof LetAst) {
      return this.generateVariableDeclaration(ast)
    }
    if (ast instanceof SetAst) {
      this.generateAssignmentExpression(ast, { valueCategory: 'rvalue' })
      return
    }
    if (ast instanceof IfAst) {
      return this.generateIfStatement(ast)
    }
    if (ast instanceof WhileAst) {
      return this.generateWhileStatement(ast)
    }
    if (ast instanceof ReturnAst) {
      return this.generateReturnStatement(ast)
    }
    if (ast instanceof SetFieldAst) {
      return this.generateAssignmentField(ast)
    }

    this.generateExpression(ast, { valueCategory: 'rvalue' })
    
    // if (node instanceof LetConstNode) {
    //   this.constants[node.name] = node.value;
    // } else if (node instanceof ProgramNode) {
    //   this.generateProgram(node);
    // } else if (node instanceof VariableDeclarationNode) {
    //   this.generateVariableDeclaration(node);
    // } else if (node instanceof ExpressionStatementNode) {
    //   this.generateExpression(node.expression, { valueCategory: 'rvalue' });
    // } else if (node instanceof IfStatementNode) {
    //   this.generateIfStatement(node);
    // } else if (node instanceof WhileStatementNode) {
    //   this.generateWhileStatement(node);
    // } else if (node instanceof BlockStatementNode) {
    //   this.generateBlockStatement(node);
    // } else if (node instanceof FunctionDeclarationNode) {
    //   this.generateFunctionDeclaration(node);
    // } else if (node instanceof ReturnNode) {
    //   this.generateReturnStatement(node);
    // } else {
      // throw new Error(`Unsupported AST node type: ${node.nodeType}`);
    // }

    // if (node instanceof AssignmentNode) {
    //   return this.generateAssignmentExpression(node, context);
    // } else if (node instanceof BinaryExpressionNode) {
    //   return this.generateBinaryExpression(node, context);
    // } else if (node instanceof IdentifierNode) {
    //   return this.generateIdentifier(node, context);
    // } else if (node instanceof LiteralNode) {
    //   return this.generateLiteral(node, context);
    // } else if (node instanceof MemberExpressionNode) {
    //   return this.generateMemberExpression(node, context);
    // } else if (node instanceof CallExpressionNode) {
    //   return this.generateCallExpression(node, context);
    // } else if (node instanceof CreateStructNode) {
    //   return this.generateCreateStructExpression(node, context);
    // } else if (node instanceof AndNode) {
    //   return this.generateAndExpression(node, context);
    // } else if (node instanceof OrNode) {
    //   return this.generateOrExpression(node, context);
    // } else {
    //   throw new Error(`Unsupported expression type: ${node.nodeType}`);
    // }
    // compilerAssert(false, 'Not implemented codegen', { ast })
  }

  generateExpression(ast: Ast, context: ExpressionContext): IRValue {
    if (ast instanceof NumberAst) {
      return this.generateNumberLiteral(ast, context)
    }
    if (ast instanceof CallAst) {
      return this.generateCallExpression(ast, context)
    }
    if (ast instanceof ConstructorAst) {
      return this.generateCreateStructExpression(ast, context)
    }
    if (ast instanceof BindingAst) {
      return this.generateBinding(ast, context)
    }
    if (ast instanceof FieldAst) {
      return this.generateMemberExpression(ast, context)
    }
    if (ast instanceof AndAst) {
      return this.generateAndExpression(ast, context)
    }
    if (ast instanceof OrAst) {
      return this.generateOrExpression(ast, context)
    }
    if (ast instanceof OperatorAst) {
      return this.generateBinaryExpression(ast, context)
    }
    
    compilerAssert(false, 'Not implemented expression', { ast })
    // this.generate(ast, context)
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

  generateVariableDeclaration(ast: LetAst) {
    const mutable = true // ast.mutable
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
    const mutable = true // ast.binding.mutable
    const capability = mutable ? Capability.Inout : Capability.Let
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, capability));
    
    console.log("Alloc instruction", reg, type)
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
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, rhsLabel, afterLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(ast.args[1], { valueCategory: 'rvalue' });
    compilerAssert(rhsReg instanceof Value, 'Right-hand side of && must be an RValue');
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);
    this.addInstruction(new PhiInstruction(outReg, [lhsReg.register, rhsReg.register]))
    return new Value(outReg);
  }

  generateOrExpression(ast: OrAst, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'or-expression must be an RValue');
    const rhsLabel = this.newLabel();
    const afterLabel = this.newLabel();
    const outReg = this.newRegister();
    const lhsReg = this.generateExpression(ast.args[0], { valueCategory: 'rvalue' });
    compilerAssert(lhsReg instanceof Value, 'Left-hand side of || must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(lhsReg.register, afterLabel, rhsLabel));
    this.newBlock(rhsLabel);
    const rhsReg = this.generateExpression(ast.args[1], { valueCategory: 'rvalue' });
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
      this.addInstruction(new GetFieldPointerInstruction(reg, structReg, fieldIndex));
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
    // const fn = this.functionBlocks.find((fn) => fn.name === node.callee);
    const fn = this.functions.get(ast.binding)
    compilerAssert(fn, `Function ${ast.binding.name} not found`);
    this.addInstruction(new CommentInstruction(`Call ${ast.binding.name}`))

    compilerAssert(ast.args.length === fn.argBindings.length, 'Argument count mismatch');
    
    const argRegs: string[] = [];
    let i = 0
    for (const arg of fn.argBindings) {
      const argIndex = i++;
      const givenArg = ast.args[argIndex];
      const newReg = this.newRegister();
      const inout = false // fn.params[argIndex].capability === Capability.Inout
      if (inout) {
        const argReg = this.generateExpression(givenArg, { valueCategory: 'lvalue' });
        compilerAssert(argReg instanceof Pointer, 'Function argument must be an pointer');
        this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Inout]));
        argRegs.push(newReg);
      } else {
        const argReg = this.generateExpression(givenArg, { valueCategory: 'rvalue' });
        const reg = argReg instanceof Value ? argReg.register : argReg.address;
        // const value = this.toValue(argReg)
        this.addInstruction(new AccessInstruction(newReg, reg, [Capability.Let]));
        argRegs.push(newReg);
      }
    }

    const functionName = ast.binding.name;
  
    // Call the function
    const resultReg = this.newRegister();
    const accessReg = this.newRegister();
    this.addFunctionInstruction(new AllocInstruction(resultReg, fn.returnType));
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set]));
    this.addInstruction(new CallInstruction(accessReg, ast.binding, argRegs));
    return new Pointer(resultReg)
  }

  generateAssignmentExpression(ast: SetAst, context: ExpressionContext): IRValue {
    // const left = ast.

    // Simple assignment to variable
    let variable = this.variableMap.get(ast.binding)
    compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
    const type = variable.type

    const newLocal = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const lvalue = this.storeResult(type, newLocal)

    // const val = this.toValue(rightVal)
    this.generateMovePointerInstruction(variable.register, lvalue, type)
    compilerAssert(variable.capability === Capability.Inout, 'Cannot assign to a let variable');
    return new Pointer(variable.register);
  }
  generateAssignmentField(ast: SetFieldAst) {
    // const checkMutable = (node: MemberExpressionNode) => {
    //   if (node.object instanceof IdentifierNode) {
    //     const variable = this.variableMap.get(node.object.name);
    //     compilerAssert(variable, `Undefined variable: ${node.object.name}`);
    //     return variable.capability === Capability.Inout
    //   } else if (node.object instanceof MemberExpressionNode) {
    //     return checkMutable(node.object)
    //   }
    // }
    // compilerAssert(checkMutable(left), 'Cannot assign to a member of an immutable struct');
    // TODO: Check mutable
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    const fieldType = ast.field.fieldType
    compilerAssert(fieldType, `Struct type not found`);
    const reg = this.newRegister();

    const newValue = this.generateExpression(ast.value, { valueCategory: 'rvalue' });
    const rightReg = this.storeResult(fieldType, newValue)
    this.addInstruction(new GetFieldPointerInstruction(reg, objReg.address, ast.field.index));
    this.generateMovePointerInstruction(reg, rightReg, fieldType);
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

  generateBinaryExpression(ast: OperatorAst, context: ExpressionContext): IRValue {
    const leftReg = this.toValue(this.generateExpression(ast.args[0], context))
    const rightReg = this.toValue(this.generateExpression(ast.args[1], context))

    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.operator, leftReg.register, rightReg.register));
    if (context.valueCategory === 'lvalue') {
      return this.storeResult(IntType, new Value(resultReg))
    }
    return new Value(resultReg)
  }

  generateBinding(ast: BindingAst, context: ExpressionContext): IRValue {
    const addressReg = this.variableMap.get(ast.binding);

    compilerAssert(addressReg, `Undefined variable: ${ast.binding.name}`);

    if (context.valueCategory === 'lvalue') {
      return new Pointer(addressReg.register);
    } else {
      return new Pointer(addressReg.register);
    }
  }

  generateNumberLiteral(ast: NumberAst, context: ExpressionContext): IRValue {
    compilerAssert(context.valueCategory === 'rvalue', 'Literal must be an RValue');
    const value = ast.value;
    const destReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(destReg, value));
    return new Value(destReg);
  }

  generateMemberExpression(ast: FieldAst, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    const destReg = this.newRegister();
    this.addInstruction(new GetFieldPointerInstruction(destReg, objReg.address, ast.field.index));
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
      new CallInstruction(callReg, moveFn.binding, [sourceAccessReg, targetAccessReg]),
      new MarkInitializedInstruction(targetAccessReg, true),
      new MarkInitializedInstruction(sourceAccessReg, false),
      new EndAccessInstruction(sourceAccessReg, [Capability.Sink]),
      new EndAccessInstruction(targetAccessReg, [capability]),
    ];
    block.instructions.splice(instrId.instrId, 1, ...instrs);
  }
}
