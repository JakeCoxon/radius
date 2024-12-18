import { IrFunction, printIrFunction, RegionCodegen, RegionId } from "../region/region_codegen";
import { externalBuiltinBindings } from "../src/compiler_sugar";
import { AliasAst, AndAst, Ast, Binding, BindingAst, BlockAst, BoolAst, BoolType, BreakAst, CallAst, Capability, CastAst, CompiledFunction, compilerAssert, ConstructorAst, DefaultConsAst, EnumVariantAst, FieldAst, FunctionParameter, GlobalCompilerState, IfAst, IntType, LetAst, LetType, MutSigilAst, NeverType, NotAst, NumberAst, OperatorAst, OrAst, ParameterizedType, PrimitiveType, RawPointerType, ReturnAst, SetAst, SetFieldAst, SetSubscriptAst, SetValueFieldAst, SourceLocation, StatementsAst, StringAst, SubscriptAst, Type, UserCallAst, ValueFieldAst, VariantCastAst, VoidAst, VoidType, WhileAst, YieldAst } from "../src/defs";
import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, Pointer, Value, ReturnInstruction, ReturnNode, StoreToAddressInstruction, Variable, VariableDeclarationNode, WhileStatementNode, GetFieldPointerInstruction, AndNode, OrNode, PhiInstruction, CommentInstruction, MoveInstruction, EndAccessInstruction, printIR, MarkInitializedInstruction, InstructionId, PhiSource, DeallocStackInstruction, PointerOffsetInstruction, ProjectBundleInstruction, YieldInstruction, BreakInstruction, GetGlobalAddress, BitCastInstruction } from "./defs";

type ExpressionContext = {
  valueCategory: 'rvalue' | 'lvalue';
}

class Scope {
  allocs: [string, Type][] = []
  regionId: RegionId | null = null
  constructor(
    public debugName: string,
    public breakBlockLabel: string | null = null,
    public breakExprReg: string | null = null
  ) {}
}

export class CodeGenerator {
  functionBlocks: FunctionBlock[] = [];
  // currentFunction: FunctionBlock
  labelCount: number = 0;
  registerCount: number = 0;
  functions: Map<Binding, CompiledFunction> = new Map();
  irFunctions: Map<Binding, IrFunction> = new Map();
  freshId: number = 0;
  globalId: number = 0;

  constructor(public globalCompiler: GlobalCompilerState) {}

  functionGenerator(compiledFunction: CompiledFunction) {
    return new FunctionCodeGenerator(this, compiledFunction, this.globalCompiler)
  }

  newLabel(): string { return `L${this.labelCount++}`; }
  newRegister(): string { return `r${this.registerCount++}`; }
  newFreshId(): string { return `i${this.freshId++}`; }
  newGlobalId(): string { return `g${this.globalId++}` }

}

export class FunctionCodeGenerator {
  variableMap: Map<Binding, Variable> = new Map(); // variable bidning -> register name
  blocks: BasicBlock[] = [];
  currentBlock: BasicBlock;
  functionInstructions: IRInstruction[] = [];
  scopes: Scope[] = [];
  blockScopeDepth: Map<Binding, number> = new Map(); // block binding -> scope depth

  currentFunction: FunctionBlock;
  currentLocation: SourceLocation
  currentStatement: Ast

  unusedBlocks: Set<string> = new Set();

  regionCodegen: RegionCodegen
  irFunction: IrFunction;
  body: Ast;

  constructor(
    public codegen: CodeGenerator,
    public compiledFunction: CompiledFunction,
    public globalCompiler: GlobalCompilerState
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
    if (instr instanceof JumpInstruction) return // Skip for now
    if (instr instanceof ConditionalJumpInstruction) return // Skip for now
    if (instr instanceof PhiInstruction) return // Skip for now

    this.currentBlock.instructions.push(instr);

    this.regionCodegen.ensureBlock()
    this.regionCodegen.insertInstruction(instr)
  }

  // TODO: Clean this up because it's not clear exactly why it's needed
  // Is it only primitive types? If it's copying a value, make it explicit
  toValue(type: Type, value: IRValue, msg: string = ''): Value {
    if (value instanceof Value) { return value; }
    // compilerAssert(type instanceof PrimitiveType, 'Only allowed on primitive types', { type });
    compilerAssert(type !== VoidType, 'Cannot convert to void type', { type, value, msg });
    const reg = this.newRegister();
    const accessReg = this.newRegister();
    compilerAssert(value.address, 'Value must have an address', { type, value });
    this.addInstruction(new CommentInstruction(`Convert to value ${value.address} ${msg}`));
    this.addInstruction(new AccessInstruction(accessReg, value.address, [Capability.Let], type));
    this.addInstruction(new LoadFromAddressInstruction(reg, type, accessReg));
    // this.addInstruction(new MarkInitializedInstruction(value.address, type, false));
    return new Value(reg);
  }

  // Entry point
  generateFunction(binding: Binding, params: FunctionParameter[], returnType: Type, body: Ast) {
    compilerAssert(!this.currentFunction, 'Already generating in a function');
    console.log("Begin generating function", binding.name);

    const paramRegs = params.map((param) => {
      const paramReg = this.newRegister()
      const variable = new Variable(param.binding.name, param.type, paramReg, param.capability);
      this.variableMap.set(param.binding, variable);
      return paramReg
    });

    this.body = body
    const fn = new IrFunction(binding.name, params, paramRegs);
    fn.returnType = returnType
    this.irFunction = fn
    if (!(returnType instanceof PrimitiveType)) {
      fn.returnParameter = new FunctionParameter(new Binding('return', returnType), returnType, false, RawPointerType, Capability.Set);
      const returnReg = this.newRegister();
      const variable = new Variable('return', RawPointerType, returnReg, Capability.Set);
      this.variableMap.set(fn.returnParameter.binding, variable)
      fn.returnRegister = returnReg
      fn.returnType = VoidType
    }
    this.regionCodegen = new RegionCodegen(fn, this.compiledFunction, this.codegen)
    this.regionCodegen.createRootSequenceRegion()    

    const entryLabel = this.newLabel();
    const entryBlock = new BasicBlock(entryLabel, []);
    this.blocks.push(entryBlock);
    this.currentBlock = entryBlock;

    this.scopes.push(new Scope("Function scope"));


    this.currentFunction = new FunctionBlock(binding.name, binding, params, paramRegs, this.blocks);

    // Make sure to use the actual specified returnType, not the body.type
    if (returnType !== VoidType && returnType !== NeverType && body.type !== NeverType) {
      this.body = new ReturnAst(body.type, SourceLocation.anon, body)
      this.generate(this.body)
    } else {
      this.generate(body);
      this.finalizeScope();
      this.addInstruction(new ReturnInstruction(VoidType, null));
    }

    this.blocks[0].instructions.unshift(...this.functionInstructions);

    // Remove unused blocks
    this.currentFunction.blocks = this.blocks.filter(block => !this.unusedBlocks.has(block.label));

    this.codegen.functionBlocks.push(this.currentFunction)

    return this.currentFunction
  }

  generate(ast: Ast): void {
    if (!this.currentLocation) {
      this.currentLocation = SourceLocation.anon
    }
    if (!this.currentStatement) {
      this.currentStatement = ast
    }

    if (ast instanceof StatementsAst) {
      for (const stmt of ast.statements) {
        this.currentStatement = stmt
        if (stmt.location.source !== SourceLocation.anon.source) {
          this.currentLocation = stmt.location
        }
        this.generate(stmt);
      }
      return
    }
    if (ast instanceof VoidAst) return

    if (ast instanceof LetAst)           { return this.generateVariableDeclaration(ast) }
    if (ast instanceof AliasAst)         { return this.generateAliasDeclaration(ast) }
    if (ast instanceof SetAst)           { return this.generateAssignmentStatement(ast) }
    if (ast instanceof IfAst)            { return this.generateIfStatement(ast) }
    if (ast instanceof WhileAst)         { return this.generateWhileStatement(ast) }
    if (ast instanceof ReturnAst)        { return this.generateReturnStatement(ast) }
    if (ast instanceof SetFieldAst)      { return this.generateAssignmentField(ast) }
    if (ast instanceof SetValueFieldAst) { return this.generateAssignmentValueField(ast) }
    if (ast instanceof BlockAst)         { return this.generateBlockStatement(ast) }
    if (ast instanceof SetSubscriptAst)  { return this.generateAssignmentSubscript(ast) }

    this.generateExpression(ast, { valueCategory: 'rvalue' })
    
  }

  generateExpression(ast: Ast, context: ExpressionContext): IRValue {
    if (ast instanceof StringAst)      { return this.generateStringLiteral(ast, context) }
    if (ast instanceof NumberAst)      { return this.generateNumberLiteral(ast, context) }
    if (ast instanceof BoolAst)        { return this.generateBoolLiteral(ast, context) }
    if (ast instanceof CallAst)        { return this.generateCallExpression(ast, context) }
    if (ast instanceof UserCallAst)    { return this.generateUserCallExpression(ast, context) }
    if (ast instanceof ConstructorAst) { return this.generateCreateStructExpression(ast, context) }
    if (ast instanceof EnumVariantAst) { return this.generateEnumVariantExpression(ast, context) }
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
    if (ast instanceof VariantCastAst) { return this.generateVariantCastExpression(ast, context) }
    if (ast instanceof YieldAst)       { return this.generateYieldExpression(ast, context) }
    if (ast instanceof BreakAst)       { return this.generateBreakExpression(ast, context) }
    // if (ast instanceof MutSigilAst)    { return this.generateExpression(ast.expr, context) }
    if (ast instanceof StatementsAst)  { return this.generateStatementsExpression(ast, context) }

    compilerAssert(false, 'Not implemented expression', { ast, fnBody: this.body })
  }

  generateStatementsExpression(ast: StatementsAst, context: ExpressionContext): IRValue {
    for (const stmt of ast.statements.slice(0, -1)) {
      this.generate(stmt);
    }
    if (ast.type !== VoidType && ast.type !== NeverType) {
      return this.generateExpression(ast.statements[ast.statements.length - 1], context);
    } else {
      this.generate(ast.statements[ast.statements.length - 1])
      return new Pointer('')
    }
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
    compilerAssert(type !== VoidType, 'Cannot allocate void type');
    compilerAssert(type !== NeverType, 'Cannot allocate never type');
    compilerAssert(!type.typeInfo.isReferenceType, "Not implemented reference type", { type })
    const reg = this.newRegister()
    this.functionInstructions.push(new AllocInstruction(reg, type))
    this.scopes[this.scopes.length - 1].allocs.push([reg, type])

    this.regionCodegen.insertBlockInstruction(this.regionCodegen.allocBlock!, new AllocInstruction(reg, type));
    return reg
  }

  finalizeScope() {
    compilerAssert(this.scopes.length > 0, 'No scopes to close');
    const scope = this.scopes[this.scopes.length - 1]
    const allocs = [...scope.allocs].reverse()
    this.addInstruction(new CommentInstruction(`Finalize scope for ${scope.debugName} ${scope.breakBlockLabel}`))
    for (const alloc of allocs) {
      this.addInstruction(new DeallocStackInstruction(alloc[0], alloc[1]))
    }
  }

  // TODO: Fold these together
  generateBlockStatement(ast: BlockAst) {

    const scopeRegionId = this.regionCodegen.insertNewScopeRegion()
    const scopeRegion = this.regionCodegen.getScopeRegion(scopeRegionId)
    this.regionCodegen.insertChildSequenceAndPushState(scopeRegionId)

    this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.bodySequence)

    const blockRegion = this.regionCodegen.insertNewBlockRegion();
    this.regionCodegen.insertChildSequence(blockRegion)
    this.regionCodegen.blockRegion = blockRegion

    this.addInstruction(new CommentInstruction(`Block ${ast.binding.name}`))

    const label = this.newLabel()
    const scope = new Scope(`Block stmt ${ast.binding.name}`, label)
    scope.regionId = scopeRegionId
    this.scopes.push(scope);
    this.blockScopeDepth.set(ast.binding, this.scopes.length - 1)
    this.generate(ast.body)
    this.finalizeScope()

    this.regionCodegen.popRegionState()

    this.scopes.pop()
    this.newBlock(label)
  }

  generateBlockExpression(ast: BlockAst, context: ExpressionContext): IRValue {
    // Don't generate scope for now
    // return this.generateExpression(ast.body, context)
    const label = this.newLabel()
    const scope = new Scope("Block expr", label)

    if (ast.breakExprBinding) scope.breakExprReg = this.generateAlloc(ast.breakExprBinding.type)
    const resultPtr = scope.breakExprReg ?? this.generateAlloc(ast.type)
  
    this.scopes.push(scope)
    this.blockScopeDepth.set(ast.binding, this.scopes.length - 1)

    const scopeRegionId = this.regionCodegen.insertNewScopeRegion()
    const scopeRegion = this.regionCodegen.getScopeRegion(scopeRegionId)
    this.regionCodegen.insertChildSequenceAndPushState(scopeRegionId)
    scope.regionId = scopeRegionId
    
    this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.bodySequence)
    const blockRegion = this.regionCodegen.insertNewBlockRegion();

    this.regionCodegen.insertChildSequence(blockRegion)
    this.regionCodegen.blockRegion = blockRegion

    this.addInstruction(new CommentInstruction(`Block ${ast.binding.name}`))

    const value = this.generateExpression(ast.body, context)
    if (ast.body.type !== NeverType) this.generateMoveToAddressInstruction(resultPtr, value, ast.type)
    this.finalizeScope()

    this.regionCodegen.popRegionState()

    this.scopes.pop()
    this.newBlock(label)
    return new Pointer(resultPtr)
  }

  generateBreakExpression(ast: BreakAst, context: ExpressionContext): IRValue {
    // Break can be an expression that returns NeverType
    compilerAssert(ast.type === NeverType, 'Break expression must be of type never', { ast })
    const depth = this.blockScopeDepth.get(ast.binding)
    compilerAssert(depth !== undefined, `Block depth not found: ${ast.binding.name}`)
    const scope = this.scopes[depth]
    compilerAssert(scope, `Block scope not found: ${ast.binding.name}`)
    const label = scope.breakBlockLabel
    compilerAssert(label, `Break label not found: ${ast.binding.name}`)
    compilerAssert(scope.regionId, `Region id not found: ${ast.binding.name}`)
    this.addInstruction(new CommentInstruction(`Break ${ast.binding.name} ${scope.regionId}`))
    
    // compilerAssert(ast.expr === null, 'Break statement must not have an expression', { ast })
    ;(() => {
      if (!ast.expr) return
      const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' })
      if (ast.expr.type === VoidType || ast.expr.type === NeverType) return
      const dest = scope.breakExprReg
      if (!dest) return // Block was not an expression
      // compilerAssert(dest, 'Break expression register not found', { ast, blockAst: scope.ast })
      this.generateMoveToAddressInstruction(dest, value, ast.expr.type)
    })()  
    this.addInstruction(new BreakInstruction(scope.regionId, VoidType, null))
    // this.addInstruction(new JumpInstruction(label))
    this._createUnusedBlock()
    return new Pointer('')
  }

  generateReturnStatement(ast: ReturnAst) {
    
    if (!ast.expr || ast.expr.type === NeverType) {
      this.finalizeScope()
      this.addInstruction(new ReturnInstruction(VoidType, null));
    } else {
      compilerAssert(ast.type !== VoidType, 'Return type must not be void', { ast })
      const returnReg = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
      if (ast.expr.type instanceof PrimitiveType) {
        const value = this.toValue(ast.type, returnReg, 'return')
        this.finalizeScope()
        this.addInstruction(new ReturnInstruction(ast.expr.type, value.register));
      } else {
        // Perform a move to the return variable
        compilerAssert(returnReg instanceof Pointer, 'Return value must be a pointer', { ast })
        const returnParameter = this.irFunction.returnParameter;
        compilerAssert(returnParameter, 'Return parameter not found', { ast })
        const variable = this.variableMap.get(returnParameter.binding)
        compilerAssert(variable, 'Return variable not found', { ast })
        this.generateMovePointerInstruction(variable.register, returnReg, ast.expr.type)
        this.finalizeScope()
        this.addInstruction(new ReturnInstruction(VoidType, null));
      }
    }
    this._createUnusedBlock();
  }

  generateCastExpression(ast: CastAst, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const reg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Cast ${ast.expr.type.shortName} to ${ast.type.shortName}`))
    if (ast.type === RawPointerType) {
      // Ugly weird stuff. Come back to this later
      if (!(ast.expr instanceof BindingAst)) {
        const v = this.toValue(ast.expr.type, value, 'cast')
        return new Value(v.register)
      } 
      compilerAssert(value instanceof Pointer, 'Expected pointer')
      // TODO: Temporary hack
      this.addInstruction(new MarkInitializedInstruction(value.address, ast.type, true));
      return new Value(value.address)
    }
    const v = this.toValue(ast.expr.type, value, 'cast')
    this.addInstruction(new BinaryOperationInstruction(reg, ast.type, 'cast', v.register, '', ast.expr.type));
    return new Value(reg);
  }

  generateVariantCastExpression(ast: VariantCastAst, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const reg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Variant cast ${ast.expr.type.shortName} to ${ast.type.shortName}`))
    const v = this.storeResult(ast.expr.type, value)
    this.addInstruction(new BitCastInstruction(reg, ast.type, v.address, ast.expr.type));
    return new Pointer(reg);
  }

  generateYieldExpression(ast: YieldAst, context: ExpressionContext): IRValue {
    const expr = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const pointer = this.storeResult(ast.expr.type, expr)
    const reg = this.newRegister();
    compilerAssert(ast.expr.type !== VoidType, 'Cannot yield void type', { ast });
    this.addInstruction(new YieldInstruction(reg, ast.expr.type, pointer.address));
    return new Value(reg);
  }

  generateVariableDeclaration(ast: LetAst) {
    if (ast.letType === LetType.Alias) return this.generateAliasDeclaration(new AliasAst(ast.type, ast.location, ast.binding, ast.value!))

    this.addInstruction(new CommentInstruction(`let ${ast.letType} ${ast.binding.name}`))
    const value = (() => {
      if (!ast.value) return null
      const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
      return this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' }) 
    })()

    if (ast.letType === LetType.VarRef || ast.letType === LetType.Let) {
      return this.generateProjection(ast, value);
    }

    this.generateMutableVariableDeclaration(ast, value);
  }

  generateAliasDeclaration(ast: AliasAst) {
    compilerAssert(ast.type !== NeverType, 'Cannot alias never type', { ast });
    // Like let but with lower capabilities
    this.addInstruction(new CommentInstruction(`Alias ${ast.binding.name}`))
    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const value = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' }) 
    const type = ast.value.type;
    const ptr = this.storeResult(type, value)
    const reg = this.newRegister();
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, Capability.Sink, true));
    // this.addInstruction(new AccessInstruction(reg, ptr.address, [Capability.Sink], type));
    this.addInstruction(new AssignInstruction(reg, type, ptr.address));
  }

  generateProjection(ast: LetAst, value: IRValue | null) {
    const type = ast.binding.type
    const reg = this.newRegister();
    const letType = ast.letType
    compilerAssert(letType, 'Let type not found');
    const capability = letType === LetType.VarRef ? Capability.Inout : Capability.Let
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, capability));
    compilerAssert(value, 'Let binding must have an initializer');
    const ptr = this.storeResult(type, value)
    // compilerAssert(value instanceof Pointer, 'Let binding must have an lvalue initializer');
    this.addInstruction(new CommentInstruction(`Projection ${ast.binding.name}`))
    this.addInstruction(new AccessInstruction(reg, ptr.address, [capability], type));
  }

  generateMutableVariableDeclaration(ast: LetAst, value: IRValue | null) {
    const type = ast.binding.type
    const reg = this.generateAlloc(type);
    
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, Capability.Sink));
    
    if (!value) return
    this.generateMoveToAddressInstruction(reg, value, type)
  }

  

  // TODO: Fold these together
  generateIfStatement(ast: IfAst) {
    this.generateIf(ast, false)
  }

  generateIfExpression(ast: IfAst, context: ExpressionContext): IRValue {
    const isExpr = ast.type !== VoidType && ast.type !== NeverType
    return this.generateIf(ast, isExpr)
  }

  generateIf(ast: IfAst, isExpression: boolean): IRValue {
    const thenLabel = this.newLabel();
    const elseLabel = this.newLabel();
    const afterLabel = this.newLabel()
    const outReg = isExpression ? this.generateAlloc(ast.type) : null

    const ifRegionId = this.regionCodegen.insertNewIfRegion()
    this.regionCodegen.insertChildSequenceAndPushState(ifRegionId)

    const ifRegion = this.regionCodegen.getIfRegion(ifRegionId);
    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.conditionSequence)
    
    const conditionValue = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const conditionReg = this.toValue(ast.expr.type, conditionValue, 'if cond')
    ifRegion.conditionRegister = conditionReg.register

    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, thenLabel, elseLabel));

    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.thenSequence)

    this.newBlock(thenLabel);
    if (isExpression) {
      const reg = this.toValue(ast.trueBody.type, this.generateExpression(ast.trueBody, { valueCategory: 'rvalue' }))
      this.generateMovePrimitiveToAddressInstruction(outReg!, reg, ast.type)
    }
    else this.generate(ast.trueBody);
    this.addInstruction(new JumpInstruction(afterLabel));

    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.elseSequence)

    this.newBlock(elseLabel);
    if (ast.falseBody) {
      if (isExpression) {
        const reg = this.toValue(ast.falseBody.type, this.generateExpression(ast.falseBody, { valueCategory: 'rvalue' }))
        this.generateMovePrimitiveToAddressInstruction(outReg!, reg, ast.type)
      }
      else this.generate(ast.falseBody);
    }
    this.addInstruction(new JumpInstruction(afterLabel));
    this.newBlock(afterLabel);

    this.regionCodegen.popRegionState()

    if (isExpression) return new Pointer(outReg!);

    return new Pointer('')
  }

  generateWhileStatement(ast: WhileAst) {
    const conditionLabel = this.newLabel();
    const bodyLabel = this.newLabel();
    const afterLabel = this.newLabel();

    const whileRegion = this.regionCodegen.insertNewWhileRegion();
    this.regionCodegen.insertChildSequenceAndPushState(whileRegion)

    // Jump to condition check
    this.addInstruction(new JumpInstruction(conditionLabel));

    this.regionCodegen.enterRegionSequence(whileRegion, this.regionCodegen.getWhileRegion(whileRegion).conditionSequence)

    const conditionBlock = new BasicBlock(conditionLabel, []);
    this.blocks.push(conditionBlock);
    this.currentBlock = conditionBlock;
    const conditionReg = this.generateExpression(ast.condition, { valueCategory: 'rvalue' });
    compilerAssert(conditionReg instanceof Value, 'While condition must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, bodyLabel, afterLabel));
    this.regionCodegen.getWhileRegion(whileRegion).conditionRegister = conditionReg.register

    this.regionCodegen.enterRegionSequence(whileRegion, this.regionCodegen.getWhileRegion(whileRegion).bodySequence)

    this.newBlock(bodyLabel);
    this.generate(ast.body);

    this.regionCodegen.popRegionState()

    this.addInstruction(new JumpInstruction(conditionLabel));
    this.newBlock(afterLabel);

    this.addInstruction(new CommentInstruction('End of while loop'))
  }

  generateAndExpression(ast: AndAst, context: ExpressionContext): IRValue {
    // Before we had a cleaner way to do this, but for now we can just use an if expression
    compilerAssert(ast.type === BoolType, 'And expression must be a boolean', { ast });
    return this.generateIfExpression(new IfAst(BoolType, ast.location, ast.args[0], ast.args[1], new BoolAst(BoolType, SourceLocation.anon, false)), context)
  }

  generateOrExpression(ast: OrAst, context: ExpressionContext): IRValue {
    // Before we had a cleaner way to do this, but for now we can just use an if expression
    compilerAssert(ast.type === BoolType, 'Or expression must be a boolean', { ast });
    return this.generateIfExpression(new IfAst(BoolType, ast.location, ast.args[0], new BoolAst(BoolType, SourceLocation.anon, true), ast.args[1]), context)
  }

  generateCreateStructExpression(ast: ConstructorAst, context: ExpressionContext): IRValue {
    // compilerAssert(context.valueCategory === 'rvalue', 'Struct creation must be an RValue');
    const structType = ast.type
    compilerAssert(structType, `Struct type not found`);
    compilerAssert(ast.args.length === structType.typeInfo.fields.length, 'Field count mismatch', { ast });
    const fnBinding = structType.typeInfo.metaobject.constructorBinding
    compilerAssert(fnBinding && fnBinding instanceof Binding, `Constructor not found for ${structType.shortName}`);

    const structReg = this.generateAlloc(structType)
    
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
    return new Pointer(structReg)
  }

  generateEnumVariantExpression(ast: EnumVariantAst, context: ExpressionContext): IRValue {
    const constr = new ConstructorAst(ast.variantType, ast.location, ast.args)
    // compilerAssert(false, "Not implemented", { ast, constr })
    return this.generateCreateStructExpression(constr, context)
  }

  generateDefaultConstructorExpression(ast: DefaultConsAst, context: ExpressionContext): IRValue {
    compilerAssert(false, `Don't use default constructor AST, use createDefaultConstructorAst instead`, { ast })
  }

  storeResult(type: Type, value: IRValue) {
    if (value instanceof Pointer) return value
    const reg = this.generateAlloc(type);
    this.generateMovePrimitiveToAddressInstruction(reg, value, type)
    return new Pointer(reg)
  }

  generateFunctionArgument(ast: Ast, reference: boolean, passingType: Type, capability: Capability): string {
    let hasMutSigil = false
    if (ast instanceof MutSigilAst) {
      hasMutSigil = true
      ast = ast.expr
    }

    // @ParameterPassing
    const newReg = this.newRegister();
    if (reference) {
      if (capability === Capability.Sink || capability === Capability.Set || capability === Capability.Inout) {
        const [expectedCapability, owned] = this.getCapabilityAndOwnership(ast)
        compilerAssert(expectedCapability !== Capability.Let, 'Cannot mutate or sink an immutable variable', { ast, stmt: this.currentStatement, location: ast.location.source ? ast.location : this.currentLocation });
        if (!owned) compilerAssert(hasMutSigil, 'Expected mutation sigil on mutable argument', { ast, stmt: this.currentStatement, location: ast.location.source ? ast.location : this.currentLocation });
      }

      const argReg = this.generateExpression(ast, { valueCategory: 'lvalue' });
      compilerAssert(argReg instanceof Pointer, 'Function argument must be an pointer', { ast, capability, passingType });
      this.addInstruction(new AccessInstruction(newReg, argReg.address, [capability], passingType));
      return newReg
    }

    // Primitive types are allow to be passed without a
    // mutation sigil and they will be copied instead.
    if (!hasMutSigil) capability = Capability.Let

    const argReg = this.generateExpression(ast, { valueCategory: 'rvalue' });
    
    // if (ast.type instanceof PrimitiveType) {
      const value = this.toValue(ast.type, argReg, 'function arg')
      this.addInstruction(new AccessInstruction(newReg, value.register, [capability], passingType));
    // } else {
      // compilerAssert(argReg instanceof Pointer, "Expected pointer")
      // this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Let]));
    // }

    if (hasMutSigil) {
      compilerAssert(capability !== Capability.Let, 'Unexpected mutation sigil', { ast, stmt: this.currentStatement, location: ast.location.source ? ast.location : this.currentLocation });
      // compilerAssert(argReg instanceof Pointer, 'Function argument must be an pointer', { ast, capability, passingType });
      const ptr = this.storeResult(ast.type, argReg)
      this.addInstruction(new MarkInitializedInstruction(ptr.address, ast.type, false));
    }

    return newReg

  }

  generateCopyCall(ast: Ast, context: ExpressionContext): IRValue {
    const binding = new Binding('copy', ast.type)
    
    this.generate(new LetAst(VoidType, SourceLocation.anon, binding, null, LetType.Var))
    const bindingAst = new BindingAst(binding.type, SourceLocation.anon, binding)
    const copyConstructor = ast.type.typeInfo.metaobject.copyConstructorBinding
    compilerAssert(copyConstructor && copyConstructor instanceof Binding, `Copy constructor not found for ${ast.type.shortName}`);
    const dest = new MutSigilAst(bindingAst.type, SourceLocation.anon, bindingAst);
    this.generateCallExpression(new CallAst(ast.type, SourceLocation.anon, copyConstructor, [dest, ast], []), context)
    return this.generateBinding(new BindingAst(binding.type, SourceLocation.anon, binding), context)
  }

  generatePrimitiveCopy(ast: Ast) {
    const targetAccessReg = this.newRegister();
    const type = ast.type;
    compilerAssert(type !== VoidType, 'Cannot copy void type');
    const source = this.generateExpression(ast, { valueCategory: 'rvalue' });
    let valueReg = source instanceof Value ? source.register : this.newRegister();
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
    return new Pointer(targetPointer);
  }

  generatePrint(ast: Ast, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast, { valueCategory: 'rvalue' });
    const valueReg = this.toValue(ast.type, value, 'print')
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.print, [valueReg.register], [ast.type], [Capability.Let]));
    return new Pointer('')
  }

  generatePrintf(args: Ast[], context: ExpressionContext): IRValue {
    const format = this.generateExpression(args[0], { valueCategory: 'rvalue' });
    const formatReg = this.toValue(args[0].type, format, 'printf')
    const argRegs: string[] = [];
    for (let i = 1; i < args.length; i++) {
      const arg = this.generateExpression(args[i], { valueCategory: 'rvalue' });
      const argReg = this.toValue(args[i].type, arg, 'printf')
      argRegs.push(argReg.register)
    }
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.printf, [formatReg.register, ...argRegs], args.map(a => a.type), args.map(a => Capability.Let)));
    return new Pointer('')
  }

  generateExit(args: Ast[], context: ExpressionContext): IRValue {
    const arg = this.generateExpression(args[0], { valueCategory: 'rvalue' });
    const argReg = this.toValue(args[0].type, arg, 'exit')
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.exit, [argReg.register], [args[0].type], [Capability.Let]));
    return new Pointer('')
  }

  generateCallExpression(ast: CallAst, context: ExpressionContext): IRValue {
    if (ast.binding === externalBuiltinBindings.copy) {
      if (ast.args[0].type instanceof PrimitiveType) return this.generatePrimitiveCopy(ast.args[0]);
      return this.generateCopyCall(ast.args[0], context)
    } else if (ast.binding === externalBuiltinBindings.print) {
      return this.generatePrint(ast.args[0], context)
    } else if (ast.binding === externalBuiltinBindings.printf) {
      return this.generatePrintf(ast.args, context)
    } else if (ast.binding === externalBuiltinBindings.exit) {
      return this.generateExit(ast.args, context)
    } else if (ast.binding === externalBuiltinBindings.initializer) {
      this.addInstruction(new CallInstruction(null, VoidType, ast.binding, [], [], []))
      return new Pointer('')
    }
    compilerAssert(ast.binding instanceof Binding, 'Expected binding', { ast });

    // Generate code for arguments
    const fn = this.codegen.functions.get(ast.binding)
    compilerAssert(fn, `Function ${ast.binding.name} not found`, {
      c: this.codegen.functions
    });
    this.addInstruction(new CommentInstruction(`Call ${ast.binding.name}`))
    const fnIr = this.codegen.irFunctions.get(ast.binding)
    if (fn.body) compilerAssert(fnIr, `Function ${ast.binding.name} not found`, { ast });
    
    compilerAssert(ast.args.length === fn.argBindings.length, 'Argument count mismatch', { binding: ast.binding, got: ast.args.length, expected: fn.argBindings.length });
    
    const argRegs: string[] = [];
    const capabilities = fn.parameters.map(p => p.capability)
    const argTypes = fn.parameters.map(p => p.type)

    let i = 0
    for (const givenArg of ast.args) {
      const argIndex = i++;
      const param = fn.parameters[argIndex]
      compilerAssert(param.capability, `Capability not found for ${param.binding.name}`);
      const argReg = this.generateFunctionArgument(givenArg, param.reference, param.passingType, param.capability);
      argRegs.push(argReg);
    }

    let resultReg: string | null = null
    if (fnIr?.returnParameter) {
      resultReg = this.generateAlloc(fn.returnType);
      argRegs.unshift(resultReg)
      capabilities.unshift(Capability.Set)
      argTypes.unshift(RawPointerType)
    }

    // Call the function
    const returnType = fnIr?.returnType || fn.returnType
    if (returnType === VoidType) {
      this.addInstruction(new CallInstruction(null, returnType, ast.binding, argRegs, argTypes, capabilities))
      if (fnIr?.returnParameter) return new Pointer(resultReg!)
      return new Pointer('') // hack. Make sure this is not used
    }
    const accessReg = this.newRegister()
    resultReg = this.generateAlloc(fn.returnType);
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set], fn.returnType));
    this.addInstruction(new CallInstruction(accessReg, fn.returnType, ast.binding, argRegs, argTypes, capabilities))
    return new Pointer(resultReg)
  }

  generateUserCallExpression(ast: UserCallAst, context: ExpressionContext): IRValue {
    return this.generateCallExpression(new CallAst(ast.type, SourceLocation.anon, ast.binding, ast.args, []), context)
  }

  generateAssignmentStatement(ast: SetAst) {
    let variable = this.variableMap.get(ast.binding)
    if (!variable) {
      const isInitializer = this.compiledFunction.binding === this.globalCompiler.initializerFunction?.binding
      const global = this.globalCompiler.globalVars.get(ast.binding)
      compilerAssert(global, `Undefined variable: ${ast.binding.name}`);
      compilerAssert(global.letType === LetType.Var || isInitializer, 'Cannot assign to a let variable', { location: ast.location, global, ast })
      const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
      const value = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
      const lvalue = this.storeResult(global.type, value)
      const targetReg = this.newRegister();
      this.addInstruction(new GetGlobalAddress(targetReg, global.type, global.register));
      this.generateMovePointerInstructionWithCapabilityCheck(targetReg, lvalue, ast.value)
      return
    }
    compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
    const type = variable.type

    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const value = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
    const lvalue = this.storeResult(type, value)

    compilerAssert(variable.capability === Capability.Inout || variable.capability === Capability.Set || variable.capability === Capability.Sink, 'Cannot assign to a let variable', { location: ast.location, variable, ast })
    this.generateMovePointerInstructionWithCapabilityCheck(variable.register, lvalue, ast.value)
  }

  ensureMutable(ast: Ast): boolean {
    if (ast instanceof BindingAst) {
      const variable = this.variableMap.get(ast.binding);
      compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
      const mutable = variable.capability === Capability.Inout || variable.capability === Capability.Set || variable.capability === Capability.Sink
      compilerAssert(mutable, 'Cannot assign to a member of an immutable struct', { location: ast.location, variable, body: this.body  });
      return mutable
    } else if (ast instanceof FieldAst) {
      return this.ensureMutable(ast.left)
    } else if (ast instanceof ValueFieldAst) {
      return this.ensureMutable(ast.left)
    } else if (ast instanceof MutSigilAst) {
      return this.ensureMutable(ast.expr)
    } else if (ast instanceof SubscriptAst) {
      return this.ensureMutable(ast.left)
    }
    compilerAssert(false, 'Not implemented mutable check', { ast })
  }

  generateAssignmentField(ast: SetFieldAst) {
    this.ensureMutable(ast.left)
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    const fieldType = ast.field.fieldType
    const reg = this.newRegister();

    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const newValue = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
    const rightReg = this.storeResult(fieldType, newValue)
    this.addInstruction(new GetFieldPointerInstruction(reg, objReg.address, ast.field));
    this.generateMovePointerInstructionWithCapabilityCheck(reg, rightReg, ast.value)
  }

  generateAssignmentValueField(ast: SetValueFieldAst) {
    this.ensureMutable(ast.left)
    this.addInstruction(new CommentInstruction(`Set value field ${ast.fieldPath.map(x => x.name).join(", ")}`))
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    let destReg: string = objReg.address
    ast.fieldPath.forEach(field => {
      const source = destReg
      destReg = this.newRegister();
      this.addInstruction(new GetFieldPointerInstruction(destReg, source, field));
    })
    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const newValue = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
    const rightReg = this.storeResult(ast.value.type, newValue)
    this.generateMovePointerInstructionWithCapabilityCheck(destReg, rightReg, ast.value)
  }

  getCapabilityAndOwnership(value: Ast): [Capability, boolean] {
    // Try to get the capability and whether the value needs
    // to be owned, by visiting the AST a bit.
    // Not sure about this.
    if (value instanceof BindingAst) {
      const variable = this.variableMap.get(value.binding)
      compilerAssert(variable, 'Variable not found', { value })
      if (variable.alias) return [variable.capability, true]
      return [variable.capability, false]
    }
    if (value instanceof ValueFieldAst)  return this.getCapabilityAndOwnership(value.left)
    if (value instanceof FieldAst)       return this.getCapabilityAndOwnership(value.left)
    if (value instanceof UserCallAst)    return [Capability.Sink, true]
    if (value instanceof CallAst)        return [Capability.Sink, true]
    if (value instanceof NumberAst)      return [Capability.Let,  false]
    if (value instanceof OperatorAst)    return [Capability.Let,  false]
    if (value instanceof StringAst)      return [Capability.Sink, true]
    if (value instanceof ConstructorAst) return [Capability.Sink, true]
    if (value instanceof EnumVariantAst) return [Capability.Sink, true]
    if (value instanceof DefaultConsAst) return [Capability.Sink, true]
    if (value instanceof SubscriptAst)   return [Capability.Sink, false]
    if (value instanceof BlockAst)       return this.getCapabilityAndOwnership(value.body)
    if (value instanceof StatementsAst)  return this.getCapabilityAndOwnership(value.statements[value.statements.length - 1])
    if (value instanceof CastAst)        return this.getCapabilityAndOwnership(value.expr)
    compilerAssert(false, 'Not implemented', { value, currentStatement: this.currentStatement })
  }

  generateMovePointerInstructionWithCapabilityCheck(targetPointer: string, sourcePointer: Pointer, valueAst: Ast) {
    let hasMutSigil = false
    if (valueAst instanceof MutSigilAst) {
      hasMutSigil = true
      valueAst = valueAst.expr
    }

    const [sourceCapability, owned] = this.getCapabilityAndOwnership(valueAst)
    const type = valueAst.type

    if (!owned && (sourceCapability === Capability.Sink || sourceCapability === Capability.Inout)) {
      compilerAssert(hasMutSigil, 'Cannot move a mutable value without a mutation sigil', { sourceCapability, location: valueAst.location.source ? valueAst.location : this.currentLocation, stmt: this.currentStatement, valueAst })
    }

    console.log("generateMovePointerInstructionWithCapabilityCheck", { sourceCapability, owned, hasMutSigil, type, targetPointer, sourcePointer })

    if (type instanceof PrimitiveType) {
      compilerAssert(type !== VoidType, 'Cannot move void type');
      const targetAccessReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      const valueReg = this.newRegister();
      this.addInstruction(new AccessInstruction(sourceAccessReg, sourcePointer.address, [sourceCapability], type));
      this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg));
      this.addInstruction(new CommentInstruction("Maybe mark"))
      if (sourceCapability === Capability.Sink)
        this.addInstruction(new MarkInitializedInstruction(sourceAccessReg, type, false));
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type));
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg));
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]));

    } else {
      if (sourceCapability === Capability.Let) {
        compilerAssert(false, 'Not implemented. probably an error', { sourceCapability })
      }
      this.addInstruction(new MoveInstruction(targetPointer, sourcePointer.address, type));
    }
  }

  generateMovePointerInstruction(targetPointer: string, sourcePointer: Pointer, type: Type) {
    compilerAssert(targetPointer, 'Target pointer must be defined', { targetPointer, sourcePointer, type })
    compilerAssert(sourcePointer.address, 'Source pointer must be defined', { targetPointer, sourcePointer, type })
    if (type instanceof PrimitiveType) {
      compilerAssert(type !== VoidType, 'Cannot move void type');
      const targetAccessReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      const valueReg = this.newRegister();
      this.addInstruction(new AccessInstruction(sourceAccessReg, sourcePointer.address, [Capability.Sink, Capability.Let], type));
      this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg));
      this.addInstruction(new CommentInstruction("Mark"))
      this.addInstruction(new MarkInitializedInstruction(sourceAccessReg, type, false));
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type));
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg));
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]));
    } else {
      this.addInstruction(new MoveInstruction(targetPointer, sourcePointer.address, type));
    }
  }

  generateMovePrimitiveToAddressInstruction(destReg: string, value: Value, type: Type) {
    compilerAssert(type instanceof PrimitiveType, 'Not implemented for non-primitive types');
    const destAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(destAccessReg, destReg, [Capability.Set], type));
    this.addInstruction(new StoreToAddressInstruction(destAccessReg, type, value.register));
    this.addInstruction(new EndAccessInstruction(destAccessReg, [Capability.Set]));
  }

  generateMoveToAddressInstruction(destReg: string, value: IRValue, type: Type) {
    if (value instanceof Value) {
      this.generateMovePrimitiveToAddressInstruction(destReg, value, type)
    } else {
      this.generateMovePointerInstruction(destReg, value, type)
    }
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
    if (!addressReg) {
      if (this.globalCompiler.globalVars.has(ast.binding)) {
        const global = this.globalCompiler.globalVars.get(ast.binding)
        compilerAssert(global, `Undefined variable: ${ast.binding.name}`);
        const reg = this.newRegister();
        this.addInstruction(new GetGlobalAddress(reg, global.type, global.register));
        return new Pointer(reg)
      }
    }
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
    // compilerAssert(context.valueCategory === 'rvalue', 'Literal must be an RValue');
    if (ast.type === RawPointerType) {
      const resultPtr = this.newRegister();
      this.addInstruction(new LoadConstantInstruction(resultPtr, ast.type, ast.value));
      return new Value(resultPtr)
    }
    
    const args = [
      new NumberAst(IntType, SourceLocation.anon, ast.value.length),
      new StringAst(RawPointerType, SourceLocation.anon, ast.value),
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

    // This is a low level operation, probably should just have a different AST node for it
    if (ast.left.type === RawPointerType) {
      const destReg = this.newRegister();
    
      const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' })
      const objRegValue = this.toValue(ast.left.type, objReg, 'subscript object')
      // compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
      
      const offset = this.toValue(ast.right.type,
        this.generateExpression(ast.right, { valueCategory: 'rvalue' }))
      this.addInstruction(new PointerOffsetInstruction(destReg, objRegValue.register, ast.type, offset.register));
      return new Pointer(destReg);
    }
    const destReg = this.newRegister();
    
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' })
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');

    const offset = this.toValue(ast.right.type,
      this.generateExpression(ast.right, { valueCategory: 'rvalue' }))
  
    compilerAssert(ast.funcs, 'Subscript must have funcs', { ast })
    const caps = [Capability.Let, Capability.Inout, Capability.Set, Capability.Sink]
    this.addInstruction(new ProjectBundleInstruction(destReg, ast.type, caps, objReg.address, [offset.register], ast.funcs as any));
    return new Pointer(destReg);

  }

  generateAssignmentSubscript(ast: SetSubscriptAst) {
    compilerAssert(ast.left.type === RawPointerType, 'Subscript assignment only supported for raw pointers');
    this.ensureMutable(ast.left)
    const objReg = this.toValue(ast.left.type,
      this.generateExpression(ast.left, { valueCategory: 'lvalue' }),
      'subscript object')
    
    const elementType = ast.value.type
    const reg = this.newRegister();

    const offset = this.toValue(ast.right.type, this.generateExpression(ast.right, { valueCategory: 'rvalue' }), 'subscript offset')
    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const newValue = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
    const valueReg = this.storeResult(elementType, newValue)
    this.addInstruction(new PointerOffsetInstruction(reg, objReg.register, elementType, offset.register));

    this.generateMovePointerInstructionWithCapabilityCheck(reg, valueReg, ast.value)
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
    console.log("Remove instruction", block.instructions[instrId.instrId])
    block.instructions.splice(instrId.instrId, 1);
  }

  replaceInstruction(block: BasicBlock, instrId: InstructionId, instr: IRInstruction) {
    console.log("Replacing instruction", block.instructions[instrId.instrId])
    block.instructions.splice(instrId.instrId, 1, instr);
  }

  spliceInstructions(block: BasicBlock, instrId: InstructionId, deleteCount: number, instrs: IRInstruction[]) {
    console.log("Splicing instruction", block.instructions.slice(instrId.instrId, instrId.instrId + deleteCount))
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
