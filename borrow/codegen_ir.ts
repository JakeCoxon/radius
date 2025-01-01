import { InstructionId, IrFunction, printIrFunction, RegionCodegen, RegionId, SequenceId } from "../region/region_codegen";
import { externalBuiltinBindings } from "../src/compiler_sugar";
import { AliasAst, AndAst, Ast, Binding, BindingAst, BlockAst, BoolAst, BoolType, BreakAst, CallAst, Capability, CastAst, CompiledFunction, compilerAssert, ConstructorAst, ContinueInterAst, DefaultConsAst, EnumVariantAst, ExternalDefinition, ExternalFunction, FieldAst, FunctionParameter, GeneratorAst, GlobalCompilerState, IfAst, InterleaveAst, IntType, LetAst, LetType, MutSigilAst, NeverType, NotAst, NumberAst, OperatorAst, OrAst, ParameterizedType, PrimitiveType, RawPointerType, ReturnAst, SetAst, SetFieldAst, SetSubscriptAst, SetValueFieldAst, SourceLocation, StatementsAst, StringAst, SubscriptAst, Type, UserCallAst, ValueFieldAst, VariantCastAst, VoidAst, VoidType, WhileAst, YieldAst, YieldGenerAst } from "../src/defs";
import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, Pointer, Value, ReturnInstruction, ReturnNode, StoreToAddressInstruction, Variable, VariableDeclarationNode, WhileStatementNode, GetFieldPointerInstruction, AndNode, OrNode, PhiInstruction, CommentInstruction, MoveInstruction, EndAccessInstruction, printIR, MarkInitializedInstruction, PhiSource, DeallocStackInstruction, PointerOffsetInstruction, ProjectBundleInstruction, YieldInstruction, BreakInstruction, GetGlobalAddress, BitCastInstruction, YieldGeneratorInstruction, JumpTableInstruction, ProjectAccessInstruction, PointerToAddressInstruction } from "./defs";

type ExpressionContext = {
  valueCategory: 'rvalue' | 'lvalue';
}

class Scope {
  allocs: [string, Type][] = []
  regionId: RegionId | null = null
  projectInstructions: [ProjectAccessInstruction, SourceLocation][] = []
  toPopRegionStateBecauseOfContinuation = false
  currentBreakExprAccessReg: string | null = null
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

  generatorCodegen: GeneratorCodegen

  regionCodegen: RegionCodegen
  irFunction: IrFunction;
  body: Ast;

  constructor(
    public codegen: CodeGenerator,
    public compiledFunction: CompiledFunction,
    public globalCompiler: GlobalCompilerState
  ) {
    this.generatorCodegen = new GeneratorCodegen(this)
  }

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

  addInstruction(instr: IRInstruction, location: SourceLocation) {
    if (instr instanceof JumpInstruction) return // Skip for now
    if (instr instanceof ConditionalJumpInstruction) return // Skip for now
    if (instr instanceof PhiInstruction) return // Skip for now

    this.currentBlock.instructions.push(instr);

    this.regionCodegen.ensureBlock()
    return this.regionCodegen.insertInstruction(instr, location)
  }

  // TODO: Clean this up because it's not clear exactly why it's needed
  // Is it only primitive types? If it's copying a value, make it explicit
  toValue(type: Type, value: IRValue, location: SourceLocation, msg: string = ''): Value {
    if (value instanceof Value) { return value; }
    // compilerAssert(type instanceof PrimitiveType, 'Only allowed on primitive types', { type });
    compilerAssert(type !== VoidType, 'Cannot convert to void type', { type, value, msg });
    const reg = this.newRegister();
    const accessReg = this.newRegister();
    compilerAssert(value.address, 'Value must have an address', { type, value });
    this.addInstruction(new CommentInstruction(`Convert to value ${value.address} ${msg}`), location);
    this.addInstruction(new AccessInstruction(accessReg, value.address, [Capability.Let], type), location);;
    this.addInstruction(new LoadFromAddressInstruction(reg, type, accessReg), location);
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

    const fnScope = new Scope("Function scope");
    this.scopes.push(fnScope);


    this.currentFunction = new FunctionBlock(binding.name, binding, params, paramRegs, this.blocks);

    // Make sure to use the actual specified returnType, not the body.type
    if (returnType !== VoidType && returnType !== NeverType && body.type !== NeverType) {
      this.body = new ReturnAst(body.type, SourceLocation.anon, body)
      this.generate(this.body)
      this.popAndFinalizeScopeUntil(fnScope);
    } else {
      this.generate(body);
      this.popAndFinalizeScopeUntil(fnScope);
      this.addInstruction(new ReturnInstruction(VoidType, null), body.location);
    }

    compilerAssert(this.scopes.length === 0, 'Function scope not popped correctly', { fnScope, scopes: this.scopes });

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
    if (ast instanceof GeneratorAst)     { return this.generateGeneratorStatement(ast) }

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
    if (ast instanceof YieldGenerAst)  { return this.generateYieldGenerExpression(ast, context) }

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
    this.addInstruction(new CommentInstruction('Unused block'), SourceLocation.anon);
    this.unusedBlocks.add(afterLabel);
  }

  generateAlloc(type: Type, location: SourceLocation) {
    compilerAssert(type !== VoidType, 'Cannot allocate void type');
    compilerAssert(type !== NeverType, 'Cannot allocate never type');
    compilerAssert(!type.typeInfo.isReferenceType, "Not implemented reference type", { type })
    const reg = this.newRegister()
    this.functionInstructions.push(new AllocInstruction(reg, type))
    this.scopes[this.scopes.length - 1].allocs.push([reg, type])

    this.regionCodegen.insertBlockInstruction(this.regionCodegen.allocBlock!, new AllocInstruction(reg, type), location);
    return reg
  }

  popAndFinalizeScopeUntil(givenScope: Scope) {
    while (this.scopes.length > 0) {
      const scope = this.scopes[this.scopes.length - 1]
      this.finalizeScope()
      if (scope.toPopRegionStateBecauseOfContinuation) {
        this.regionCodegen.popRegionState()
      }
      this.scopes.pop()
      if (scope === givenScope) return
    }
    compilerAssert(false, 'Scope not found')
  }

  finalizeScope() {
    compilerAssert(this.scopes.length > 0, 'No scopes to close');
    const scope = this.scopes[this.scopes.length - 1]
    const allocs = [...scope.allocs].reverse()
    this.addInstruction(new CommentInstruction(`Finalize scope for ${scope.debugName} ${scope.breakBlockLabel}`), SourceLocation.anon)
    for (const alloc of allocs) {
      this.addInstruction(new DeallocStackInstruction(alloc[0], alloc[1]), SourceLocation.anon)
    }
  }

  generateBlockStatement(ast: BlockAst) {

    const scopeRegionId = this.regionCodegen.insertNewScopeRegion()
    const scopeRegion = this.regionCodegen.getScopeRegion(scopeRegionId)
    this.regionCodegen.insertChildSequenceAndPushState(scopeRegionId)

    this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.bodySequence)

    const blockRegion = this.regionCodegen.insertNewBlockRegion();
    this.regionCodegen.insertChildSequence(blockRegion)
    this.regionCodegen.blockRegion = blockRegion

    this.addInstruction(new CommentInstruction(`Block ${ast.binding.name}`), ast.location)

    const label = this.newLabel()
    const scope = new Scope(`Block stmt ${ast.binding.name}`, label)
    scope.regionId = scopeRegionId
    this.scopes.push(scope);
    this.blockScopeDepth.set(ast.binding, this.scopes.length - 1)
    this.generate(ast.body)
    this.popAndFinalizeScopeUntil(scope);

    this.regionCodegen.popRegionState()

    this.newBlock(label)
  }

  generateBlockExpression(ast: BlockAst, context: ExpressionContext): IRValue {
    
    const label = this.newLabel()
    const scope = new Scope("Block expr", label)
    scope.toPopRegionStateBecauseOfContinuation = true

    scope.breakExprReg = this.generateAlloc(RawPointerType, ast.location)
    const addrReg = this.newRegister()
    scope.currentBreakExprAccessReg = addrReg
  
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

    this.addInstruction(new CommentInstruction(`Block ${ast.binding.name}`), ast.location)

    const value = this.generateExpression(ast.body, context);

    if (ast.body.type !== NeverType) {
      this.generateBlockBreakValueAndProject(scope, value, ast.body)
    }

    this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.exitSequence)

    // TODO: This scope gets finalized twice
    this.finalizeScope()

    this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.continuationSequence)

    const loaded = this.toValue(RawPointerType, new Pointer(scope.breakExprReg), ast.location, 'load block expr ptr ptr')
    this.addInstruction(new PointerToAddressInstruction(addrReg, ast.type, loaded.register), ast.location)

    scope.projectInstructions.forEach(([instr, location]) => {
      this.addInstruction(instr, ast.location)
    })
    
    return new Pointer(scope.currentBreakExprAccessReg)
  }

  generateBlockBreakValueAndProject(scope: Scope, value: IRValue, ast: Ast) {
    
    const resultReg = scope.currentBreakExprAccessReg
    compilerAssert(resultReg, 'Block expression must have a break expression', { ast, scope })
    compilerAssert(scope.breakExprReg, 'Break expression access register not found', { scope })

    const valueLocation = value instanceof Value ? this.irFunction.locations[value.register] : ast.location

    // Here we support Let and Sink capabiltiies because we allo the user to
    // move the value out. If that happens then it has consequences and we can't
    // allow any more access to the value. This is handled in exclusivity.ts
    // with a special case for Sink capability ProjectAccessInstruction
    const capabilities: Capability[] = [Capability.Let, Capability.Sink]

    const valuePtr = this.storeResult(ast.type, value, ast.location)
    const accessReg = this.newRegister()
    this.addInstruction(new AccessInstruction(accessReg, valuePtr.address, capabilities, ast.type), valueLocation)
    this.generateCopyPrimitiveToAddressInstruction(scope.breakExprReg, new Value(accessReg), RawPointerType, valueLocation)

    const newResultReg = this.newRegister()
    const projectInstr = new ProjectAccessInstruction(newResultReg, ast.type, capabilities, valuePtr.address, resultReg)
    scope.projectInstructions.push([projectInstr, valueLocation])
    scope.currentBreakExprAccessReg = newResultReg
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
    this.addInstruction(new CommentInstruction(`Break ${ast.binding.name} ${scope.regionId}`), ast.location)
    
    ;(() => {
      if (!ast.expr) return
      const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' })
      if (ast.expr.type === VoidType || ast.expr.type === NeverType) return
      if (!scope.breakExprReg) return // Block was not an expression

      this.generateBlockBreakValueAndProject(scope, value, ast.expr)
    })()  
    
    this.addInstruction(new BreakInstruction(scope.regionId, VoidType, null), ast.location)
    this._createUnusedBlock()
    return new Pointer('')
  }

  generateReturnStatement(ast: ReturnAst) {
    
    if (!ast.expr || ast.expr.type === NeverType) {
      this.finalizeScope()
      this.addInstruction(new ReturnInstruction(VoidType, null), ast.location);
    } else {
      compilerAssert(ast.type !== VoidType, 'Return type must not be void', { ast })
      const returnReg = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
      if (ast.expr.type instanceof PrimitiveType) {
        const value = this.toValue(ast.type, returnReg, ast.location, 'return')
        this.finalizeScope()
        this.addInstruction(new ReturnInstruction(ast.expr.type, value.register), ast.location);
      } else {
        // Perform a move to the return variable
        compilerAssert(returnReg instanceof Pointer, 'Return value must be a pointer', { ast })
        const returnParameter = this.irFunction.returnParameter;
        compilerAssert(returnParameter, 'Return parameter not found', { ast })
        const variable = this.variableMap.get(returnParameter.binding)
        compilerAssert(variable, 'Return variable not found', { ast })
        this.generateMovePointerInstruction(variable.register, returnReg, ast.expr.type, ast.location)
        this.finalizeScope()
        this.addInstruction(new ReturnInstruction(VoidType, null), ast.location);
      }
    }
    this._createUnusedBlock();
  }

  generateCastExpression(ast: CastAst, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const reg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Cast ${ast.expr.type.shortName} to ${ast.type.shortName}`), ast.location)
    if (ast.type === RawPointerType) {
      // Ugly weird stuff. Come back to this later
      if (!(ast.expr instanceof BindingAst)) {
        const v = this.toValue(ast.expr.type, value, ast.location, 'cast')
        return new Value(v.register)
      } 
      compilerAssert(value instanceof Pointer, 'Expected pointer')
      // TODO: Temporary hack
      this.addInstruction(new MarkInitializedInstruction(value.address, ast.type, true), ast.location);
      return new Value(value.address)
    }
    const v = this.toValue(ast.expr.type, value, ast.location, 'cast')
    this.addInstruction(new BinaryOperationInstruction(reg, ast.type, 'cast', v.register, '', ast.expr.type), ast.location);
    return new Value(reg);
  }

  generateVariantCastExpression(ast: VariantCastAst, context: ExpressionContext): IRValue {
    const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const reg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Variant cast ${ast.expr.type.shortName} to ${ast.type.shortName}`), ast.location)
    const v = this.storeResult(ast.expr.type, value, ast.location)
    this.addInstruction(new BitCastInstruction(reg, ast.type, v.address, ast.expr.type), ast.location);
    return new Pointer(reg);
  }

  generateYieldExpression(ast: YieldAst, context: ExpressionContext): IRValue {
    const expr = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const pointer = this.storeResult(ast.expr.type, expr, ast.location);
    const reg = this.newRegister();
    compilerAssert(ast.expr.type !== VoidType, 'Cannot yield void type', { ast });
    this.addInstruction(new YieldInstruction(reg, ast.expr.type, pointer.address), ast.location);
    return new Value(reg);
  }

  generateVariableDeclaration(ast: LetAst) {
    if (ast.letType === LetType.Alias) return this.generateAliasDeclaration(new AliasAst(ast.type, ast.location, ast.binding, ast.value!))

    this.addInstruction(new CommentInstruction(`let ${ast.letType} ${ast.binding.name}`), ast.location)
    const value = (() => {
      if (!ast.value) return null
      const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
      return this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' }) 
    })()

    if (ast.letType === LetType.VarRef || ast.letType === LetType.Let) {
      return this.generateProjection(ast, value);
    }

    this.addInstruction(new CommentInstruction(`let ${ast.letType} ${ast.binding.name} line=${ast.location.line}`), ast.location)
    this.generateMutableVariableDeclaration(ast, value);
  }

  generateAliasDeclaration(ast: AliasAst) {
    // TODO: Choose either AliasAst or LetType.Alias but not both
    // TODO: Is Alias stupid anyway? Maybe just Let/Var

    compilerAssert(ast.type !== NeverType, 'Cannot alias never type', { ast });
    // Like let but with lower capabilities
    this.addInstruction(new CommentInstruction(`Alias ${ast.binding.name}`), ast.location)
    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const value = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' }) 
    const type = ast.value.type;
    const ptr = this.storeResult(type, value, ast.location);
    const reg = this.newRegister();
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, Capability.Sink, true));
    this.addInstruction(new AccessInstruction(reg, ptr.address, [Capability.Let, Capability.Inout, Capability.Set, Capability.Sink], type), ast.location);
  }

  generateProjection(ast: LetAst, value: IRValue | null) {
    const type = ast.binding.type
    const reg = this.newRegister();
    const letType = ast.letType
    compilerAssert(letType, 'Let type not found');
    const capability = letType === LetType.VarRef ? Capability.Inout : Capability.Let
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, capability));
    compilerAssert(value, 'Let binding must have an initializer');
    const ptr = this.storeResult(type, value, ast.location);
    // compilerAssert(value instanceof Pointer, 'Let binding must have an lvalue initializer');
    this.addInstruction(new CommentInstruction(`Projection ${ast.binding.name}`), ast.location)
    this.addInstruction(new AccessInstruction(reg, ptr.address, [capability], type), ast.location);
  }

  generateMutableVariableDeclaration(ast: LetAst, value: IRValue | null) {
    const type = ast.binding.type
    const reg = this.generateAlloc(type, ast.location);
    
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, Capability.Sink));
    
    if (!value) return
    this.generateMoveToAddressInstruction(reg, value, type, ast.location)
  }


  generateIfExpression(ast: IfAst, context: ExpressionContext): IRValue {
    const isExpr = ast.type !== VoidType && ast.type !== NeverType
    
    if (!isExpr) {
      this.generateIfStatement(ast)
      return new Pointer('')
    }

    // Rewrite it to a a block that returns the result

    const binding = new Binding('if_result', ast.type)
    const breakExprBinding = new Binding('if_break', RawPointerType)

    const trueBody = ast.trueBody instanceof BlockAst ? ast.trueBody.body : ast.trueBody
    const falseBody = !ast.falseBody ? null : ast.falseBody instanceof BlockAst ? ast.falseBody.body : ast.falseBody
    
    const newAst = new BlockAst(ast.type, ast.location, binding, breakExprBinding, 
      new IfAst(NeverType, ast.location, ast.expr, 
        new BreakAst(NeverType, ast.location, binding, trueBody),
        falseBody ? new BreakAst(NeverType, ast.location, binding, falseBody) : null)
    )
    return this.generateBlockExpression(newAst, { valueCategory: 'rvalue' })
  }

  generateIfStatement(ast: IfAst) {

    const thenLabel = this.newLabel();
    const elseLabel = this.newLabel();
    const afterLabel = this.newLabel()

    const ifRegionId = this.regionCodegen.insertNewIfRegion()
    this.regionCodegen.insertChildSequenceAndPushState(ifRegionId)

    const ifRegion = this.regionCodegen.getIfRegion(ifRegionId);
    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.conditionSequence)
    
    const conditionValue = this.generateExpression(ast.expr, { valueCategory: 'rvalue' });
    const conditionReg = this.toValue(ast.expr.type, conditionValue, ast.location, 'if cond')
    ifRegion.conditionRegister = conditionReg.register

    this.addInstruction(new ConditionalJumpInstruction(conditionReg.register, thenLabel, elseLabel), ast.location);

    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.thenSequence)

    this.newBlock(thenLabel);

    this.generate(ast.trueBody);
    this.addInstruction(new JumpInstruction(afterLabel), ast.location);

    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.elseSequence)

    this.newBlock(elseLabel);
    if (ast.falseBody) this.generate(ast.falseBody);
    this.addInstruction(new JumpInstruction(afterLabel), ast.location);
    this.newBlock(afterLabel);

    this.regionCodegen.popRegionState()

  }

  generateWhileStatement(ast: WhileAst) {
    const conditionLabel = this.newLabel();
    const bodyLabel = this.newLabel();
    const afterLabel = this.newLabel();

    const whileRegionId = this.regionCodegen.insertNewWhileRegion();
    this.regionCodegen.insertChildSequenceAndPushState(whileRegionId)

    // Jump to condition check
    this.addInstruction(new JumpInstruction(conditionLabel), ast.location);

    this.regionCodegen.enterRegionSequence(whileRegionId, this.regionCodegen.getWhileRegion(whileRegionId).conditionSequence)

    const conditionBlock = new BasicBlock(conditionLabel, []);
    this.blocks.push(conditionBlock);
    this.currentBlock = conditionBlock;
    const conditionReg = this.generateExpression(ast.condition, { valueCategory: 'rvalue' });
    const conditionValue = this.toValue(ast.condition.type, conditionReg, ast.location, 'while cond')
    // compilerAssert(conditionReg instanceof Value, 'While condition must be an RValue');
    this.addInstruction(new ConditionalJumpInstruction(conditionValue.register, bodyLabel, afterLabel), ast.location);
    this.regionCodegen.getWhileRegion(whileRegionId).conditionRegister = conditionValue.register

    this.regionCodegen.enterRegionSequence(whileRegionId, this.regionCodegen.getWhileRegion(whileRegionId).bodySequence)

    this.newBlock(bodyLabel);
    this.generate(ast.body);

    this.regionCodegen.popRegionState()

    this.addInstruction(new JumpInstruction(conditionLabel), ast.location);
    this.newBlock(afterLabel);

    this.addInstruction(new CommentInstruction('End of while loop'), ast.location)
  }

  generateGeneratorStatement(ast: GeneratorAst) {
    this.generatorCodegen.generateGeneratorStatement(ast)
  }

  generateYieldGenerExpression(ast: YieldGenerAst, context: ExpressionContext): IRValue {
    return this.generatorCodegen.generateYieldGenerExpression(ast, context)
  }

  generateAndExpression(ast: AndAst, context: ExpressionContext): IRValue {
    // Before we had a cleaner way to do this, but for now we can just use an if expression
    compilerAssert(ast.type === BoolType, 'And expression must be a boolean', { ast });
    const location = ast.location
    const if_ = new IfAst(BoolType, location, ast.args[0], ast.args[1], new BoolAst(BoolType, location, false));
    const copy = new CallAst(BoolType, location, externalBuiltinBindings.copy, [if_], [])
    return this.generateExpression(copy, context)
  }

  generateOrExpression(ast: OrAst, context: ExpressionContext): IRValue {
    // Before we had a cleaner way to do this, but for now we can just use an if expression
    compilerAssert(ast.type === BoolType, 'Or expression must be a boolean', { ast });
    const location = ast.location
    const if_ = new IfAst(BoolType, location, ast.args[0], new BoolAst(BoolType, location, true), ast.args[1]);
    const copy = new CallAst(BoolType, location, externalBuiltinBindings.copy, [if_], [])
    return this.generateExpression(copy, context)
  }

  generateCreateStructExpression(ast: ConstructorAst, context: ExpressionContext): IRValue {
    // compilerAssert(context.valueCategory === 'rvalue', 'Struct creation must be an RValue');
    const structType = ast.type
    compilerAssert(structType, `Struct type not found`);
    compilerAssert(!(structType instanceof PrimitiveType), 'Cannot create a struct from a primitive type', { structType, currentStatement: this.currentStatement });
    compilerAssert(ast.args.length === structType.typeInfo.fields.length, 'Field count mismatch', { ast, currentStatement: this.currentStatement });
    const fnBinding = structType.typeInfo.metaobject.constructorBinding
    compilerAssert(fnBinding && fnBinding instanceof Binding, `Constructor not found for ${structType.shortName}`);

    const structReg = this.generateAlloc(structType, ast.location)
    
    const fn = this.codegen.functions.get(fnBinding)
    compilerAssert(fn, `Function ${fnBinding.name} not found`);

    const fields = structType.typeInfo.fields
    const argRegs: string[] = [];

    const structAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(structAccessReg, structReg, [Capability.Set], structType), ast.location);
    argRegs.push(structAccessReg)

    for (let i = 0; i < fields.length; i++) {
      const param = fn.parameters[i + 1]
      const reg = this.generateFunctionArgument(ast.args[i], param.reference, param.passingType, Capability.Sink)
      argRegs.push(reg)
    }
    this.addInstruction(new CallInstruction(null, VoidType, fnBinding, argRegs, fn.parameters.map(b => b.type), fn.parameters.map(b => b.capability)), ast.location)
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

  storeResult(type: Type, value: IRValue, location: SourceLocation) {
    if (value instanceof Pointer) return value
    if (type === VoidType) return new Pointer('')
    const reg = this.generateAlloc(type, location);
    compilerAssert(type instanceof PrimitiveType, 'storeResult not implemented for non-primitive types', { type, value, currentStatement: this.currentStatement });
    this.generateCopyPrimitiveToAddressInstruction(reg, value, type, location)
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
      this.addInstruction(new AccessInstruction(newReg, argReg.address, [capability], passingType), ast.location);
      return newReg
    }

    // Primitive types are allow to be passed without a
    // mutation sigil and they will be copied instead.
    if (!hasMutSigil) capability = Capability.Let

    const argReg = this.generateExpression(ast, { valueCategory: 'rvalue' });
    
    // if (ast.type instanceof PrimitiveType) {
      const value = this.toValue(ast.type, argReg, ast.location, 'function arg')
      this.addInstruction(new AccessInstruction(newReg, value.register, [capability], passingType), ast.location);
    // } else {
      // compilerAssert(argReg instanceof Pointer, "Expected pointer")
      // this.addInstruction(new AccessInstruction(newReg, argReg.address, [Capability.Let]));
    // }

    if (hasMutSigil) {
      compilerAssert(capability !== Capability.Let, 'Unexpected mutation sigil', { ast, stmt: this.currentStatement, location: ast.location.source ? ast.location : this.currentLocation });
      // compilerAssert(argReg instanceof Pointer, 'Function argument must be an pointer', { ast, capability, passingType });
      const ptr = this.storeResult(ast.type, argReg, ast.location)
      this.addInstruction(new MarkInitializedInstruction(ptr.address, ast.type, false), ast.location);
    }

    return newReg

  }

  generateCopyCall(ast: Ast, context: ExpressionContext, location: SourceLocation): IRValue {
    const binding = new Binding('copy', ast.type)
    
    this.generate(new LetAst(VoidType, SourceLocation.anon, binding, null, LetType.Var))
    const bindingAst = new BindingAst(binding.type, SourceLocation.anon, binding)
    const copyConstructor = ast.type.typeInfo.metaobject.copyConstructorBinding
    compilerAssert(copyConstructor && copyConstructor instanceof Binding, `Copy constructor not found for ${ast.type.shortName}`);
    const dest = new MutSigilAst(bindingAst.type, SourceLocation.anon, bindingAst);
    this.generateCallExpression(new CallAst(ast.type, SourceLocation.anon, copyConstructor, [dest, ast], []), context)
    return this.generateBinding(new BindingAst(binding.type, SourceLocation.anon, binding), context)
  }

  generatePrimitiveCopy(ast: Ast, location: SourceLocation) {
    const targetAccessReg = this.newRegister();
    const type = ast.type;
    compilerAssert(type !== VoidType, 'Cannot copy void type');
    const source = this.generateExpression(ast, { valueCategory: 'rvalue' });
    let valueReg = source instanceof Value ? source.register : this.newRegister();
    if (source instanceof Pointer) {
      const sourceAccessReg = this.newRegister();
      // Load from pointer, using a let capability because it is a primitive type and a copy is safe
      this.addInstruction(new AccessInstruction(sourceAccessReg, source.address, [Capability.Let], type), location);
      this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg), location);
    }
    const targetPointer = this.generateAlloc(type, location);
    this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type), location);
    this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg), location);
    this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]), location);
    return new Pointer(targetPointer);
  }

  generatePrint(ast: Ast, context: ExpressionContext, location: SourceLocation): IRValue {
    const value = this.generateExpression(ast, { valueCategory: 'rvalue' });
    const valueReg = this.toValue(ast.type, value, ast.location, 'print')
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.print, [valueReg.register], [ast.type], [Capability.Let]), location);
    return new Pointer('')
  }

  generatePrintf(args: Ast[], context: ExpressionContext, location: SourceLocation): IRValue {
    const format = this.generateExpression(args[0], { valueCategory: 'rvalue' });
    const formatReg = this.toValue(args[0].type, format, location, 'printf')
    const argRegs: string[] = [];
    for (let i = 1; i < args.length; i++) {
      const arg = this.generateExpression(args[i], { valueCategory: 'rvalue' });
      const argReg = this.toValue(args[i].type, arg, location, 'printf')
      argRegs.push(argReg.register)
    }
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.printf, [formatReg.register, ...argRegs], args.map(a => a.type), args.map(a => Capability.Let)), location);
    return new Pointer('')
  }

  generateExit(args: Ast[], context: ExpressionContext, location: SourceLocation): IRValue {
    const arg = this.generateExpression(args[0], { valueCategory: 'rvalue' });
    const argReg = this.toValue(args[0].type, arg, location, 'exit')
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.exit, [argReg.register], [args[0].type], [Capability.Let]), location);
    return new Pointer('')
  }

  generateExternalCall(ast: CallAst, fn: ExternalDefinition) {
    // TODO: Merge this with call expression and make it cleaner

    compilerAssert(ast.args.length === fn.paramTypes.length, 'Argument count mismatch', { binding: ast.binding, got: ast.args.length, expected: fn.paramTypes.length });
    
    const argRegs: string[] = [];
    const capabilities = fn.paramTypes.map(p => Capability.Let)
    const argTypes = fn.paramTypes

    let i = 0
    for (const givenArg of ast.args) {
      const argIndex = i++;
      const paramType = fn.paramTypes[argIndex]
      const capability = Capability.Let
      // compilerAssert(param.capability, `Capability not found for ${param.binding.name}`);
      const argReg = this.generateFunctionArgument(givenArg, false, paramType, capability);
      argRegs.push(argReg);
    }

    // Call the function
    const returnType = fn.returnType
    if (returnType === VoidType) {
      this.addInstruction(new CallInstruction(null, returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
      return new Pointer('') // hack. Make sure this is not used
    }
    const accessReg = this.newRegister()
    const resultReg = this.generateAlloc(fn.returnType, ast.location);
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set], fn.returnType), ast.location);
    this.addInstruction(new CallInstruction(accessReg, fn.returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
    return new Pointer(resultReg)

  }

  generateCallExpression(ast: CallAst, context: ExpressionContext): IRValue {
    if (ast.binding === externalBuiltinBindings.copy) {
      if (ast.args[0].type instanceof PrimitiveType) return this.generatePrimitiveCopy(ast.args[0], ast.location);
      return this.generateCopyCall(ast.args[0], context, ast.location)
    } else if (ast.binding === externalBuiltinBindings.print) {
      return this.generatePrint(ast.args[0], context, ast.location)
    } else if (ast.binding === externalBuiltinBindings.printf) {
      return this.generatePrintf(ast.args, context, ast.location)
    } else if (ast.binding === externalBuiltinBindings.exit) {
      return this.generateExit(ast.args, context, ast.location)
    } else if (ast.binding === externalBuiltinBindings.initializer) {
      this.addInstruction(new CallInstruction(null, VoidType, ast.binding, [], [], []), ast.location)
      return new Pointer('')
    }
    compilerAssert(ast.binding instanceof Binding, 'Expected binding', { ast });


    // Generate code for arguments
    const fn = this.codegen.functions.get(ast.binding)
    if (!fn) {
      const ex = this.globalCompiler.externalDefinitions.find(x => x.binding === ast.binding)
      if (ex) return this.generateExternalCall(ast, ex)
    }
    compilerAssert(fn, `Function ${ast.binding.name} not found`, {
      c: this.codegen.functions
    });
    this.addInstruction(new CommentInstruction(`Call ${ast.binding.name}`), ast.location)
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
      resultReg = this.generateAlloc(fn.returnType, ast.location);
      argRegs.unshift(resultReg)
      capabilities.unshift(Capability.Set)
      argTypes.unshift(RawPointerType)
    }

    // Call the function
    const returnType = fnIr?.returnType || fn.returnType
    if (returnType === VoidType) {
      this.addInstruction(new CallInstruction(null, returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
      if (fnIr?.returnParameter) return new Pointer(resultReg!)
      return new Pointer('') // hack. Make sure this is not used
    }
    const accessReg = this.newRegister()
    resultReg = this.generateAlloc(fn.returnType, ast.location);
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set], fn.returnType), ast.location);
    this.addInstruction(new CallInstruction(accessReg, fn.returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
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
      const lvalue = this.storeResult(global.type, value, ast.location)
      const targetReg = this.newRegister();
      this.irFunction.globalRegisters.push({
        register: global.register,
        type: global.type,
      })
      this.addInstruction(new GetGlobalAddress(targetReg, global.type, global.register), ast.location);
      this.generateMovePointerInstructionWithCapabilityCheck(targetReg, lvalue, ast.value)
      return
    }
    compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
    const type = variable.type

    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const value = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
    const lvalue = this.storeResult(type, value, ast.location)

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
    const rightReg = this.storeResult(fieldType, newValue, ast.location)
    this.addInstruction(new GetFieldPointerInstruction(reg, objReg.address, ast.field), ast.location);
    this.generateMovePointerInstructionWithCapabilityCheck(reg, rightReg, ast.value)
  }

  generateAssignmentValueField(ast: SetValueFieldAst) {
    this.ensureMutable(ast.left)
    this.addInstruction(new CommentInstruction(`Set value field ${ast.fieldPath.map(x => x.name).join(", ")}`), ast.location)
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    let destReg: string = objReg.address
    ast.fieldPath.forEach(field => {
      const source = destReg
      destReg = this.newRegister();
      this.addInstruction(new GetFieldPointerInstruction(destReg, source, field), ast.location);
    })
    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const newValue = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
    const rightReg = this.storeResult(ast.value.type, newValue, ast.location)
    this.generateMovePointerInstructionWithCapabilityCheck(destReg, rightReg, ast.value)
  }

  getCapabilityAndOwnership(value: Ast): [Capability, boolean] {
    // Try to get the capability and whether the value needs
    // to be owned, by visiting the AST a bit.
    // Not sure about this.
    if (value instanceof BindingAst) {
      const variable = this.variableMap.get(value.binding)
      if (!variable && this.globalCompiler.globalVars.has(value.binding)) {
        return [Capability.Set, true]
      }
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
    if (value instanceof BoolAst)        return [Capability.Let,  false]
    if (value instanceof StringAst)      return [Capability.Sink, true]
    if (value instanceof ConstructorAst) return [Capability.Sink, true]
    if (value instanceof EnumVariantAst) return [Capability.Sink, true]
    if (value instanceof DefaultConsAst) return [Capability.Sink, true]
    if (value instanceof SubscriptAst)   return [Capability.Sink, false]
    if (value instanceof IfAst)          return [Capability.Sink, true]
    if (value instanceof AndAst)         return [Capability.Sink, true]
    if (value instanceof OrAst)          return [Capability.Sink, true]
    if (value instanceof BlockAst)       return this.getCapabilityAndOwnership(value.body)
    if (value instanceof StatementsAst)  return this.getCapabilityAndOwnership(value.statements[value.statements.length - 1])
    if (value instanceof CastAst)        return this.getCapabilityAndOwnership(value.expr)
    compilerAssert(false, 'Not implemented', { value, currentStatement: this.currentStatement })
  }

  generateMovePointerInstructionWithCapabilityCheck(targetPointer: string, sourcePointer: IRValue, valueAst: Ast) {
    const location = valueAst.location.source ? valueAst.location : this.currentLocation

    if (sourcePointer instanceof Value) {
      this.generateCopyPrimitiveToAddressInstruction(targetPointer, sourcePointer, valueAst.type, location)
      return
    }

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


    if (type instanceof PrimitiveType) {
      compilerAssert(type !== VoidType, 'Cannot move void type');
      const targetAccessReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      const valueReg = this.newRegister();
      this.addInstruction(new AccessInstruction(sourceAccessReg, sourcePointer.address, [sourceCapability], type), location);
      this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg), location);
      this.addInstruction(new CommentInstruction("Maybe mark"), location)
      if (sourceCapability === Capability.Sink)
        this.addInstruction(new MarkInitializedInstruction(sourceAccessReg, type, false), location);
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type), location);
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg), location);
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]), location);

    } else {
      compilerAssert(sourceCapability !== Capability.Let, 'Cannot move a let value', { sourceCapability, location: valueAst.location.source ? valueAst.location : this.currentLocation, stmt: this.currentStatement, valueAst })
      this.addInstruction(new MoveInstruction(targetPointer, sourcePointer.address, type), location);
    }
  }

  generateMovePointerInstruction(targetPointer: string, sourcePointer: Pointer, type: Type, location: SourceLocation) {
    compilerAssert(targetPointer, 'Target pointer must be defined', { targetPointer, sourcePointer, type })
    compilerAssert(sourcePointer.address, 'Source pointer must be defined', { targetPointer, sourcePointer, type })
    if (type instanceof PrimitiveType) {
      compilerAssert(type !== VoidType, 'Cannot move void type');
      const targetAccessReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      const valueReg = this.newRegister();
      this.addInstruction(new AccessInstruction(sourceAccessReg, sourcePointer.address, [Capability.Sink, Capability.Let], type), location);
      this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg), location);
      this.addInstruction(new CommentInstruction("Mark"), location)
      this.addInstruction(new MarkInitializedInstruction(sourceAccessReg, type, false), location);
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type), location);
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg), location);
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]), location);
    } else {
      this.addInstruction(new MoveInstruction(targetPointer, sourcePointer.address, type), location);
    }
  }

  generateCopyPrimitiveToAddressInstruction(destReg: string, value: Value, type: Type, location: SourceLocation) {
    compilerAssert(type instanceof PrimitiveType, 'Unexpected non-primitive types', { type, currentStatement: this.currentStatement });
    const destAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(destAccessReg, destReg, [Capability.Set], type), location);
    this.addInstruction(new StoreToAddressInstruction(destAccessReg, type, value.register), location);
    this.addInstruction(new EndAccessInstruction(destAccessReg, [Capability.Set]), location);
  }

  generateMoveToAddressInstruction(destReg: string, value: IRValue, type: Type, location: SourceLocation) {
    if (value instanceof Value) {
      this.generateCopyPrimitiveToAddressInstruction(destReg, value, type, location)
    } else {
      this.generateMovePointerInstruction(destReg, value, type, location)
    }
  }

  generateBinaryExpression(ast: OperatorAst, context: ExpressionContext): IRValue {
    compilerAssert(ast.type instanceof PrimitiveType, "Expected primitive type", { ast })
    const leftReg = this.toValue(ast.args[0].type, this.generateExpression(ast.args[0], context), ast.location)
    const rightReg = this.toValue(ast.args[1].type, this.generateExpression(ast.args[1], context), ast.location)

    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, ast.operator, leftReg.register, rightReg.register, ast.args[0].type), ast.location);
    if (context.valueCategory === 'lvalue') {
      return this.storeResult(ast.type, new Value(resultReg), ast.location)
    }
    return new Value(resultReg)
  }

  generateNotExpression(ast: NotAst, context: ExpressionContext): IRValue {
    const value = this.toValue(ast.expr.type, this.generateExpression(ast.expr, context), ast.location)
    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, '!', value.register, '', ast.expr.type), ast.location);
    return new Value(resultReg);
  }

  generateBinding(ast: BindingAst, context: ExpressionContext): IRValue {
    const addressReg = this.variableMap.get(ast.binding);
    if (!addressReg) {
      if (this.globalCompiler.globalVars.has(ast.binding)) {
        const global = this.globalCompiler.globalVars.get(ast.binding)
        compilerAssert(global, `Undefined variable: ${ast.binding.name}`);
        this.irFunction.globalRegisters.push({
          register: global.register,
          type: global.type,
        })
        const reg = this.newRegister();
        this.addInstruction(new GetGlobalAddress(reg, global.type, global.register), ast.location);
        return new Pointer(reg)
      }
    }
    compilerAssert(addressReg, `Undefined variable: ${ast.binding.name}`);
    return new Pointer(addressReg.register);
  }

  generateNumberLiteral(ast: NumberAst, context: ExpressionContext): IRValue {
    const value = ast.value;
    const destReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(destReg, ast.type, value), ast.location);
    return new Value(destReg);
  }
  
  generateBoolLiteral(ast: BoolAst, context: ExpressionContext): IRValue {
    const value = ast.value;
    const destReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(destReg, ast.type, value ? 1 : 0), ast.location);
    return new Value(destReg);
  }

  generateStringLiteral(ast: StringAst, context: ExpressionContext): IRValue {
    // compilerAssert(context.valueCategory === 'rvalue', 'Literal must be an RValue');
    if (ast.type === RawPointerType) {
      const resultPtr = this.newRegister();
      this.addInstruction(new LoadConstantInstruction(resultPtr, ast.type, ast.value), ast.location);
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
    this.addInstruction(new CommentInstruction(`Get field ${ast.field.name}`), ast.location)
    this.addInstruction(new GetFieldPointerInstruction(destReg, objReg.address, ast.field), ast.location);
    return new Pointer(destReg);
  }

  generateValueFieldExpression(ast: ValueFieldAst, context: ExpressionContext): IRValue {
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' });
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
    
    this.addInstruction(new CommentInstruction(`Get field ${ast.fieldPath.map(x => x.name).join(", ")}`), ast.location)
    let destReg: string = objReg.address
    ast.fieldPath.forEach(field => {
      const source = destReg
      destReg = this.newRegister();
      this.addInstruction(new GetFieldPointerInstruction(destReg, source, field), ast.location);
    })
    return new Pointer(destReg);
  }

  generateSubscriptExpression(ast: SubscriptAst, context: ExpressionContext): IRValue {

    // This is a low level operation, probably should just have a different AST node for it
    if (ast.left.type === RawPointerType) {
      const destReg = this.newRegister();
    
      const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' })
      const objRegValue = this.toValue(ast.left.type, objReg, ast.location, 'subscript object')
      // compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');
      
      const offset = this.toValue(ast.right.type,
        this.generateExpression(ast.right, { valueCategory: 'rvalue' }), ast.location)
      this.addInstruction(new PointerOffsetInstruction(destReg, objRegValue.register, ast.type, offset.register), ast.location);
      return new Pointer(destReg);
    }
    const destReg = this.newRegister();
    
    const objReg = this.generateExpression(ast.left, { valueCategory: 'lvalue' })
    compilerAssert(objReg instanceof Pointer, 'Object must be an pointer');

    const offset = this.toValue(ast.right.type,
      this.generateExpression(ast.right, { valueCategory: 'rvalue' }), ast.location)
  
    compilerAssert(ast.funcs, 'Subscript must have funcs', { ast })
    const caps = [Capability.Let, Capability.Inout, Capability.Set, Capability.Sink]
    this.addInstruction(new ProjectBundleInstruction(destReg, ast.type, caps, objReg.address, [offset.register], ast.funcs as any), ast.location);
    return new Pointer(destReg);

  }

  generateAssignmentSubscript(ast: SetSubscriptAst) {
    compilerAssert(ast.left.type === RawPointerType, 'Subscript assignment only supported for raw pointers');
    this.ensureMutable(ast.left)
    const objReg = this.toValue(ast.left.type,
      this.generateExpression(ast.left, { valueCategory: 'lvalue' }), ast.location,
      'subscript object')
    
    const elementType = ast.value.type
    const reg = this.newRegister();

    const offset = this.toValue(ast.right.type, this.generateExpression(ast.right, { valueCategory: 'rvalue' }), ast.location, 'subscript offset')
    const astWithoutMutSigil = ast.value instanceof MutSigilAst ? ast.value.expr : ast.value
    const newValue = this.generateExpression(astWithoutMutSigil, { valueCategory: 'rvalue' });
    const valueReg = this.storeResult(elementType, newValue, ast.location)
    this.addInstruction(new PointerOffsetInstruction(reg, objReg.register, elementType, offset.register), ast.location);

    this.generateMovePointerInstructionWithCapabilityCheck(reg, valueReg, ast.value)
  }


  ///////////////////////////

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



type Generator = {
  location: SourceLocation
  regionId: RegionId,
  ast: GeneratorAst,
  entryBranch: GeneratorBranch,
  elseBranch: GeneratorBranch,
  currentBranch: GeneratorBranch
}

class GeneratorBranch {
  regionIds: RegionId[] = []
  jumpInstrs: InstructionId[] = []
  constructor(
    public sequenceId: SequenceId,
    public writeStateAddress: string,
    public readStateAddress: string,
    public writeAddress: string | null,
    public readAddress: string | null) {}
}

class GeneratorCodegen {

  // Split out the generator codegen into a separate class
  // because it has its own state and is a bit more complex

  generators: Generator[] = []

  constructor(public fnCodegen: FunctionCodeGenerator) {}

  insertNewJumpPoint(generator: Generator) {
    const fnCodegen = this.fnCodegen
    const { regionCodegen } = fnCodegen
    const currentBranch = generator.currentBranch
    const isEntry = currentBranch === generator.entryBranch

    // Currently just insert an empty scope region for the yield
    // to jump to, which will be emitted as LLVM labels later.
    // I also tried to wrap the rest of the instructions inside the
    // scope region, but that only works at the top level, not inside
    // if statements or loops.

    const scopeRegionId = regionCodegen.insertNewScopeRegion()
    currentBranch.regionIds.push(scopeRegionId)
    regionCodegen.insertChildSequence(scopeRegionId)
    regionCodegen.blockRegion = null
    fnCodegen.addInstruction(new CommentInstruction(`Yield segment ${isEntry ? 'entry' : 'else'}`), generator.location)
  }

  generateGeneratorStatement(ast: GeneratorAst) {
    const fnCodegen = this.fnCodegen
    const { regionCodegen } = fnCodegen
    const entryStateAddress = fnCodegen.generateAlloc(IntType, ast.location)
    const entryReadAddress = ast.elseValueType !== VoidType ? fnCodegen.generateAlloc(RawPointerType, ast.location) : null
    const elseStateAddress = fnCodegen.generateAlloc(IntType, ast.location)
    const elseReadAddress = ast.entryValueType !== VoidType ? fnCodegen.generateAlloc(RawPointerType, ast.location) : null

    const constant0 = fnCodegen.generateNumberLiteral(new NumberAst(IntType, SourceLocation.anon, 0), { valueCategory: 'rvalue' })
    compilerAssert(constant0 instanceof Value, 'Expected value', { constant0 })
    fnCodegen.generateCopyPrimitiveToAddressInstruction(entryStateAddress, constant0, IntType, ast.location)
    fnCodegen.generateCopyPrimitiveToAddressInstruction(elseStateAddress, constant0, IntType, ast.location)
    
    const interleaveRegionId = regionCodegen.insertNewGeneratorRegion();
    regionCodegen.insertChildSequenceAndPushState(interleaveRegionId)
    const region = regionCodegen.getGeneratorRegion(interleaveRegionId);

    const entryBranch = new GeneratorBranch(region.entrySequence, elseStateAddress, entryStateAddress, elseReadAddress, entryReadAddress)
    const elseBranch = new GeneratorBranch(region.elseSequence, entryStateAddress, elseStateAddress, entryReadAddress, elseReadAddress)
    const generator: Generator = { location: ast.location, regionId: interleaveRegionId, ast, entryBranch, elseBranch, currentBranch: entryBranch }
    this.generators.push(generator)

    regionCodegen.enterRegionSequence(interleaveRegionId, region.entrySequence)
    this.insertNewJumpPoint(generator)
    fnCodegen.generate(ast.entryBlock)
    
    generator.currentBranch = elseBranch

    regionCodegen.enterRegionSequence(interleaveRegionId, region.elseSequence)
    this.insertNewJumpPoint(generator)
    fnCodegen.generate(ast.elseBlock)

    region.entryRegionIds = entryBranch.regionIds
    region.elseRegionIds = elseBranch.regionIds

    entryBranch.jumpInstrs.forEach(instrId => {
      const instr = regionCodegen.getInstructionById(instrId)
      compilerAssert(instr instanceof JumpTableInstruction, 'Expected jump table instruction', { instr })
      instr.table.push(...elseBranch.regionIds)
    })
    elseBranch.jumpInstrs.forEach(instrId => {
      const instr = regionCodegen.getInstructionById(instrId)
      compilerAssert(instr instanceof JumpTableInstruction, 'Expected jump table instruction', { instr })
      // TODO: Skip the first one because it cannot be entered, however this will mess with the order of the region ids
      // Maybe LLVM will optimize this out anyway
      instr.table.push(...entryBranch.regionIds) 
    })

    this.generators.pop()
    regionCodegen.popRegionState()
  }

  generateYieldGenerExpression(ast: YieldGenerAst, context: ExpressionContext): IRValue {
    const fnCodegen = this.fnCodegen
    const { regionCodegen } = fnCodegen

    const generator = this.generators.find(x => x.ast.binding === ast.generatorBinding)
    compilerAssert(generator, 'Generator not found', { ast })
    const currentBranch = generator.currentBranch
    
    if (currentBranch.writeAddress) {
      compilerAssert(ast.expr, 'Expected expression', { ast })
      const valueAddress = fnCodegen.storeResult(ast.expr.type, fnCodegen.generateExpression(ast.expr, { valueCategory: 'lvalue' }), ast.location)
      fnCodegen.addInstruction(new CommentInstruction(`Write pointer ${valueAddress?.address} to ${currentBranch.writeAddress}`), ast.location)
      fnCodegen.generateCopyPrimitiveToAddressInstruction(currentBranch.writeAddress, new Value(valueAddress.address), RawPointerType, ast.location)
    }

    const nextJumpRegion = currentBranch.regionIds.length
    fnCodegen.addInstruction(new CommentInstruction(`Set next jump = ${nextJumpRegion}`), ast.location)
    const constant = fnCodegen.generateNumberLiteral(new NumberAst(IntType, SourceLocation.anon, nextJumpRegion), { valueCategory: 'rvalue' })
    compilerAssert(constant instanceof Value, 'Expected value', { constant })
    fnCodegen.generateCopyPrimitiveToAddressInstruction(currentBranch.writeStateAddress, constant, IntType, ast.location)

    const readStateValue = fnCodegen.toValue(IntType, new Pointer(currentBranch.readStateAddress), ast.location, 'yield')
    const jumpInstr = fnCodegen.addInstruction(new JumpTableInstruction(IntType, readStateValue.register, []), ast.location)!
    currentBranch.jumpInstrs.push(jumpInstr)
    
    this.insertNewJumpPoint(generator)

    if (currentBranch.readAddress) {
      fnCodegen.addInstruction(new MarkInitializedInstruction(currentBranch.readAddress, RawPointerType, true), ast.location)
      const value = fnCodegen.toValue(RawPointerType, new Pointer(currentBranch.readAddress), ast.location, 'yield')
      return new Pointer(value.register)
    }

    // TODO: Maybe need to hold access here
    return new Pointer('')
  }
  
}