import { InstructionId, IrFunction, printIrFunction, RegionCodegen, RegionId, SequenceId } from "../region/region_codegen";
import { externalBuiltinBindings } from "../src/compiler_sugar";
import { AliasAst, AndAst, Ast, Binding, BindingAst, BlockAst, BoolAst, BoolType, BreakAst, CallAst, Capability, CastAst, CompiledFunction, compilerAssert, CompilerError, ConstructorAst, ContinueInterAst, createStatements, DefaultConsAst, DiagnosticLocation, EnumVariantAst, ExternalDefinition, ExternalFunction, FieldAst, FunctionParameter, GeneratorAst, GlobalCompilerState, IfAst, InterleaveAst, IntType, LetAst, LetType, MutSigilAst, NeverType, NotAst, NumberAst, OperatorAst, OrAst, ParameterizedType, PrimitiveType, RawPointerType, ReturnAst, SetAst, SetFieldAst, SetSubscriptAst, SetValueFieldAst, SourceLocation, StatementsAst, StringAst, SubscriptAst, Type, UserCallAst, ValueFieldAst, VariantCastAst, VoidAst, VoidType, WhileAst, YieldAst, YieldGenerAst } from "../src/defs";
import { ASTNode, AllocInstruction, AssignInstruction, AssignmentNode, BasicBlock, BinaryExpressionNode, BinaryOperationInstruction, BlockStatementNode, CallExpressionNode, CallInstruction, AccessInstruction, ConditionalJumpInstruction, CreateStructNode, ExpressionNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, IRInstruction, IRValue, IdentifierNode, IfStatementNode, JumpInstruction, LetConstNode, LiteralNode, LoadConstantInstruction, LoadFromAddressInstruction, MemberExpressionNode, ProgramNode, Pointer, Value, ReturnInstruction, ReturnNode, StoreToAddressInstruction, Variable, VariableDeclarationNode, WhileStatementNode, GetFieldPointerInstruction, AndNode, OrNode, PhiInstruction, CommentInstruction, MoveInstruction, EndAccessInstruction, printIR, MarkInitializedInstruction, PhiSource, DeallocStackInstruction, PointerOffsetInstruction, ProjectBundleInstruction, YieldInstruction, BreakInstruction, GetGlobalAddress, BitCastInstruction, YieldGeneratorInstruction, JumpTableInstruction, ProjectAccessInstruction, PointerToAddressInstruction } from "./defs";


class ValueRegister {
  public outRegister: string | null = null
  constructor() {}
}
class PointerRegister {
  public outAddressRegister: string | null = null
  constructor() {}
}
class ConstructRegister {
  constructor(public address: string) {}
}
type IrTarget = ValueRegister | PointerRegister | ConstructRegister

type ExpressionContext = {
  target: IrTarget;
  expectMutable?: ExpectMutable
}

class ExpectMutable {
  constructor(public definitionLocation: SourceLocation) {}
}

class Scope {
  allocs: [string, Type][] = []
  regionId: RegionId | null = null
  projectInstructions: [ProjectAccessInstruction, SourceLocation][] = []
  toPopRegionStateBecauseOfContinuation = false
  currentBreakExprAccessReg: string | null = null
  target: IrTarget | null = null
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

  toValueRegister(register: string, type: Type, addressReg: string, location: SourceLocation, msg: string = '') {
    compilerAssert(type !== VoidType, 'Cannot convert to void type', { type, addressReg, msg });
    const reg = register
    const accessReg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Convert to value ${addressReg} ${msg}`), location);
    this.addInstruction(new AccessInstruction(accessReg, addressReg, [Capability.Let], type), location);;
    this.addInstruction(new LoadFromAddressInstruction(reg, type, accessReg), location);
  }

  // Entry point
  generateFunction(binding: Binding, params: FunctionParameter[], returnType: Type, body: Ast) {
    compilerAssert(!this.currentFunction, 'Already generating in a function');
    console.log("Begin generating function", binding.name);

    const paramRegs = params.map((param) => {
      const paramReg = this.newRegister()
      const variable = new Variable(param.binding.name, param.type, paramReg, param.capability, SourceLocation.anon);
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
      const variable = new Variable('return', RawPointerType, returnReg, Capability.Set, SourceLocation.anon);
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

      const needsReturn = (returnType === VoidType || returnType === NeverType)
      if (needsReturn)
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

    const _unusedReg = this.generateExpressionToLoadRegister(ast, {  })
    
  }

  generateExpressionToLoadRegister(ast: Ast, context: { expectMutable?: ExpectMutable }): string {
    compilerAssert(ast.type instanceof PrimitiveType, 'Expression must be a primitive type', { ast });
    const target = new ValueRegister()
    this.generateExpression(ast, { target, expectMutable: context.expectMutable })
    return target.outRegister!
  }

  generateExpressionToLValueRegister(ast: Ast, context: { expectMutable?: ExpectMutable }): string {
    const target = new PointerRegister()
    this.generateExpression(ast, { target, expectMutable: context.expectMutable })
    return target.outAddressRegister!
  }
  

  extractMutableSigil(ast: Ast): Ast {
    while (ast instanceof MutSigilAst) { ast = ast.expr }
    return ast
  }

  generateExpression(ast: Ast, context: ExpressionContext): void {
    if (ast instanceof MutSigilAst)    { return this.generateMutSigilExpression(ast, context) }
    if (ast instanceof ConstructorAst) { return this.generateCreateStructExpression(ast, context) }
    if (ast instanceof BindingAst)     { return this.generateBinding(ast, context) }
    if (ast instanceof BlockAst)       { return this.generateBlockExpression(ast, context) }
    if (ast instanceof IfAst)          { return this.generateIfExpression(ast, context) }
    if (ast instanceof StatementsAst)  { return this.generateStatementsExpression(ast, context) }
    if (ast instanceof UserCallAst)    { return this.generateUserCallExpression(ast, context) }
    if (ast instanceof CallAst)        { return this.generateCallExpression(ast, context) }
    if (ast instanceof NumberAst)      { return this.generateNumberLiteral(ast, context) }
    if (ast instanceof OperatorAst)    { return this.generateOperatorExpression(ast, context) }
    if (ast instanceof ValueFieldAst)  { return this.generateValueFieldExpression(ast, context) }
    if (ast instanceof FieldAst)       { return this.generateFieldExpression(ast, context) }
    if (ast instanceof StringAst)      { return this.generateStringLiteral(ast, context) }
    if (ast instanceof SubscriptAst)   { return this.generateSubscriptExpression(ast, context) }
    if (ast instanceof AndAst)         { return this.generateAndExpression(ast, context) }
    if (ast instanceof OrAst)          { return this.generateOrExpression(ast, context) }
    if (ast instanceof BoolAst)        { return this.generateBoolLiteral(ast, context) }
    if (ast instanceof BreakAst)       { return this.generateBreakExpression(ast, context) }
    if (ast instanceof NotAst)         { return this.generateNotExpression(ast, context) }
    if (ast instanceof DefaultConsAst) { return this.generateDefaultConstructorExpression(ast, context) }
    if (ast instanceof CastAst)        { return this.generateCastExpression(ast, context) }
    if (ast instanceof YieldAst)       { return this.generateYieldExpression(ast, context) }
    if (ast instanceof EnumVariantAst) { return this.generateEnumVariantExpression(ast, context) }
    if (ast instanceof VariantCastAst) { return this.generateVariantCastExpression(ast, context) }
    if (ast instanceof YieldGenerAst)  { return this.generateYieldGenerExpression(ast, context) }

    compilerAssert(false, 'Not implemented expression', { ast, fnBody: this.body })
  }

  generateStatementsExpression(ast: StatementsAst, context: ExpressionContext) {
    for (const stmt of ast.statements.slice(0, -1)) {
      this.generate(stmt);
    }
    if (ast.type !== VoidType && ast.type !== NeverType) {
      return this.generateExpression(ast.statements[ast.statements.length - 1], context);
    } else {
      this.generate(ast.statements[ast.statements.length - 1])
      return
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

  generateAllocToRegister(register: string, type: Type, location: SourceLocation) {
    compilerAssert(type !== VoidType, 'Cannot allocate void type');
    compilerAssert(type !== NeverType, 'Cannot allocate never type');
    compilerAssert(!type.typeInfo.isReferenceType, "Not implemented reference type", { type })
    this.functionInstructions.push(new AllocInstruction(register, type))
    this.scopes[this.scopes.length - 1].allocs.push([register, type])

    this.regionCodegen.insertBlockInstruction(this.regionCodegen.allocBlock!, new AllocInstruction(register, type), location);
    return register
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

  generateMutSigilExpression(ast: MutSigilAst, context: ExpressionContext) {
    let allow = !!context.expectMutable

    // We have a special case for mutable sigils where we don't actually know if the
    // value will be sinked or not, and we need to get the compiler to allow it.
    // This happens in the case of array literals which get transformed into a block
    // expression that moves or projects the array value out. This is the best way I 
    // can think of dealing with this for now, but open to revisit it later.
    allow = allow || ast.onlyIfNeccessary

    if (allow) return this.generateExpression(this.extractMutableSigil(ast.expr), { target: context.target })
    compilerAssert(false, 'Unexpected mutable sigil (&)', { ast, context, location: ast.location })
  }

  generateBlockStatement(ast: BlockAst) {

    // this.generateBlockExpression(ast, { target: new PointerRegister() })

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
    const addrReg = this.newRegister()
    scope.currentBreakExprAccessReg = addrReg
    scope.breakExprReg = this.generateAlloc(RawPointerType, ast.location)
    this.scopes.push(scope);
    this.blockScopeDepth.set(ast.binding, this.scopes.length - 1)
    this.generate(ast.body)

    this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.exitSequence)
    this.popAndFinalizeScopeUntil(scope);

    this.regionCodegen.popRegionState()

    this.newBlock(label)
  }

  generateBlockExpression(ast: BlockAst, context: ExpressionContext): void {
    
    const label = this.newLabel()
    const scope = new Scope("Block expr", label)
    scope.toPopRegionStateBecauseOfContinuation = true
    scope.target = context.target ?? null
    // scope.targetType = context.target!

    scope.breakExprReg = context.target instanceof ConstructRegister ? null : this.generateAlloc(RawPointerType, ast.location)
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

    if (ast.body.type !== NeverType) {

      if (scope.target instanceof ConstructRegister) {
        const reg = this.generateAlloc(ast.type, ast.location)
        this.generateExpression(ast.body, { target: new ConstructRegister(reg) })
        this.generateMovePointerInstruction(scope.target.address, reg, ast.type, ast.location)
      } else {
        this.generateBlockBreakValueAndProject(scope, ast.body)
      }

    } else {
      this.addInstruction(new CommentInstruction(`Block expr ${ast.binding.name} ${scope.currentBreakExprAccessReg}`), ast.location)
      const _unusedReg = this.generateExpressionToLoadRegister(ast.body, { })
    }
    
    this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.exitSequence)

    if (context.target instanceof ValueRegister) {
      const outReg = this.newRegister()
      this.addInstruction(new CommentInstruction(`Block expr ${ast.binding.name} ${scope.currentBreakExprAccessReg} reg = ${outReg}`), ast.location)
      const ptrReg = this.newRegister()
      this.addInstruction(new LoadFromAddressInstruction(ptrReg, RawPointerType, scope.breakExprReg!), ast.location)
      this.addInstruction(new LoadFromAddressInstruction(outReg, ast.type, ptrReg), ast.location)
      context.target.outRegister = outReg

      this.popAndFinalizeScopeUntil(scope)

    } else if (context.target instanceof ConstructRegister) {
      // Values should already have been sinked by now - just cleanup
      this.popAndFinalizeScopeUntil(scope)
    } else if (context.target instanceof PointerRegister) {

      // TODO: This scope gets finalized twice
      this.finalizeScope()

      this.regionCodegen.enterRegionSequence(scopeRegionId, scopeRegion.continuationSequence)

      context.target.outAddressRegister = this.newRegister()
      this.addInstruction(new CommentInstruction(`Block expr ${ast.binding.name} ${scope.currentBreakExprAccessReg} addr = ${context.target.outAddressRegister}`), ast.location)
      this.addInstruction(new LoadFromAddressInstruction(context.target.outAddressRegister, RawPointerType, scope.breakExprReg!), ast.location)

    } else {
      compilerAssert(false, 'Block expression must have a target', { ast, context })
    }

  }

  generateBlockBreakValueAndProject(scope: Scope, ast: Ast) {

    compilerAssert(!(scope.target instanceof ConstructRegister), 'Block expression must have a target', { ast, scope })

    const resultReg = scope.currentBreakExprAccessReg
    compilerAssert(resultReg !== undefined, 'Block expression must have a break expression', { ast, scope })

    const reg = this.generateExpressionToLValueRegister(ast, { })

    compilerAssert(scope.breakExprReg, 'Break expression access register not found', { scope })

    // Here we support Let and Sink capabiltiies because we allow the user to
    // move the value out. If that happens then it has consequences and we can't
    // allow any more access to the value. This is handled in exclusivity.ts
    // with a special case for Sink capability ProjectAccessInstruction
    const capabilities: Capability[] = [Capability.Let, Capability.Sink]

    const valueLocation = ast.location
    const accessReg = this.newRegister()
    this.addInstruction(new AccessInstruction(accessReg, reg, capabilities, ast.type), valueLocation)
    this.generateCopyPrimitiveToAddressInstruction(scope.breakExprReg, accessReg, RawPointerType, valueLocation)
  }

  generateBreakExpression(ast: BreakAst, context: ExpressionContext) {
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
      // const value = this.generateExpression(ast.expr, { valueCategory: 'rvalue' })
      if (ast.expr.type === VoidType || ast.expr.type === NeverType) return
      // if (!scope.breakExprReg) return // Block was not an expression // Not used with new implementation
      if (scope.target instanceof ConstructRegister) {
        this.generateExpression(ast.expr, { target: new ConstructRegister(scope.target.address) })
        return
      }

      this.generateBlockBreakValueAndProject(scope, ast.expr)
    })() 
    
    this.addInstruction(new BreakInstruction(scope.regionId, VoidType, null), ast.location)
    this._createUnusedBlock()
  }

  generateReturnStatement(ast: ReturnAst) {
    
    if (!ast.expr || ast.expr.type === NeverType) {
      this.finalizeScope()
      this.addInstruction(new ReturnInstruction(VoidType, null), ast.location);
      this._createUnusedBlock();
      return
    }

    compilerAssert(ast.type !== VoidType, 'Return type must not be void', { ast })
    
    if (ast.expr.type instanceof PrimitiveType) {
      const returnReg = this.generateExpressionToLoadRegister(ast.expr, { });
      // const value = this.toValue(ast.type, returnReg, ast.location, 'return')
      this.finalizeScope()
      this.addInstruction(new ReturnInstruction(ast.expr.type, returnReg), ast.location);
      this._createUnusedBlock();
      return
    }

    // Perform a move to the return variable
    // compilerAssert(returnReg instanceof Pointer, 'Return value must be a pointer', { ast })
    const returnParameter = this.irFunction.returnParameter;
    compilerAssert(returnParameter, 'Return parameter not found', { ast })
    const variable = this.variableMap.get(returnParameter.binding)
    compilerAssert(variable, 'Return variable not found', { ast })
    const returnReg = this.generateExpressionToLValueRegister(ast.expr, { });
    this.generateMovePointerInstruction(variable.register, returnReg, ast.expr.type, ast.location)
    this.finalizeScope()
    this.addInstruction(new ReturnInstruction(VoidType, null), ast.location);
    
    this._createUnusedBlock();
  }

  generateCastExpression(ast: CastAst, context: ExpressionContext) {

    this.addInstruction(new CommentInstruction(`Cast ${ast.expr.type.shortName} to ${ast.type.shortName}`), ast.location)
    if (ast.type === RawPointerType) {
      // compilerAssert(false, 'Not implemented cast expression', { ast, context })

      if (ast.expr instanceof BindingAst) {
        const addressReg = this.variableMap.get(ast.expr.binding);
        compilerAssert(addressReg, 'Variable not found', { ast, context })
        const valueReg = this.newRegister();
        this.addInstruction(new AssignInstruction(valueReg, RawPointerType, addressReg.register), ast.location)
        this.addInstruction(new MarkInitializedInstruction(valueReg, ast.type, true), ast.location)
        this.generateStorePrimitiveIfNeccessary(context, valueReg, ast.type, ast.location)
      } else {
        compilerAssert(false, 'Not implemented cast expression', { ast, context })
      }
      return
    }
    const valueReg = this.generateExpressionToLoadRegister(ast.expr, { })
    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, 'cast', valueReg, '', ast.expr.type), ast.location);
    this.generateStorePrimitiveIfNeccessary(context, resultReg, ast.type, ast.location)
  }

  generateVariantCastExpression(ast: VariantCastAst, context: ExpressionContext) {
    const reg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Variant cast ${ast.expr.type.shortName} to ${ast.type.shortName}`), ast.location)
    const valueReg = this.generateExpressionToLValueRegister(ast.expr, { })
    this.addInstruction(new BitCastInstruction(reg, ast.type, valueReg, ast.expr.type), ast.location);
    this.generateMoveOrLoadPointerUsingContext(context, reg, ast.type, ast.location)
  }

  generateYieldExpression(ast: YieldAst, context: ExpressionContext) {
    compilerAssert(ast.expr.type !== VoidType, 'Cannot yield void type', { ast });

    const resultReg = this.newRegister()
    const valueReg = this.generateExpressionToLValueRegister(ast.expr, { })
    this.addInstruction(new CommentInstruction(`Yield ${ast.expr.type.shortName}`), ast.location)
    this.addInstruction(new YieldInstruction(resultReg, RawPointerType, valueReg), ast.location);
    this.generateStorePrimitiveIfNeccessary(context, resultReg, ast.expr.type, ast.location)
  }

  generateVariableDeclaration(ast: LetAst) {
    if (ast.letType === LetType.Alias) return this.generateAliasDeclaration(new AliasAst(ast.type, ast.location, ast.binding, ast.value!))

    this.addInstruction(new CommentInstruction(`let ${ast.letType} ${ast.binding.name}`), ast.location)
    if (ast.letType === LetType.VarRef || ast.letType === LetType.Let) {
      return this.generateProjection(ast);
    }

    this.addInstruction(new CommentInstruction(`let ${ast.letType} ${ast.binding.name} line=${ast.location.line}`), ast.location)
    this.generateMutableVariableDeclaration(ast);
  }

  generateAliasDeclaration(ast: AliasAst): void {
    // TODO: Choose either AliasAst or LetType.Alias but not both
    // TODO: Is Alias stupid anyway? Maybe just Let/Var
    // compilerAssert(false, 'Not implemented alias declaration', { ast })

    compilerAssert(ast.type !== NeverType, 'Cannot alias never type', { ast });
    // Like let but with lower capabilities
    this.addInstruction(new CommentInstruction(`Alias ${ast.binding.name}`), ast.location)
    const astWithoutMutSigil = this.extractMutableSigil(ast.value)
    const value = this.generateExpressionToLValueRegister(astWithoutMutSigil, { }) 
    const type = ast.value.type;
    const reg = this.newRegister()
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, Capability.Sink, ast.location, true));
    this.addInstruction(new AccessInstruction(reg, value, [Capability.Let, Capability.Inout, Capability.Set, Capability.Sink], type), ast.location);
  }

  generateProjection(ast: LetAst) {

    const type = ast.binding.type
    const letType = ast.letType
    compilerAssert(letType, 'Let type not found');
    const capability = letType === LetType.VarRef ? Capability.Inout : Capability.Let
    const reg = this.newRegister();

    compilerAssert(ast.value, 'Value not found', { ast });

    const expectMutable = ast.letType === LetType.VarRef ? new ExpectMutable(ast.location) : undefined
    const storageReg = this.generateExpressionToLValueRegister(ast.value, { expectMutable }) 
    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, reg, capability, ast.location));
    
    this.addInstruction(new CommentInstruction(`Projection ${ast.binding.name}`), ast.location)
    this.addInstruction(new AccessInstruction(reg, storageReg, [capability], type), ast.location);
  }

  generateMutableVariableDeclaration(ast: LetAst) {
    const type = ast.binding.type
    const storageReg = this.generateAlloc(type, ast.location)

    this.variableMap.set(ast.binding, new Variable(ast.binding.name, type, storageReg, Capability.Sink, ast.location));

    if (!ast.value) return null

    this.generateExpression(ast.value, { target: new ConstructRegister(storageReg), expectMutable: new ExpectMutable(ast.location) })
  }


  generateIfExpression(ast: IfAst, context: ExpressionContext): void {
    const isExpr = ast.type !== VoidType && ast.type !== NeverType
    
    if (!isExpr) {
      this.generateIfStatement(ast)
      return
    }

    const trueBody = ast.trueBody instanceof BlockAst ? ast.trueBody.body : ast.trueBody
    const falseBody = !ast.falseBody ? null : ast.falseBody instanceof BlockAst ? ast.falseBody.body : ast.falseBody
    const binding = new Binding('if_result', ast.type)
    const breakExprBinding = new Binding('if_break_expr', RawPointerType)
    const block = (stmt: Ast) => new BlockAst(VoidType, ast.location, new Binding('', VoidType), null, stmt)

    const newAst = new BlockAst(ast.type, ast.location, binding, breakExprBinding, 
      new IfAst(NeverType, ast.location, ast.expr, 
        block(new BreakAst(NeverType, ast.location, binding, trueBody)),
        falseBody ? block(new BreakAst(NeverType, ast.location, binding, falseBody)) : null)
    )
    this.generateBlockExpression(newAst, context)
  }

  generateIfStatement(ast: IfAst) {
    const target = this.generateExpressionToLoadRegister(ast.expr, { });

    const ifRegionId = this.regionCodegen.insertNewIfRegion()
    this.regionCodegen.insertChildSequenceAndPushState(ifRegionId)

    const ifRegion = this.regionCodegen.getIfRegion(ifRegionId);
    ifRegion.conditionRegister = target

    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.thenSequence)
    this.generate(ast.trueBody);
    this.regionCodegen.enterRegionSequence(ifRegionId, ifRegion.elseSequence)
    if (ast.falseBody) this.generate(ast.falseBody);
    this.regionCodegen.popRegionState()
  }

  generateWhileStatement(ast: WhileAst) {
    const whileRegionId = this.regionCodegen.insertNewWhileRegion();
    this.regionCodegen.insertChildSequenceAndPushState(whileRegionId)

    this.regionCodegen.enterRegionSequence(whileRegionId, this.regionCodegen.getWhileRegion(whileRegionId).conditionSequence)

    const conditionReg = this.generateExpressionToLoadRegister(ast.condition, { });
    this.regionCodegen.getWhileRegion(whileRegionId).conditionRegister = conditionReg

    this.regionCodegen.enterRegionSequence(whileRegionId, this.regionCodegen.getWhileRegion(whileRegionId).bodySequence)
    this.generate(ast.body);
    this.regionCodegen.popRegionState()
    this.addInstruction(new CommentInstruction('End of while loop'), ast.location)
  }

  generateGeneratorStatement(ast: GeneratorAst) {
    this.generatorCodegen.generateGeneratorStatement(ast)
  }

  generateYieldGenerExpression(ast: YieldGenerAst, context: ExpressionContext) {
    return this.generatorCodegen.generateYieldGenerExpression(ast, context)
  }

  generateAndExpression(ast: AndAst, context: ExpressionContext) {
    // Before we had a cleaner way to do this, but for now we can just use an if expression
    compilerAssert(ast.type === BoolType, 'And expression must be a boolean', { ast });
    const location = ast.location
    const if_ = new IfAst(BoolType, location, ast.args[0], ast.args[1], new BoolAst(BoolType, location, false));
    const copy = new CallAst(BoolType, location, externalBuiltinBindings.copy, [if_], [])
    return this.generateExpression(copy, context)
  }

  generateOrExpression(ast: OrAst, context: ExpressionContext) {
    // Before we had a cleaner way to do this, but for now we can just use an if expression
    compilerAssert(ast.type === BoolType, 'Or expression must be a boolean', { ast });
    const location = ast.location
    const if_ = new IfAst(BoolType, location, ast.args[0], new BoolAst(BoolType, location, true), ast.args[1]);
    const copy = new CallAst(BoolType, location, externalBuiltinBindings.copy, [if_], [])
    return this.generateExpression(copy, context)
  }

  generateCreateStructExpression(ast: ConstructorAst, context: ExpressionContext) {
    const structType = ast.type
    compilerAssert(structType, `Struct type not found`);
    compilerAssert(!(structType instanceof PrimitiveType), 'Cannot create a struct from a primitive type', { structType, currentStatement: this.currentStatement });
    compilerAssert(ast.args.length === structType.typeInfo.fields.length, 'Field count mismatch', { ast, currentStatement: this.currentStatement, got: ast.args.length, expected: structType.typeInfo.fields.length });
    const fnBinding = structType.typeInfo.metaobject.constructorBinding
    compilerAssert(fnBinding && fnBinding instanceof Binding, `Constructor not found for ${structType.shortName}`);

    const storageReg = this.createAllocTargetFromContext(context, structType, ast.location)
    
    const fn = this.codegen.functions.get(fnBinding)
    compilerAssert(fn, `Function ${fnBinding.name} not found`);

    const fields = structType.typeInfo.fields
    const argRegs: string[] = [];

    const structAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(structAccessReg, storageReg, [Capability.Set], structType), ast.location);
    argRegs.push(structAccessReg)

    for (let i = 0; i < fields.length; i++) {
      const param = fn.parameters[i + 1]
      const reg = this.generateFunctionArgument(ast.args[i], param.reference, param.passingType, Capability.Sink)
      argRegs.push(reg)
    }
    this.addInstruction(new CallInstruction(null, VoidType, fnBinding, argRegs, fn.parameters.map(b => b.type), fn.parameters.map(b => b.capability)), ast.location)
    this.createLoadIntoRegisterFromContextIfNeccessary(context, structAccessReg, structType, ast.location)
  }

  generateEnumVariantExpression(ast: EnumVariantAst, context: ExpressionContext) {
    this.addInstruction(new CommentInstruction(`Enum variant ${ast.variantType.shortName}`), ast.location)
    const constr = new ConstructorAst(ast.variantType, ast.location, ast.args)
    return this.generateCreateStructExpression(constr, context)
  }

  generateDefaultConstructorExpression(ast: DefaultConsAst, context: ExpressionContext) {
    compilerAssert(false, `Don't use default constructor AST, use createDefaultConstructorAst instead`, { ast })
  }

  generateFunctionArgument(ast: Ast, reference: boolean, passingType: Type, capability: Capability): string {
    // @ParameterPassing
    const newReg = this.newRegister();

    const expectMutable = capability === Capability.Sink || capability === Capability.Set || capability === Capability.Inout ?
      new ExpectMutable(ast.location) : undefined
      
    if (reference) {
      const argReg = this.generateExpressionToLValueRegister(ast, { expectMutable });
      this.addInstruction(new AccessInstruction(newReg, argReg, [capability], passingType), ast.location);
      return newReg
    }

    const argReg = this.generateExpressionToLoadRegister(ast, { expectMutable });
    this.addInstruction(new AccessInstruction(newReg, argReg, [capability], passingType), ast.location);

    return newReg
  }

  generateCopyCall(ast: Ast, context: ExpressionContext, location: SourceLocation) {
    const binding = new Binding('copy', ast.type)
    
    this.generate(new LetAst(VoidType, SourceLocation.anon, binding, null, LetType.Var))
    const bindingAst = new BindingAst(binding.type, SourceLocation.anon, binding)
    const copyConstructor = ast.type.typeInfo.metaobject.copyConstructorBinding
    compilerAssert(copyConstructor && copyConstructor instanceof Binding, `Copy constructor not found for ${ast.type.shortName}`);
    const dest = new MutSigilAst(bindingAst.type, SourceLocation.anon, bindingAst);
    this.generateCallExpression(new CallAst(ast.type, SourceLocation.anon, copyConstructor, [dest, ast], []), { target: context.target })
    const resultAst = new BindingAst(binding.type, SourceLocation.anon, binding);
    this.generateExpression(resultAst, { target: context.target })
  }

  generatePrimitiveCopy(ast: Ast, context: ExpressionContext, location: SourceLocation) {
    if (context.target instanceof ValueRegister) {
      this.generateExpression(ast, { target: context.target });
      return
    }
    
    const type = ast.type
    compilerAssert(type !== VoidType, 'Cannot copy void type')

    const valueReg = this.generateExpressionToLoadRegister(ast, { })
    this.generateStorePrimitiveIfNeccessary(context, valueReg, type, location)
  }

  generatePrintf(args: Ast[], context: ExpressionContext, location: SourceLocation) {
    const formatReg = this.generateExpressionToLoadRegister(args[0], { });
    const argRegs: string[] = [];
    for (let i = 1; i < args.length; i++) {
      const arg = this.generateExpressionToLoadRegister(args[i], { });
      argRegs.push(arg)
    }
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.printf, [formatReg, ...argRegs], args.map(a => a.type), args.map(a => Capability.Let)), location);
  }

  generateExit(args: Ast[], context: ExpressionContext, location: SourceLocation) {
    const argReg = this.generateExpressionToLoadRegister(args[0], { });
    // const argReg = this.toValue(args[0].type, arg, location, 'exit')
    this.addInstruction(new CallInstruction(null, VoidType, externalBuiltinBindings.exit, [argReg], [args[0].type], [Capability.Let]), location);
  }

  generateExternalCall(ast: CallAst, context: ExpressionContext, fn: ExternalDefinition) {
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
      const argReg = this.generateFunctionArgument(givenArg, false, paramType, capability);
      argRegs.push(argReg);
    }

    // Call the function
    const returnType = fn.returnType
    if (returnType === VoidType) {
      this.addInstruction(new CallInstruction(null, returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
      return
    }
    const resultReg = this.createAllocTargetFromContext(context, returnType, ast.location)
    const accessReg = this.newRegister()
    this.addInstruction(new AccessInstruction(accessReg, resultReg, [Capability.Set], fn.returnType), ast.location);
    this.addInstruction(new CallInstruction(accessReg, fn.returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
    this.createLoadIntoRegisterFromContextIfNeccessary(context, accessReg, returnType, ast.location)
  }

  generateCallExpression(ast: CallAst, context: ExpressionContext) {
    if (ast.binding === externalBuiltinBindings.copy) {
      if (ast.args[0].type instanceof PrimitiveType) return this.generatePrimitiveCopy(ast.args[0], context, ast.location);
      return this.generateCopyCall(ast.args[0], context, ast.location)
    } else if (ast.binding === externalBuiltinBindings.print) {
      compilerAssert(false, 'Not implemented print', { ast })
    } else if (ast.binding === externalBuiltinBindings.printf) {
      return this.generatePrintf(ast.args, context, ast.location)
    } else if (ast.binding === externalBuiltinBindings.exit) {
      return this.generateExit(ast.args, context, ast.location)
    } else if (ast.binding === externalBuiltinBindings.initializer) {
      this.addInstruction(new CallInstruction(null, VoidType, ast.binding, [], [], []), ast.location)
      return
    }
    compilerAssert(ast.binding instanceof Binding, 'Expected binding', { ast });


    // Generate code for arguments
    const fn = this.codegen.functions.get(ast.binding)
    if (!fn) {
      const ex = this.globalCompiler.externalDefinitions.find(x => x.binding === ast.binding)
      if (ex) return this.generateExternalCall(ast, context, ex)
    }
    compilerAssert(fn, `Function ${ast.binding.name} not found`, { c: this.codegen.functions });
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

    const returnType = fnIr?.returnType ?? fn.returnType
    // Call the function

    let resultReg: string | null = null
    if (fnIr?.returnParameter) {
      compilerAssert(!(context.target instanceof ValueRegister), 'Unexpected target', { context, ast })
      const originalReturnType = fn.returnType;
      resultReg = this.createAllocTargetFromContext(context, originalReturnType, ast.location)
      const access = this.newRegister()
      this.addInstruction(new AccessInstruction(access, resultReg, [Capability.Set], fn.returnType), ast.location)
      argRegs.unshift(access)
      capabilities.unshift(Capability.Set)
      argTypes.unshift(RawPointerType)
      this.addInstruction(new CallInstruction(null, returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
      return
    }

    if (returnType === VoidType) {
      this.addInstruction(new CallInstruction(null, fn.returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
      return
    }

    const accessReg = this.newRegister()
    const resultReg2 = this.createAllocTargetFromContext(context, returnType, ast.location)
    compilerAssert(resultReg2, 'Unexpected target', { context, ast, returnType })
    this.addInstruction(new AccessInstruction(accessReg, resultReg2, [Capability.Set], fn.returnType), ast.location);
    this.addInstruction(new CallInstruction(accessReg, fn.returnType, ast.binding, argRegs, argTypes, capabilities), ast.location)
    this.createLoadIntoRegisterFromContextIfNeccessary(context, accessReg, returnType, ast.location)

  }

  createAllocTargetFromContext(context: ExpressionContext, type: Type, location: SourceLocation) {
    if (context.target instanceof ConstructRegister) {
      return context.target.address
    } else if (context.target instanceof PointerRegister) {
      context.target.outAddressRegister = this.newRegister()
      this.generateAllocToRegister(context.target.outAddressRegister, type, location)
      return context.target.outAddressRegister
    } else if (context.target instanceof ValueRegister) {
      return this.generateAlloc(type, location)
    }
    compilerAssert(false, 'Unexpected target', { context, type, location })
  }

  createLoadIntoRegisterFromContextIfNeccessary(context: ExpressionContext, sourceReg: string, type: Type, location: SourceLocation) {
    if (context.target instanceof ValueRegister) {
      context.target.outRegister = this.newRegister()
      this.toValueRegister(context.target.outRegister, type, sourceReg, location)
    }
  }

  generateUserCallExpression(ast: UserCallAst, context: ExpressionContext) {
    return this.generateCallExpression(new CallAst(ast.type, SourceLocation.anon, ast.binding, ast.args, []), context)
  }

  generateMovePointerInstruction(targetPointer: string, sourceAddressReg: string, type: Type, location: SourceLocation) {
    compilerAssert(targetPointer, 'Target pointer must be defined', { targetPointer, sourceAddressReg, type })
    compilerAssert(sourceAddressReg, 'Source pointer must be defined', { targetPointer, sourceAddressReg, type })
    if (type instanceof PrimitiveType) {
      compilerAssert(type !== VoidType, 'Cannot move void type');
      const targetAccessReg = this.newRegister();
      const sourceAccessReg = this.newRegister();
      const valueReg = this.newRegister();
      this.addInstruction(new AccessInstruction(sourceAccessReg, sourceAddressReg, [Capability.Sink, Capability.Let], type), location);
      this.addInstruction(new LoadFromAddressInstruction(valueReg, type, sourceAccessReg), location);
      this.addInstruction(new CommentInstruction("Mark"), location)
      this.addInstruction(new MarkInitializedInstruction(sourceAccessReg, type, false), location);
      this.addInstruction(new AccessInstruction(targetAccessReg, targetPointer, [Capability.Set], type), location);
      this.addInstruction(new StoreToAddressInstruction(targetAccessReg, type, valueReg), location);
      this.addInstruction(new EndAccessInstruction(targetAccessReg, [Capability.Set]), location);
    } else {
      this.addInstruction(new MoveInstruction(targetPointer, sourceAddressReg, type), location);
    }
  }

  generateCopyPrimitiveToAddressInstruction(destReg: string, valueReg: string, type: Type, location: SourceLocation) {
    compilerAssert(type instanceof PrimitiveType, 'Unexpected non-primitive types', { type, currentStatement: this.currentStatement });
    const destAccessReg = this.newRegister();
    this.addInstruction(new AccessInstruction(destAccessReg, destReg, [Capability.Set], type), location);
    this.addInstruction(new StoreToAddressInstruction(destAccessReg, type, valueReg), location);
    this.addInstruction(new EndAccessInstruction(destAccessReg, [Capability.Set]), location);
  }

  generateOperatorExpression(ast: OperatorAst, context: ExpressionContext) {
    compilerAssert(ast.type instanceof PrimitiveType, "Expected primitive type", { ast })

    const leftReg = this.generateExpressionToLoadRegister(ast.args[0], { });
    const rightReg = this.generateExpressionToLoadRegister(ast.args[1], { });
    
    const resultReg = this.newRegister();
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, ast.operator, leftReg, rightReg, ast.args[0].type), ast.location);
    this.generateStorePrimitiveIfNeccessary(context, resultReg, ast.type, ast.location)
  }

  generateNotExpression(ast: NotAst, context: ExpressionContext) {
    const valueReg = this.generateExpressionToLoadRegister(ast.expr, { });

    const resultReg = this.newRegister()
    this.addInstruction(new BinaryOperationInstruction(resultReg, ast.type, '!', valueReg, '', ast.expr.type), ast.location);
    this.generateStorePrimitiveIfNeccessary(context, resultReg, ast.type, ast.location)
  }

  /** Generates move, load, copy or alias depending on context requirements */
  generateMoveOrLoadPointerUsingContext(context: ExpressionContext, sourceReg: string, type: Type, location: SourceLocation) {
    if (context.expectMutable) {
      if (!(type instanceof PrimitiveType)) {
        const diagnosticLocations = [new DiagnosticLocation(context.expectMutable.definitionLocation, 'Mutation happens here')]
        compilerAssert(false, 'Expected mutable sigil', { type, context, diagnosticLocations, location })
      }
    }
    const isPrimitiveCopy = context.target instanceof ConstructRegister && 
      context.expectMutable && type instanceof PrimitiveType

    if (isPrimitiveCopy) {
      // Implicit primtive copying - Convert the move to a copy if the conditions are right
      // This relies on the mutation sigil being present at the point of the move which may
      // not be exactly where one would expect it to be
      // E.G valid: var z = ifx true { x& } else { y& }
      // Not valid: var z = ifx true { x } else { y }&
      // This seems like the best option for now, but can revisit later
      const loadedReg = this.newRegister();
      this.toValueRegister(loadedReg, type, sourceReg, location)  
      this.generateCopyPrimitiveToAddressInstruction((context.target as ConstructRegister).address, loadedReg, type, location)
    } else if (context.target instanceof ConstructRegister) {
      this.generateMovePointerInstruction(context.target.address, sourceReg, type, location)
    } else if (context.target instanceof ValueRegister) {
      context.target.outRegister = this.newRegister()
      this.toValueRegister(context.target.outRegister, type, sourceReg, location)
    } else if (context.target instanceof PointerRegister) {
      context.target.outAddressRegister = sourceReg
    }
  }

  generateBinding(ast: BindingAst, context: ExpressionContext): void {

    const addressReg = this.variableMap.get(ast.binding);
    if (!addressReg) {
      if (this.globalCompiler.globalVars.has(ast.binding)) {
        // TODO: Seperate AST for global bindings?
        const global = this.globalCompiler.globalVars.get(ast.binding)
        compilerAssert(global, `Undefined variable: ${ast.binding.name}`);
        this.irFunction.globalRegisters.push({ register: global.register, type: global.type })
        const reg = this.newRegister();
        this.addInstruction(new GetGlobalAddress(reg, global.type, global.register), ast.location);
        this.generateMoveOrLoadPointerUsingContext(context, reg, global.type, ast.location)
        return
      }
    }
    compilerAssert(addressReg, `Undefined variable: ${ast.binding.name}`);
    this.generateMoveOrLoadPointerUsingContext(context, addressReg.register, ast.type, ast.location)
  }

  generateStorePrimitiveIfNeccessary(context: ExpressionContext, valueReg: string, type: Type, location: SourceLocation) {
    if (context.target instanceof ConstructRegister) {
      this.generateCopyPrimitiveToAddressInstruction(context.target.address, valueReg, type, location)
    } else if (context.target instanceof PointerRegister) {
      context.target.outAddressRegister = this.newRegister()
      this.generateAllocToRegister(context.target.outAddressRegister, type, location)
      this.generateCopyPrimitiveToAddressInstruction(context.target.outAddressRegister, valueReg, type, location)
    } else if (context.target instanceof ValueRegister) {
      context.target.outRegister = valueReg
    }
  }

  generateNumberLiteral(ast: NumberAst, context: ExpressionContext) {
    const resultReg = this.newRegister();
    this.addInstruction(new LoadConstantInstruction(resultReg, ast.type, ast.value), ast.location);
    this.generateStorePrimitiveIfNeccessary(context, resultReg, ast.type, ast.location)
  }
  
  generateBoolLiteral(ast: BoolAst, context: ExpressionContext) {
    const value = ast.value ? 1 : 0
    const resultReg = this.newRegister()
    this.addInstruction(new LoadConstantInstruction(resultReg, ast.type, value), ast.location);
    this.generateStorePrimitiveIfNeccessary(context, resultReg, ast.type, ast.location)
  }

  generateStringLiteral(ast: StringAst, context: ExpressionContext) {
    if (ast.type === RawPointerType) {
      const resultReg = this.newRegister()
      this.addInstruction(new LoadConstantInstruction(resultReg, ast.type, ast.value), ast.location);
      this.generateStorePrimitiveIfNeccessary(context, resultReg, ast.type, ast.location)
      return
    }
    
    const args = [
      new NumberAst(IntType, SourceLocation.anon, ast.value.length),
      new StringAst(RawPointerType, SourceLocation.anon, ast.value),
    ]
    this.generateCreateStructExpression(new ConstructorAst(ast.type, SourceLocation.anon, args), context)
  }

  generateFieldExpression(ast: FieldAst, context: ExpressionContext) {
    const reg = this.generateExpressionToLValueRegister(ast.left, { });

    const destReg = this.newRegister();
    this.addInstruction(new CommentInstruction(`Get field ${ast.field.name}`), ast.location)
    this.addInstruction(new GetFieldPointerInstruction(destReg, reg, ast.field), ast.location);
    this.generateMoveOrLoadPointerUsingContext(context, destReg, ast.type, ast.location)
  }

  generateValueFieldExpression(ast: ValueFieldAst, context: ExpressionContext) {
    const objReg = this.generateExpressionToLValueRegister(ast.left, { });

    this.addInstruction(new CommentInstruction(`Get field ${ast.fieldPath.map(x => x.name).join(", ")}`), ast.location)
    let destReg: string = objReg
    ast.fieldPath.forEach(field => {
      const source = destReg
      destReg = this.newRegister();
      this.addInstruction(new GetFieldPointerInstruction(destReg, source, field), ast.location);
    })

    this.generateMoveOrLoadPointerUsingContext(context, destReg, ast.type, ast.location)
  }

  generateSubscriptExpression(ast: SubscriptAst, context: ExpressionContext) {
    // This is a low level operation, probably should just have a different AST node for it
    if (ast.left.type === RawPointerType) {
      const destReg = this.newRegister();
      const objReg = this.generateExpressionToLoadRegister(ast.left, { });
      const offset = this.generateExpressionToLoadRegister(ast.right, { });
      this.addInstruction(new PointerOffsetInstruction(destReg, objReg, ast.type, offset), ast.location);
      this.generateMoveOrLoadPointerUsingContext(context, destReg, ast.type, ast.location)
      return
    }
    const destReg = this.newRegister();
    
    const objReg = this.generateExpressionToLValueRegister(ast.left, { });
    const offset = this.generateExpressionToLoadRegister(ast.right, { });
  
    compilerAssert(ast.funcs, 'Subscript must have funcs', { ast })
    const caps = [Capability.Let, Capability.Inout, Capability.Set, Capability.Sink]
    this.addInstruction(new ProjectBundleInstruction(destReg, ast.type, caps, objReg, [offset], ast.funcs as any), ast.location);
    this.generateMoveOrLoadPointerUsingContext(context, destReg, ast.type, ast.location)
  }


  generateSinkIntoPointer(destPointer: string, ast: Ast, location: SourceLocation) {
    if (ast.type instanceof PrimitiveType) {
      const valueReg = this.generateExpressionToLoadRegister(ast, { expectMutable: new ExpectMutable(location) });
      this.generateCopyPrimitiveToAddressInstruction(destPointer, valueReg, ast.type, location)
    } else {
      const rightReg = this.generateAlloc(ast.type, location)
      this.generateExpression(ast, { target: new ConstructRegister(rightReg), expectMutable: new ExpectMutable(location) })
      this.generateMovePointerInstruction(destPointer, rightReg, ast.type, location)
    }
  }

  generateAssignmentStatement(ast: SetAst) {
    let variable = this.variableMap.get(ast.binding)
    if (!variable) {
      const isInitializer = this.compiledFunction.binding === this.globalCompiler.initializerFunction?.binding
      const global = this.globalCompiler.globalVars.get(ast.binding)
      compilerAssert(global, `Undefined variable: ${ast.binding.name}`);
      compilerAssert(global.letType === LetType.Var || isInitializer, 'Cannot assign to a let variable', { location: ast.location, global, ast })
      const targetReg = this.newRegister();
      this.addInstruction(new GetGlobalAddress(targetReg, global.type, global.register), ast.location);
      const storageReg = this.generateAlloc(global.type, ast.location)
      this.generateExpression(ast.value, { target: new ConstructRegister(storageReg), expectMutable: new ExpectMutable(ast.location) })
      this.generateMovePointerInstruction(targetReg, storageReg, global.type, ast.location)
      this.irFunction.globalRegisters.push({ register: global.register, type: global.type, })
      return
    }
    compilerAssert(variable, `Undefined variable: ${ast.binding.name}`);
    compilerAssert(variable.capability === Capability.Inout || variable.capability === Capability.Set || variable.capability === Capability.Sink, 'Cannot assign to a let variable', { location: ast.location, variable, ast })
    this.generateSinkIntoPointer(variable.register, ast.value, ast.location)
  }

  generateAssignmentField(ast: SetFieldAst) {
    const leftReg = this.generateExpressionToLValueRegister(ast.left, { });
    const reg = this.newRegister()
    this.addInstruction(new GetFieldPointerInstruction(reg, leftReg, ast.field), ast.location);
    this.generateSinkIntoPointer(reg, ast.value, ast.location)
  }

  generateAssignmentValueField(ast: SetValueFieldAst) {
    this.addInstruction(new CommentInstruction(`Set value field ${ast.fieldPath.map(x => x.name).join(", ")}`), ast.location)
    const objReg = this.generateExpressionToLValueRegister(ast.left, { });
    
    let destReg: string = objReg
    ast.fieldPath.forEach(field => {
      const source = destReg
      destReg = this.newRegister();
      this.addInstruction(new GetFieldPointerInstruction(destReg, source, field), ast.location);
    })

    this.generateSinkIntoPointer(destReg, ast.value, ast.location)
  }

  generateAssignmentSubscript(ast: SetSubscriptAst) {
    compilerAssert(ast.left.type === RawPointerType, 'Subscript assignment only supported for raw pointers');
    const objReg = this.generateExpressionToLoadRegister(ast.left, { })
    
    const elementType = ast.value.type
    const offset = this.generateExpressionToLoadRegister(ast.right, { })

    const reg = this.newRegister()
    this.addInstruction(new PointerOffsetInstruction(reg, objReg, elementType, offset), ast.location);
    this.generateSinkIntoPointer(reg, ast.value, ast.location)
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

    const constant0 = fnCodegen.generateExpressionToLoadRegister(new NumberAst(IntType, SourceLocation.anon, 0), { })
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
    fnCodegen.addInstruction(new CommentInstruction(`End generator`), ast.location)
  }

  generateYieldGenerExpression(ast: YieldGenerAst, context: ExpressionContext) {
    const fnCodegen = this.fnCodegen

    const generator = this.generators.find(x => x.ast.binding === ast.generatorBinding)
    compilerAssert(generator, 'Generator not found', { ast })
    const currentBranch = generator.currentBranch
    
    if (currentBranch.writeAddress) {
      compilerAssert(ast.expr, 'Expected expression', { ast })
      const addressReg = fnCodegen.generateExpressionToLValueRegister(ast.expr, { })
      fnCodegen.addInstruction(new CommentInstruction(`Write pointer ${addressReg} to ${currentBranch.writeAddress}`), ast.location)
      fnCodegen.generateCopyPrimitiveToAddressInstruction(currentBranch.writeAddress, addressReg, RawPointerType, ast.location)
    }

    const nextJumpRegion = currentBranch.regionIds.length
    fnCodegen.addInstruction(new CommentInstruction(`Set next jump = ${nextJumpRegion}`), ast.location)
    const constant = fnCodegen.generateExpressionToLoadRegister(new NumberAst(IntType, SourceLocation.anon, nextJumpRegion), { })
    fnCodegen.generateCopyPrimitiveToAddressInstruction(currentBranch.writeStateAddress, constant, IntType, ast.location)

    const readStateValue = fnCodegen.newRegister()
    fnCodegen.toValueRegister(readStateValue, IntType, currentBranch.readStateAddress, ast.location)
    const jumpInstr = fnCodegen.addInstruction(new JumpTableInstruction(IntType, readStateValue, []), ast.location)!
    currentBranch.jumpInstrs.push(jumpInstr)
    
    this.insertNewJumpPoint(generator)

    if (currentBranch.readAddress) {
      fnCodegen.addInstruction(new CommentInstruction(`Read pointer from ${currentBranch.readAddress}`), ast.location)
      fnCodegen.addInstruction(new MarkInitializedInstruction(currentBranch.readAddress, RawPointerType, true), ast.location)
      const addressReg = fnCodegen.newRegister()
      if (context.target instanceof PointerRegister) {
        context.target.outAddressRegister = addressReg
      } else if (context.target instanceof ValueRegister) {
      } else {
        compilerAssert(false, 'Unexpected ConstructValue target. When does this happen?', { context, ast })
      }
      
      fnCodegen.toValueRegister(addressReg, RawPointerType, currentBranch.readAddress, ast.location)
      fnCodegen.createLoadIntoRegisterFromContextIfNeccessary(context, addressReg, ast.type, ast.location)
      return
    }

  }
  
}