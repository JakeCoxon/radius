import { Binding, Capability, CompiledFunction, FunctionParameter, Type, TypeField } from "../src/defs";

export function compilerAssert(expected: unknown, message: string="", info: object={}): asserts expected {
  if (expected) return;
  console.dir(info, { depth: 4 })
  throw new Error(message, info)
}

export class Pointer {
  constructor(public address: string) {}
}
export class Value {
  constructor(public register: string) {}
}

export type IRValue =  Pointer | Value

export class Variable {
  constructor(
    public name: string, 
    public type: Type,
    public register: string,
    public capability: Capability
  ) {}
}

// IR Instruction Base Class
export abstract class IRInstruction {
  abstract irType: string;
}

export class PhiSource {
  constructor(public value: string, public block: string) {}
}

export class AssignInstruction extends IRInstruction {              irType = 'assign';                constructor(public dest: string, public source: string) { super(); } }
export class LoadConstantInstruction extends IRInstruction {        irType = 'loadconst';             constructor(public dest: string, public type: Type, public value: number) { super(); } }
export class AllocInstruction extends IRInstruction {               irType = 'alloc';                 constructor(public dest: string, public type: Type) { super(); } }
export class GetFieldPointerInstruction extends IRInstruction {     irType = 'getfieldptr';           constructor(public dest: string, public address: string, public field: TypeField) { super(); } }
export class JumpInstruction extends IRInstruction {                irType = 'jump';                  constructor(public target: string) { super(); } }
export class ConditionalJumpInstruction extends IRInstruction {     irType = 'cjump';                 constructor(public condition: string, public targetLabel: string, public elseLabel: string) { super(); } }
export class BinaryOperationInstruction extends IRInstruction {     irType = 'binaryop';              constructor(public dest: string, public type: Type, public operator: string, public left: string, public right: string) { super(); } }
export class CallInstruction extends IRInstruction {                irType = 'call';                  constructor(public target: string | null, public type: Type, public binding: Binding, public args: string[]) { super(); } }
export class ReturnInstruction extends IRInstruction {              irType = 'return';                constructor(public value: string | null) { super(); } }
export class AccessInstruction extends IRInstruction {              irType = 'access';                constructor(public dest: string, public source: string, public capabilities: Capability[]) { super(); } }
export class EndAccessInstruction extends IRInstruction {           irType = 'end_access';            constructor(public source: string, public capabilities: Capability[]) { super(); } }
export class StoreToAddressInstruction extends IRInstruction {      irType = 'store_to_address';      constructor(public address: string, public type: Type, public source: string) { super(); } }
export class LoadFromAddressInstruction extends IRInstruction {     irType = 'load_from_address';     constructor(public dest: string, public type: Type, public address: string) { super(); } }
export class MoveInstruction extends IRInstruction {                irType = 'move';                  constructor(public target: string, public source: string, public type: Type) { super(); } }
export class MarkInitializedInstruction extends IRInstruction {     irType = 'mark_initialized';      constructor(public target: string, public initialized: boolean) { super(); } }
export class PhiInstruction extends IRInstruction {                 irType = 'phi';                   constructor(public dest: string, public type: Type, public sources: PhiSource[]) { super(); } }
export class CommentInstruction extends IRInstruction {             irType = 'comment';               constructor(public comment: string) { super(); } }

export const getInstructionOperands = (instr: IRInstruction): string[] => {
  if (instr instanceof AssignInstruction)               { return [instr.source]; } 
  else if (instr instanceof BinaryOperationInstruction) { return [instr.left, instr.right]; } 
  else if (instr instanceof LoadFromAddressInstruction) { return [instr.address]; } 
  else if (instr instanceof StoreToAddressInstruction)  { return [instr.address, instr.source]; } 
  else if (instr instanceof CallInstruction)            { return instr.target ? [instr.target, ...instr.args] : [...instr.args]; } 
  else if (instr instanceof AccessInstruction)          { return [instr.source]; } 
  else if (instr instanceof GetFieldPointerInstruction) { return [instr.address]; } 
  else if (instr instanceof ReturnInstruction)          { return instr.value ? [instr.value] : []; } 
  else if (instr instanceof MoveInstruction)            { return [instr.target, instr.source]; }
  else if (instr instanceof PhiInstruction)             { return instr.sources.map(s => s.value); }
  else if (instr instanceof EndAccessInstruction)       { return [instr.source]; }
  else if (instr instanceof MarkInitializedInstruction) { return [instr.target]; }
  else { return []; }
}

export const getInstructionResult = (instr: IRInstruction): string | null => {
  if (instr instanceof AssignInstruction)               { return instr.dest; } 
  else if (instr instanceof AllocInstruction)           { return instr.dest; } 
  else if (instr instanceof CallInstruction)            { return null; } 
  else if (instr instanceof GetFieldPointerInstruction) { return instr.dest; } 
  else if (instr instanceof LoadFromAddressInstruction) { return instr.dest } 
  else if (instr instanceof ReturnInstruction)          { return null; } 
  else if (instr instanceof BinaryOperationInstruction) { return instr.dest; } 
  else if (instr instanceof StoreToAddressInstruction)  { return instr.address; } 
  else if (instr instanceof AccessInstruction)          { return instr.dest; } 
  else if (instr instanceof LoadConstantInstruction)    { return instr.dest; } 
  else if (instr instanceof MoveInstruction)            { return null; }
  else if (instr instanceof PhiInstruction)             { return instr.dest; }
  else { return null; }
}

// Basic Block
export class BasicBlock {
  constructor(public label: string, public instructions: IRInstruction[]) {}
}

export class FunctionBlock {
  constructor(
    public name: string, 
    public binding: Binding,
    public params: FunctionParameter[],
    public parameterRegisters: string[],
    public blocks: BasicBlock[]) {}
}

export class Module {
  functionMap: Map<Binding, CompiledFunction> = new Map();
  constructor() {}
}

// Base AST Node Class
export abstract class ASTNode {
  abstract nodeType: string;
}


// Base Expression Node Class
export abstract class ExpressionNode extends ASTNode {}

// TODO: These become TestUtil nodes and we create a transformation
// into proper typed AST nodes, which gives us a way to test just
// the IR generation and optimization passes in a more isolated way
export class ProgramNode extends ASTNode {                 nodeType = 'Program';              constructor(public body: ASTNode[]) { super(); } }
export class LetConstNode extends ASTNode {                nodeType = 'LetConst';             constructor(public name: string, public value: any) { super(); } }
export class VariableDeclarationNode extends ASTNode {     nodeType = 'VariableDeclaration';  constructor(public name: string, public mutable: boolean, public type: string, public initializer?: ExpressionNode | undefined) { super(); } }
export class ExpressionStatementNode extends ASTNode {     nodeType = 'ExpressionStatement';  constructor(public expression: ExpressionNode) { super(); } }
export class IfStatementNode extends ASTNode {             nodeType = 'IfStatement';          constructor(public condition: ExpressionNode, public consequent: ASTNode, public alternate?: ASTNode) { super(); } }
export class WhileStatementNode extends ASTNode {          nodeType = 'WhileStatement';       constructor(public condition: ExpressionNode, public body: ASTNode) { super(); } }
export class BlockStatementNode extends ASTNode {          nodeType = 'BlockStatement';       constructor(public body: ASTNode[]) { super(); } }
export class AssignmentNode extends ExpressionNode {       nodeType = 'AssignmentExpression'; constructor(public left: ExpressionNode, public right: ExpressionNode) { super(); } }
export class MemberExpressionNode extends ExpressionNode { nodeType = 'MemberExpression';     constructor(public object: ExpressionNode, public type: string, public property: string) { super(); } }
export class IdentifierNode extends ExpressionNode {       nodeType = 'Identifier';           constructor(public name: string) { super(); } }
export class LiteralNode extends ExpressionNode {          nodeType = 'Literal';              constructor(public value: any) { super(); } }
export class BinaryExpressionNode extends ExpressionNode { nodeType = 'BinaryExpression';     constructor(public operator: string, public left: ExpressionNode, public right: ExpressionNode) { super(); } }
export class CallExpressionNode extends ExpressionNode {   nodeType = 'CallExpression';       constructor(public callee: string, public args: ExpressionNode[]) { super(); } }
export class CreateStructNode extends ExpressionNode {     nodeType = 'CreateStruct';         constructor(public name: string, public fields: ExpressionNode[]) { super(); } }
export class ReturnNode extends ASTNode {                  nodeType = 'ReturnStatement';      constructor(public argument: ExpressionNode | undefined = undefined) { super(); } }
export class FunctionDeclarationNode extends ASTNode {     nodeType = 'FunctionDeclaration';  constructor(public name: string, public params: FunctionParameterNode[], public returnType: string, public body: BlockStatementNode) { super(); } }
export class AndNode extends ExpressionNode {              nodeType = 'AndExpression';        constructor(public left: ExpressionNode, public right: ExpressionNode) { super(); } }
export class OrNode extends ExpressionNode {               nodeType = 'OrExpression';         constructor(public left: ExpressionNode, public right: ExpressionNode) { super(); } }
export class PrintNode extends ExpressionNode {            nodeType = 'Print';                constructor(public value: ExpressionNode) { super(); } }

export class FunctionParameterNode extends ASTNode {
  nodeType = 'FunctionParameter';
  constructor(public name: string, public type: string, public capability: Capability) {
    super();
  }
}

export function printIR(blocks: BasicBlock[]) {
  for (const block of blocks) {
    console.log(`\nBlock ${block.label}:`);
    let i = 0;
    for (const instr of block.instructions) {
      
      if ((instr instanceof CommentInstruction)) {
        console.log(`  ${formatInstruction(instr)}`);
      } else {
        console.log(`${i}.  ${formatInstruction(instr)}`);
      }
      i++
    }
  }
}

export function formatInstruction(instr: IRInstruction): string {
  if (instr instanceof AssignInstruction) {
    return `${instr.dest} = ${instr.source}`;
  } else if (instr instanceof LoadConstantInstruction) {
    return `${instr.dest} = constant ${instr.value}`;
  } else if (instr instanceof BinaryOperationInstruction) {
    return `${instr.dest} = ${instr.left} ${instr.operator} ${instr.right}`;
  } else if (instr instanceof ConditionalJumpInstruction) {
    return `if ${instr.condition} != 0 goto ${instr.targetLabel} else goto ${instr.elseLabel}`;
  } else if (instr instanceof JumpInstruction) {
    return `goto ${instr.target}`;
  } else if (instr instanceof CallInstruction) {
    return `into ${instr.target} call ${instr.binding.name}(${instr.args.join(', ')})`;
  } else if (instr instanceof ReturnInstruction) {
    return `return ${instr.value}`;
  } else if (instr instanceof StoreToAddressInstruction) {
    return `into ${instr.address} store ${instr.source}`;
  } else if (instr instanceof LoadFromAddressInstruction) {
    return `${instr.dest} = load from address ${instr.address}`;
  } else if (instr instanceof AllocInstruction) {
    return `alloc ${instr.dest}: ${instr.type.shortName}`;
  } else if (instr instanceof AccessInstruction) {
    return `${instr.dest} = access [${instr.capabilities.join(', ')}] ${instr.source}`;
  } else if (instr instanceof EndAccessInstruction) {
    return `end_access [${instr.capabilities.join(', ')}] ${instr.source}`;
  } else if (instr instanceof GetFieldPointerInstruction) {
    return `${instr.dest} = offset address ${instr.address} .${instr.field.index}`;
  } else if (instr instanceof MoveInstruction) {
    return `into ${instr.target} move from ${instr.source}`;
  } else if (instr instanceof MarkInitializedInstruction) {
    return `mark ${instr.target} as ${instr.initialized ? 'initialized' : 'uninitialized'}`;
  } else if (instr instanceof PhiInstruction) {
    return `${instr.dest} = phi(${instr.sources.map(x => `${x.value} @ ${x.block}`).join(', ')})`;
  } else if (instr instanceof CommentInstruction) {
    return `\n# ${instr.comment}`;
  } else {
    return `Unknown instruction: ${instr.irType}`;
  }
}

export class InstructionId {
  constructor(public blockId: string, public instrId: number) { }
}

export enum LivenessType {
  LiveIn = 'LiveIn',
  LiveOut = 'LiveOut',
  LiveInAndOut = 'LiveInAndOut',
  Closed = 'Closed'
}

export class LivenessState {
  static LiveInAndOut = new LivenessState(LivenessType.LiveInAndOut);
  static LiveOut = new LivenessState(LivenessType.LiveOut);
  static LiveIn = (lastUse: InstructionId | null) => new LivenessState(LivenessType.LiveIn, lastUse);
  static Closed = (lastUse: InstructionId | null) => new LivenessState(LivenessType.Closed, lastUse);
  private constructor(public livenessType: LivenessType, public lastUse: InstructionId | null = null) { }
}

// Register -> Block -> LivenessState
export type LivenessMap = Record<string, Record<string, LivenessState>>

export const printLivenessMap = (liveness: LivenessMap) => {
  for (const [operand, livenessMap] of Object.entries(liveness)) {
    console.log(`Liveness for ${operand}:`);
    for (const [blockId, state] of Object.entries(livenessMap)) {
      console.log(`  ${blockId}: ${state.livenessType}${state.lastUse ? ` (last use: ${state.lastUse.blockId}:${state.lastUse.instrId})` : ''}`);
    }
  }
}

export type Usage = {
  instrId: InstructionId,
  operandIndex: number
}
export type UsageMap = Map<string, Usage[]>;

export const createUsageMap = (blocks: BasicBlock[]): UsageMap => {

  const usageMap = new Map<string, Usage[]>();
  for (const block of blocks) {
    for (let id = 0; id < block.instructions.length; id++) {
      const instr = block.instructions[id];
      const operands = getInstructionOperands(instr);
      for (let i = 0; i < operands.length; i++) {
        const operand = operands[i];
        const usage = { instrId: new InstructionId(block.label, id), operandIndex: i };
        if (usageMap.has(operand)) {
          usageMap.get(operand)!.push(usage);
        } else {
          usageMap.set(operand, [usage]);
        }
      }
    }
  }
  return usageMap;
}


// https://gist.github.com/JBlond/2fea43a3049b38287e5e9cefc87b2124
export const textColors = {
  red: (string: string) => `\x1b[31m${string}\x1b[39m`,
  yellow: (string: string) => `\x1b[33m${string}\x1b[39m`,
  green: (string: string) => `\x1b[32m${string}\x1b[39m`,
  cyan: (string: string) => `\x1b[36m${string}\x1b[39m`,
  gray: (string: string) => `\x1b[38;5;242m${string}\x1b[39m`,
}
