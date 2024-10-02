export function compilerAssert(expected: unknown, message: string="", info: object={}): asserts expected {
  if (expected) return;
  throw new Error(message, info)
}

export enum Capability {
  Let = "Let",
  Set = "Set",
  Inout = "Inout",
  Sink = "Sink",
}

export class LValue {
  constructor(public address: string) {}
}
export class RValue {
  constructor(public register: string) {}
}

export type IRValue =  LValue | RValue
export class Variable {
  constructor(
    public name: string, 
    public type: Type,
    public register: string,
    public isReference: boolean
  ) {}
}

// IR Instruction Base Class
export abstract class IRInstruction {
  abstract irType: string;
}

export class AssignInstruction extends IRInstruction {              irType = 'assign';                constructor(public dest: string, public source: string) { super(); } }
export class LoadConstantInstruction extends IRInstruction {        irType = 'loadconst';             constructor(public dest: string, public value: number) { super(); } }
export class AllocInstruction extends IRInstruction {               irType = 'alloc';                 constructor(public dest: string, public type: Type) { super(); } }
export class GetFieldPointerInstruction extends IRInstruction {     irType = 'getfieldptr';           constructor(public dest: string, public address: string, public field: number) { super(); } }
export class JumpInstruction extends IRInstruction {                irType = 'jump';                  constructor(public target: string) { super(); } }
export class ConditionalJumpInstruction extends IRInstruction {     irType = 'cjump';                 constructor(public condition: string, public targetLabel: string, public elseLabel: string) { super(); } }
export class BinaryOperationInstruction extends IRInstruction {     irType = 'binaryop';              constructor(public dest: string, public operator: string, public left: string, public right: string) { super(); } }
export class CallInstruction extends IRInstruction {                irType = 'call';                  constructor(public target: string, public functionName: string, public args: string[]) { super(); } }
export class ReturnInstruction extends IRInstruction {              irType = 'return';                constructor(public value: string | null) { super(); } }
export class AccessInstruction extends IRInstruction {              irType = 'access';                constructor(public dest: string, public source: string, public capabilities: Capability[]) { super(); } }
export class EndAccessInstruction extends IRInstruction {           irType = 'end_access';            constructor(public source: string, public capabilities: Capability[]) { super(); } }
export class StoreToAddressInstruction extends IRInstruction {      irType = 'store_to_address';      constructor(public address: string, public type: Type, public source: string) { super(); } }
export class LoadFromAddressInstruction extends IRInstruction {     irType = 'load_from_address';     constructor(public dest: string, public address: string) { super(); } }
export class AddressOfInstruction extends IRInstruction {           irType = 'addressof';             constructor(public dest: string, public source: string) { super(); } }
export class ComputeFieldAddressInstruction extends IRInstruction { irType = 'compute_field_address'; constructor(public dest: string, public address: string, public field: string) { super(); } }
export class MoveInstruction extends IRInstruction {                irType = 'move';                  constructor(public target: string, public source: string) { super(); } }
export class PhiInstruction extends IRInstruction {                 irType = 'phi';                   constructor(public dest: string, public sources: string[]) { super(); } }
export class CommentInstruction extends IRInstruction {             irType = 'comment';               constructor(public comment: string) { super(); } }

export const getInstructionOperands = (instr: IRInstruction): string[] => {
  if (instr instanceof AssignInstruction) { return [instr.source]; } 
  else if (instr instanceof BinaryOperationInstruction) { return [instr.left, instr.right]; } 
  else if (instr instanceof LoadFromAddressInstruction) { return [instr.address]; } 
  else if (instr instanceof StoreToAddressInstruction) { return [instr.address, instr.source]; } 
  else if (instr instanceof CallInstruction) { return [instr.target, ...instr.args]; } 
  else if (instr instanceof AccessInstruction) { return [instr.source]; } 
  else if (instr instanceof GetFieldPointerInstruction) { return [instr.address]; } 
  else if (instr instanceof ReturnInstruction) { return instr.value ? [instr.value] : []; } 
  else if (instr instanceof ComputeFieldAddressInstruction) { return [instr.address]; } 
  else if (instr instanceof AddressOfInstruction) { return [instr.source]; } 
  else if (instr instanceof MoveInstruction) { return [instr.target, instr.source]; }
  else if (instr instanceof PhiInstruction) { return instr.sources; }
  else { return []; }
}
export const getInstructionResult = (instr: IRInstruction): string | null => {
  if (instr instanceof AssignInstruction) { return instr.dest; } 
  else if (instr instanceof AllocInstruction) { return instr.dest; } 
  else if (instr instanceof CallInstruction) { return null; } 
  else if (instr instanceof GetFieldPointerInstruction) { return instr.dest; } 
  else if (instr instanceof LoadFromAddressInstruction) { return instr.dest } 
  else if (instr instanceof ComputeFieldAddressInstruction) { return instr.dest; } 
  else if (instr instanceof ReturnInstruction) { return null; } 
  else if (instr instanceof BinaryOperationInstruction) { return instr.dest; } 
  else if (instr instanceof StoreToAddressInstruction) { return instr.address; } 
  else if (instr instanceof AccessInstruction) { return instr.dest; } 
  else if (instr instanceof LoadConstantInstruction) { return instr.dest; } 
  else if (instr instanceof AddressOfInstruction) { return instr.dest; }
  else if (instr instanceof MoveInstruction) { return null; }
  else if (instr instanceof PhiInstruction) { return instr.dest; }
  else { return null; }
}

// Basic Block
export class BasicBlock {
  constructor(public label: string, public instructions: IRInstruction[]) {}
}

export class FunctionBlock {
  constructor(public name: string, public params: FunctionParameter[], public blocks: BasicBlock[]) {}
}

// Base AST Node Class
export abstract class ASTNode {
  abstract nodeType: string;
}


// Base Expression Node Class
export abstract class ExpressionNode extends ASTNode {}

export class ProgramNode extends ASTNode {                 nodeType = 'Program';              constructor(public body: ASTNode[]) { super(); } }
export class LetConstNode extends ASTNode {                nodeType = 'LetConst';             constructor(public name: string, public value: any) { super(); } }
export class VariableDeclarationNode extends ASTNode {     nodeType = 'VariableDeclaration';  constructor(public name: string, public mutable: boolean, public type: string) { super(); } }
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
export class FunctionDeclarationNode extends ASTNode {     nodeType = 'FunctionDeclaration';  constructor(public name: string, public params: FunctionParameterNode[], public body: BlockStatementNode) { super(); } }
export class AndNode extends ExpressionNode {              nodeType = 'AndExpression';        constructor(public left: ExpressionNode, public right: ExpressionNode) { super(); } }
export class OrNode extends ExpressionNode {               nodeType = 'OrExpression';         constructor(public left: ExpressionNode, public right: ExpressionNode) { super(); } }

export class FunctionParameterNode extends ASTNode {
  nodeType = 'FunctionParameter';
  constructor(public name: string, public type: string, public byReference: boolean) {
    super();
  }
}

export class FunctionParameter {
  constructor(public name: string, public type: Type, public byReference: boolean) {}
}

export function printIR(blocks: BasicBlock[]) {
  for (const block of blocks) {
    console.log(`\nBlock ${block.label}:`);
    for (const instr of block.instructions) {
      console.log(`  ${formatInstruction(instr)}`);
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
    return `into ${instr.target} call ${instr.functionName}(${instr.args.join(', ')})`;
  } else if (instr instanceof ReturnInstruction) {
    return `return ${instr.value}`;
  } else if (instr instanceof StoreToAddressInstruction) {
    return `into ${instr.address} store ${instr.source}`;
  } else if (instr instanceof LoadFromAddressInstruction) {
    return `${instr.dest} = load from address ${instr.address}`;
  } else if (instr instanceof AddressOfInstruction) {
    return `${instr.dest} = &${instr.source}`;
  } else if (instr instanceof ComputeFieldAddressInstruction) {
    return `${instr.dest} = &${instr.address}.${instr.field}`;
  } else if (instr instanceof AllocInstruction) {
    return `alloc ${instr.dest}: ${instr.type.shortName}`;
  } else if (instr instanceof AccessInstruction) {
    return `${instr.dest} = access [${instr.capabilities.join(', ')}] ${instr.source}`;
  } else if (instr instanceof EndAccessInstruction) {
    return `end_access [${instr.capabilities.join(', ')}] ${instr.source}`;
  } else if (instr instanceof GetFieldPointerInstruction) {
    return `${instr.dest} = address of ${instr.address}.${instr.field}`;
  } else if (instr instanceof MoveInstruction) {
    return `move ${instr.target} from ${instr.source}`;
  } else if (instr instanceof PhiInstruction) {
    return `${instr.dest} = phi(${instr.sources.join(', ')})`;
  } else if (instr instanceof CommentInstruction) {
    return `# ${instr.comment}`;
  } else {
    return `Unknown instruction: ${instr.irType}`;
  }
}

export class PrimitiveType {
  constructor(public name: string) {}
  get shortName() { return this.name }
}
export class ArrayType {
  constructor(public elementType: Type) {}
  get shortName() { return 'array' }
}
export class TypeField {
  constructor(public name: string, public type: Type) {}
}
export class StructType {
  constructor(public name: string, public fields: TypeField[]) {}
  get shortName() { return this.name }
}
export class GenericParameterType {
  constructor(public name: string) {}
  get shortName() { return this.name }
}
export class TypeConstructorType {
  constructor(public name: string, public typeParameters: Type[]) {}
  get shortName() { return this.name }
}
export class GenericInstanceType {
  constructor(public constructorType: TypeConstructorType, public typeArguments: Type[]) {}
  get shortName() { return this.constructorType.shortName }
}

export type Type = PrimitiveType | ArrayType | StructType | GenericParameterType | TypeConstructorType | GenericInstanceType

export const IntType = new PrimitiveType('int');
export const BoolType = new PrimitiveType('bool');
export const VoidType = new PrimitiveType('void');
export const StringType = new PrimitiveType('string');
export const FloatType = new PrimitiveType('float');

export const PointType = new StructType('Point', [
  new TypeField('x', IntType),
  new TypeField('y', IntType)
]);

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
