import { inspect } from "bun";
import { AccessInstruction, CallInstruction, CommentInstruction, EndAccessInstruction, formatInstruction, GetFieldPointerInstruction, getInstructionIdentifier, getInstructionOperands, getInstructionResult, IRInstruction, MarkInitializedInstruction, MoveInstruction } from "../borrow/defs";
import { Binding, Capability, CompiledFunction, compilerAssert, FunctionParameter, textColors, Type, VoidType } from "../src/defs";
import { CodeGenerator } from "../borrow/codegen_ir";

export type Regions = Region[];

export class InstructionNode {
  constructor(public instruction: IRInstruction, public prev: InstructionId | null, public next: InstructionId | null, public region: RegionId) {}
}

export class IrFunction {
  regions: Region[] = []
  sequences: RegionSequence[] = []
  instructions: { [key: string]: InstructionNode } = {}
  root: SequenceId
  returnParameter: FunctionParameter | null = null
  returnRegister: string;
  returnType: Type // May be converted to VoidType if returnRegister is used

  constructor(
    public debugName: string,
    public params: FunctionParameter[],
    public parameterRegisters: string[],
  ) {}

  getInstruction(instrId: InstructionId): IRInstruction | null { return this.instructions[instrId]?.instruction ?? null; }
  getInstructionNode(instrId: InstructionId): InstructionNode | null { return this.instructions[instrId] ?? null; }
  getInstructionRegion(instrId: InstructionId): RegionId { 
    // @Speed issue, remove this later
    compilerAssert(this.instructions[instrId], "Instruction not found", { instrId });
    return this.instructions[instrId].region ?? null;
  }
}

export type RegionId = number & { __regionId: true };
export type SequenceId = number & { __sequenceId: true };
export type InstructionId = string & { __instructionId: true };

export class RegionSequence {
  firstChildRegion: RegionId | null = null
  lastChildRegion: RegionId | null = null
  parentRegion: RegionId | null = null
}

export class BlockRegion {
  firstInstruction: InstructionId | null = null
  lastInstruction: InstructionId | null = null
  parentSequence: SequenceId
  prevRegion: RegionId | null = null
  nextRegion: RegionId | null = null
  result: string
  constructor() {
  }
}

export class WhileRegion {
  parentSequence: SequenceId
  prevRegion: RegionId | null = null
  nextRegion: RegionId | null = null
  conditionSequence: SequenceId
  bodySequence: SequenceId
  exitSequence: SequenceId
  result: string
  conditionRegister: string
  constructor() {}
}

export class IfRegion {
  parentSequence: SequenceId
  prevRegion: RegionId | null = null
  nextRegion: RegionId | null = null
  conditionSequence: SequenceId
  thenSequence: SequenceId
  elseSequence: SequenceId
  exitSequence: SequenceId
  result: string
  conditionRegister: string
  constructor() {}
}

export class ScopeRegion {
  parentSequence: SequenceId
  prevRegion: RegionId | null = null
  nextRegion: RegionId | null = null
  bodySequence: SequenceId
  exitSequence: SequenceId
  result: string
  constructor() {}
}

export type InsertPosition = { startOfRegion: RegionId } | { endOfRegion: RegionId } | { afterInstruction: InstructionId } | { beforeInstruction: InstructionId };
export const InsertPosition = {
  startOfRegion: (region: RegionId): InsertPosition => ({ startOfRegion: region }),
  endOfRegion: (region: RegionId): InsertPosition => ({ endOfRegion: region }),
  after: (instr: InstructionId): InsertPosition => ({ afterInstruction: instr }),
  before: (instr: InstructionId): InsertPosition => ({ beforeInstruction: instr }),
}
export type Region = BlockRegion | IfRegion | WhileRegion | ScopeRegion;

export class IrDiagnostics {
  instructionNotes: { instrId: InstructionId, note: string }[] = []
  regionNotes: { regionId: RegionId, note: string }[] = []

  instructionNote(instrId: InstructionId, note: string) {
    this.instructionNotes.push({ instrId, note });
  }
  regionNote(regionId: RegionId, note: string) {
    this.regionNotes.push({ regionId, note });
  }
}

export const printIrFunction = (function_: IrFunction, diagnostics?: IrDiagnostics) => {

  const regionsSet = new Set<number>(function_.regions.map((_, i) => i));
  const sequencesSet = new Set<number>(function_.sequences.map((_, i) => i));

  const instructionNotes: { [key: string]: string[] } = {};
  diagnostics?.instructionNotes.forEach(({ instrId, note }) => {
    const origNote = instructionNotes[instrId] ?? [];
    instructionNotes[instrId] = [...origNote, note];
  });
  const regionNotes: { [key: string]: string[] } = {};
  diagnostics?.regionNotes.forEach(({ regionId, note }) => {
    const origNote = regionNotes[regionId] ?? [];
    regionNotes[regionId] = [...origNote, note];
  });
  
  const visitRegion = (region: Region, label: string, depth = 0, isLast = true, prefix = "", regionId: number) => {
    const branch = isLast ? "└─ " : "├─ ";
    const connection = depth > 0 ? prefix + branch : "";
    const typeLabel = region ? textColors.blue(`[${region.constructor.name.replace("Region", "")} ${regionId}]`) : textColors.gray("undefined");
    console.log(`${connection}${typeLabel}`, region?.result ?? "");
    compilerAssert(regionsSet.has(regionId), "Region already visited", { regionId, regionsSet });
    regionsSet.delete(regionId);
    
    let nextPrefix = prefix + (isLast ? "   " : "│  ");

    if (regionNotes[regionId]) {
      regionNotes[regionId].join("\n").split("\n").forEach(line => {
        console.log(nextPrefix + textColors.green(`|  ${line}`));
        // console.log(indent + textColors.green(`|`) + `  ${line}`);
      })
    }
    
    if (region instanceof BlockRegion) {
      for (let instrId = region.firstInstruction; instrId !== null; instrId = function_.getInstructionNode(instrId)?.next ?? null) {
        const instruction = function_.getInstruction(instrId)
        const node = function_.getInstructionNode(instrId)
        compilerAssert(node?.region === regionId, "Sanity check - Instruction region mismatch", { instrId, regionId, node });
        compilerAssert(instruction, "Instruction not found", { instrId, region, function_ });
        const isLastInstruction = instrId === region.lastInstruction;
        // let label = instruction.result ? `${instruction.result} = ${instruction.label}` : instruction.label;
        let label = formatInstruction(instruction);
        label = instruction instanceof CommentInstruction ? textColors.gray(`// ${label}`) : label;
        label += textColors.gray(` (${instrId})`);
        let indent = nextPrefix + (isLastInstruction ? "   " : "   ")
        indent += (" ".repeat(Math.max(0, 15 - indent.length)));
        console.log(indent + label);
        if (instructionNotes[instrId]) {
          instructionNotes[instrId].join("\n").split("\n").forEach(line => {
            console.log(indent + textColors.green(`|  ${line}`));
            // console.log(indent + textColors.green(`|`) + `  ${line}`);
          })
        }
      }
    } else if (region instanceof IfRegion) {
      visitSequence(region.conditionSequence, "Cond", depth + 1, false, nextPrefix);
      visitSequence(region.thenSequence, "Then", depth + 1, false, nextPrefix);
      visitSequence(region.elseSequence, "Else", depth + 1, false, nextPrefix);
      visitSequence(region.exitSequence, "Exit", depth + 1, true, nextPrefix);
    } else if (region instanceof WhileRegion) {
      visitSequence(region.conditionSequence, "Cond", depth + 1, false, nextPrefix);
      visitSequence(region.bodySequence, "Body", depth + 1, false, nextPrefix);
      visitSequence(region.exitSequence, "Exit", depth + 1, true, nextPrefix);
    } else if (region instanceof ScopeRegion) {
      visitSequence(region.bodySequence, "Body", depth + 1, false, nextPrefix);
      visitSequence(region.exitSequence, "Exit", depth + 1, true, nextPrefix);
    } else {
      compilerAssert(false, "Unknown region type", { region });
    }
  };
  const visitSequence = (sequenceId: number, label: string, depth = 0, isLast = true, prefix = "") => {
    const sequence = function_.sequences[sequenceId]
    const branch = isLast ? "└─ " : "├─ ";
    const connection = depth > 0 ? prefix + branch : "";
    console.log(`${connection}${textColors.green(label)} (${sequenceId})`);
    // console.log({ sequenceId, sequence });
    compilerAssert(sequencesSet.has(sequenceId), "Sequence already visited", { sequencesSet, sequenceId });
    sequencesSet.delete(sequenceId);
    
    let nextPrefix = prefix + (depth === 0 ? '' : isLast ? "   " : "│  ");
    
    let region = sequence.firstChildRegion;
    let i = 0;
    while (region !== null) {
      const isLast = region === sequence.lastChildRegion;
      
      visitRegion(function_.regions[region], label, depth + 1, isLast, nextPrefix, region);
      region = function_.regions[region].nextRegion;
      i++;
    }
  }

  console.log(textColors.green(`Function: ${function_.debugName}`));
  visitSequence(function_.root, "Root");
  if (regionsSet.size > 0) {
    console.log("Unvisited regions", regionsSet);
  }
  if (sequencesSet.size > 0) {
    console.log("Unvisited sequences", sequencesSet);
  }
};

export class RegionCodegen {

  regionSequence: SequenceId | null = null
  blockRegion: RegionId | null = null
  allocBlock: RegionId | null = null
  currentRegion: RegionId | null = null

  regionState: { region: RegionId | null, sequence: SequenceId }[] = []

  constructor(
    public irFunction: IrFunction,
    public compiledFunction: CompiledFunction,
    public globalState: CodeGenerator
  ) {
  }

  newRegister() { return this.globalState.newRegister(); }
  newFreshId() { return this.globalState.newFreshId(); }

  createRootSequenceRegion() {
    this.regionSequence = this.insertNewSequenceRegion(null);
    this.currentRegion = null
    this.allocBlock = this.insertNewBlockRegion();
    this.insertSequenceChild(this.regionSequence, this.allocBlock);
    this.irFunction.root = this.regionSequence;
  }

  insertNewSequenceRegion(parentRegion: RegionId | null): SequenceId {
    const sequence = new RegionSequence()
    sequence.parentRegion = parentRegion;
    this.irFunction.sequences.push(sequence);
    return this.irFunction.sequences.length - 1 as SequenceId;
  }

  checkRegion(region: RegionId, info: any) {
    compilerAssert(region >= 0 && region < this.irFunction.regions.length, "Region out of bounds", { region, regions: this.irFunction.regions });
    this.irFunction.sequences.forEach((sequence, i) => {
      compilerAssert(sequence.firstChildRegion !== region, "Region already has parent", { region, sequence, i });
      compilerAssert(sequence.lastChildRegion !== region, "Region already has parent", { region, sequence, i });
      for (let r = sequence.firstChildRegion; r !== null; r = this.irFunction.regions[r].nextRegion) {
        compilerAssert(r !== region, "Region already has parent", { region, sequence, i });
      }
    })
  }

  ensureBlock() {
    compilerAssert(this.regionSequence !== null, 'No regionSequence', { regionSequence: this.regionSequence });
    if (this.blockRegion === null) {
      this.blockRegion = this.insertNewBlockRegion();
      this.insertSequenceChild(this.regionSequence, this.blockRegion);
    }
  }

  setInsertionBlock(regionId: RegionId) {
    const region = this.irFunction.regions[regionId];
    compilerAssert(region instanceof BlockRegion, "Region is not a block", { region });
    this.blockRegion = regionId;
  }

  insertInstruction(instr: IRInstruction) {
    compilerAssert(this.blockRegion !== null, 'No blockRegion. Call ensureBlock()', { blockRegion: this.blockRegion });
    return this.insertBlockInstruction(this.blockRegion, instr);
  }

  enterRegionSequence(region: RegionId, sequenceId: SequenceId) {
    this.currentRegion = region
    this.regionSequence = sequenceId
    this.blockRegion = null
  }

  insertChildSequence(region: RegionId) {
    compilerAssert(this.regionSequence !== null, 'No parentSequence', { parentSequence: this.regionSequence });
    this.insertSequenceChild(this.regionSequence, region)
    this.currentRegion = region
  }

  insertChildSequenceAndPushState(region: RegionId) {
    this.insertChildSequence(region)
    this.pushRegionState()
  }

  pushRegionState() {
    compilerAssert(this.regionSequence !== null, 'No parentSequence', { parentSequence: this.regionSequence });
    this.regionState.push({ region: this.currentRegion, sequence: this.regionSequence })
  }

  popRegionState() {
    const state = this.regionState.pop()!
    this.currentRegion = state.region
    this.regionSequence = state.sequence
    this.blockRegion = null
  }
  

  insertSequenceChild(parentSequence: SequenceId, child: RegionId) {
    const region = this.irFunction.sequences[parentSequence];
    const childRegion = this.irFunction.regions[child];
    this.checkRegion(child, { parentSequence })
    if (region.lastChildRegion === null) {
      region.firstChildRegion = child;
      region.lastChildRegion = child;
    } else {
      const lastChild = this.irFunction.regions[region.lastChildRegion];
      compilerAssert(lastChild, "lastChild is null", { region, child, parentSequence });
      lastChild.nextRegion = child;
      childRegion.prevRegion = region.lastChildRegion;
      region.lastChildRegion = child;
    }
    childRegion.parentSequence = parentSequence;
  }

  insertSequenceChildAfter(parentSequence: SequenceId, prevRegion: RegionId, child: RegionId) {
    const region = this.irFunction.sequences[parentSequence];
    compilerAssert(region, "Region not found", { parentSequence, prevRegion, child });
    const childRegion = this.irFunction.regions[child];
    if (region.lastChildRegion === prevRegion) {
      this.insertSequenceChild(parentSequence, child);
      return;
    } else {
      const prev = this.irFunction.regions[prevRegion];
      compilerAssert(prev.nextRegion, "prev.next is null", { region, prevRegion, child, parentSequence });
      const next = this.irFunction.regions[prev.nextRegion];
      compilerAssert(next, "next is null", { region, prevRegion, child, parentSequence });
      childRegion.nextRegion = prev.nextRegion;
      childRegion.prevRegion = prevRegion;
      childRegion.parentSequence = parentSequence;
      prev.nextRegion = child;
      next.prevRegion = child;
    }
  }

  insertSequenceChildAtBeginning(parentSequence: SequenceId, child: RegionId) {
    const region = this.irFunction.sequences[parentSequence];
    const childRegion = this.irFunction.regions[child];
    if (region.firstChildRegion === null) {
      region.firstChildRegion = child;
      region.lastChildRegion = child;
    } else {
      const firstChild = this.irFunction.regions[region.firstChildRegion];
      compilerAssert(firstChild, "firstChild is null", { region, child, parentSequence });
      firstChild.prevRegion = child;
      childRegion.nextRegion = region.firstChildRegion;
      region.firstChildRegion = child;
    }
    childRegion.parentSequence = parentSequence;
  }

  moveRegionsToAfter(prevRegion: RegionId, startRegion: RegionId, endRegion: RegionId) {
    
    const parentSequence = this.irFunction.regions[startRegion].parentSequence;
    compilerAssert(parentSequence !== null, "Parent sequence not found", { startRegion, parentSequence });
    const sequence = this.irFunction.sequences[parentSequence];
    compilerAssert(sequence, "Sequence not found", { parentSequence, startRegion, endRegion });
    const prev = this.irFunction.regions[prevRegion];
    compilerAssert(prev, "prevRegion not found", { prevRegion, startRegion, endRegion });
    const start = this.irFunction.regions[startRegion];
    compilerAssert(start, "startRegion not found", { prevRegion, startRegion, endRegion });
    const end = this.irFunction.regions[endRegion]
    compilerAssert(end.parentSequence === start.parentSequence, "endRegion is not in the same sequence", { parentSequence, startRegion, endRegion });
    const oldSequence = this.irFunction.sequences[start.parentSequence];

    const oldPrevId = start.prevRegion;
    const oldNextId = end.nextRegion;
    const newNextId = prev.nextRegion;

    prev.nextRegion = startRegion;
    start.prevRegion = prevRegion;

    if (sequence.lastChildRegion === prevRegion) {
      sequence.lastChildRegion = endRegion
    } else {
      const next = this.irFunction.regions[newNextId!]
      compilerAssert(next, "nextRegion not found", { prevRegion, startRegion, endRegion });
      
      end.nextRegion = newNextId;
      next.prevRegion = prevRegion;
    }

    // Fix up old sequence

    if (oldSequence.firstChildRegion === startRegion) {
      oldSequence.firstChildRegion = oldNextId // pointer to the one after the end
    } else {
      const oldPrev = this.irFunction.regions[oldPrevId!]
      compilerAssert(oldPrev, "prevRegion not found", { prevRegion, startRegion, endRegion });
      oldPrev.nextRegion = oldNextId
    }

    if (oldSequence.lastChildRegion === endRegion) {
      oldSequence.lastChildRegion = oldPrevId // pointer to the one before the start
    } else {
      const oldNext = this.irFunction.regions[oldNextId!]
      compilerAssert(oldNext, "nextRegion not found", { prevRegion, startRegion, endRegion });
      oldNext.prevRegion = oldPrevId
    }

    // Fix up parent sequence ids

    const onePastEnd = this.irFunction.regions[endRegion].nextRegion
    for (let regionId: RegionId | null = startRegion; regionId !== null && regionId !== onePastEnd; regionId = this.irFunction.regions[regionId].nextRegion) {
      this.irFunction.regions[regionId].parentSequence = parentSequence;
    }

  }
    

  createInstructionId(parent: RegionId, instruction: IRInstruction): InstructionId {
    const name = getInstructionIdentifier(instruction) ?? this.newFreshId();
    return name as InstructionId;
  }

  getInstructionById(instrId: InstructionId): IRInstruction {
    const instrNode = this.irFunction.getInstruction(instrId)
    compilerAssert(instrNode, "Instruction not found", { instrId, instructions: this.irFunction.instructions });
    return instrNode;
  }

  insertBlockInstruction(parent: RegionId, instruction: IRInstruction) {
    const instrId = this.createInstructionId(parent, instruction);
    compilerAssert(!this.irFunction.getInstruction(instrId), "Instruction already exists", { instrId, instruction, instructions: this.irFunction.instructions });
    const region = this.irFunction.regions[parent] as BlockRegion;
    const newNode = new InstructionNode(instruction, region.lastInstruction, null, parent)
    this.irFunction.instructions[instrId] = newNode;
    if (region.lastInstruction === null) {
      region.firstInstruction = instrId;
      region.lastInstruction = instrId;
    } else {
      const lastNode = this.irFunction.getInstructionNode(region.lastInstruction)!;
      lastNode.next = instrId;
      region.lastInstruction = instrId;
    }
    return instrId;
  }

  insertInstructionAtBeginning(parent: RegionId, instruction: IRInstruction) {
    const instrId = this.createInstructionId(parent, instruction);
    compilerAssert(!this.irFunction.getInstruction(instrId), "Instruction already exists", { instrId, instruction, instructions: this.irFunction.instructions });
    const region = this.irFunction.regions[parent] as BlockRegion;
    const newNode = new InstructionNode(instruction, null, region.firstInstruction, parent)
    this.irFunction.instructions[instrId] = newNode;
    if (region.firstInstruction === null) {
      region.firstInstruction = instrId;
      region.lastInstruction = instrId;
    } else {
      const firstNode = this.irFunction.getInstructionNode(region.firstInstruction)!;
      firstNode.prev = instrId;
      region.firstInstruction = instrId;
    }
    return instrId;
  }

  insertInstructionAfter(prevId: InstructionId, instruction: IRInstruction) {
    const regionId = this.irFunction.instructions[prevId].region;
    const instrId = this.createInstructionId(regionId, instruction);
    compilerAssert(!this.irFunction.getInstruction(instrId), "Instruction already exists", { instrId, instruction, instructions: this.irFunction.instructions });
    const region = this.irFunction.regions[regionId] as BlockRegion;
    const prevNode = this.irFunction.instructions[prevId];
    const nextId = prevNode.next;
    const nextNode = this.irFunction.instructions[nextId!];
    const newNode = new InstructionNode(instruction, prevId, nextId, regionId)
    this.irFunction.instructions[instrId] = newNode;
    prevNode.next = instrId;
    if (region.lastInstruction === prevId) {
      region.lastInstruction = instrId;
    } else {
      nextNode.prev = instrId;
    }
    return instrId;
  }

  insertInstructionBefore(nextId: InstructionId, instruction: IRInstruction) {
    const regionId = this.irFunction.instructions[nextId].region;
    const instrId = this.createInstructionId(regionId, instruction);
    compilerAssert(!this.irFunction.getInstruction(instrId), "Instruction already exists", { instrId, instruction, instructions: this.irFunction.instructions });
    const region = this.irFunction.regions[regionId] as BlockRegion;
    const nextNode = this.irFunction.instructions[nextId];
    const prevId = nextNode.prev;
    const prevNode = this.irFunction.instructions[prevId!];
    const newNode = new InstructionNode(instruction, prevId, nextId, regionId)
    this.irFunction.instructions[instrId] = newNode;
    nextNode.prev = instrId;
    if (region.firstInstruction === nextId) {
      region.firstInstruction = instrId;
    } else {
      prevNode.next = instrId;
    }
    return instrId;
  }

  insertInstructionAtPosition(insertPosition: InsertPosition, instruction: IRInstruction) {
    if ('startOfRegion' in insertPosition) {
      return this.insertInstructionAtBeginning(insertPosition.startOfRegion, instruction);
    } else if ('endOfRegion' in insertPosition) {
      return this.insertBlockInstruction(insertPosition.endOfRegion, instruction);
    } else if ('afterInstruction' in insertPosition) {
      return this.insertInstructionAfter(insertPosition.afterInstruction, instruction);
    } else if ('beforeInstruction' in insertPosition) {
      return this.insertInstructionBefore(insertPosition.beforeInstruction, instruction);
    }
    compilerAssert(false, "Invalid insertPosition", { insertPosition });
  }

  insertInstructionsAtPosition(insertPosition: InsertPosition, instructions: IRInstruction[]) {
    const ids = { firstId: null as InstructionId | null, lastId: null as InstructionId | null }
    instructions.forEach(instr => {
      const newId = this.insertInstructionAtPosition(insertPosition, instr)
      if (ids.firstId === null) ids.firstId = newId
      ids.lastId = newId
      insertPosition = { afterInstruction: newId }
    })
    return ids
  }

  deleteInstruction(instrId: InstructionId) {
    const instrNode = this.irFunction.getInstructionNode(instrId);
    compilerAssert(instrNode, "Instruction not found", { instrId, instructions: this.irFunction.instructions });
    const region = this.irFunction.regions[instrNode.region] as BlockRegion;
    let replacingPosition: InsertPosition
    if (instrId === region.firstInstruction) {
      region.firstInstruction = instrNode.next;
      replacingPosition = InsertPosition.startOfRegion(instrNode.region);
    } else {
      const prevNode = this.irFunction.getInstructionNode(instrNode.prev!)!;
      prevNode.next = instrNode.next;
      replacingPosition = InsertPosition.after(instrNode.prev!);
    }
    if (instrId === region.lastInstruction) {
      region.lastInstruction = instrNode.prev;
    } else {
      const nextNode = this.irFunction.getInstructionNode(instrNode.next!)!;
      nextNode.prev = instrNode.prev;
    }
    instrNode.instruction = null!
    delete this.irFunction.instructions[instrId];
    return replacingPosition;
  }

  replaceInstruction(instrId: InstructionId, instruction: IRInstruction) {
    const replacingPosition = this.deleteInstruction(instrId);
    return this.insertInstructionAtPosition(replacingPosition, instruction);
  }

  getIfRegion(region: RegionId): IfRegion { return this.irFunction.regions[region] as IfRegion; }
  getWhileRegion(region: RegionId): WhileRegion { return this.irFunction.regions[region] as WhileRegion; }
  getBlockRegion(region: RegionId): BlockRegion { return this.irFunction.regions[region] as BlockRegion; }
  getScopeRegion(region: RegionId): ScopeRegion { return this.irFunction.regions[region] as ScopeRegion; }

  insertNewBlockRegion(): RegionId {
    const region = new BlockRegion()
    this.irFunction.regions.push(region);
    const regionId = this.irFunction.regions.length - 1;
    return regionId as RegionId;
  }

  insertNewIfRegion(): RegionId {
    const region = new IfRegion()
    this.irFunction.regions.push(region);
    const regionId = this.irFunction.regions.length - 1 as RegionId
    region.conditionSequence = this.insertNewSequenceRegion(regionId);
    region.thenSequence = this.insertNewSequenceRegion(regionId);
    region.elseSequence = this.insertNewSequenceRegion(regionId);
    region.exitSequence = this.insertNewSequenceRegion(regionId);
    return regionId
  }

  insertNewWhileRegion(): RegionId {
    const region = new WhileRegion()
    this.irFunction.regions.push(region);
    const regionId = this.irFunction.regions.length - 1 as RegionId
    region.conditionSequence = this.insertNewSequenceRegion(regionId);
    region.bodySequence = this.insertNewSequenceRegion(regionId);
    region.exitSequence = this.insertNewSequenceRegion(regionId);
    return regionId
  }

  insertNewScopeRegion(): RegionId {
    const region = new ScopeRegion()
    this.irFunction.regions.push(region);
    const regionId = this.irFunction.regions.length - 1 as RegionId
    region.bodySequence = this.insertNewSequenceRegion(regionId);
    region.exitSequence = this.insertNewSequenceRegion(regionId);
    return regionId
  }

  splitBlockBeforeInstr(instrId: InstructionId) {
    const prevRegionId = this.irFunction.getInstructionRegion(instrId)
    const prevRegion = this.irFunction.regions[prevRegionId]
    compilerAssert(prevRegion instanceof BlockRegion, "Region is not a block", { prevRegionId, prevRegion })
    const sequenceId = prevRegion.parentSequence
    compilerAssert(sequenceId !== null && sequenceId !== undefined, "Sequence not found", { prevRegionId, prevRegion, instrId })
    // const instrIndex = instrId.instrId
    // compilerAssert(instrIndex >= 0, "Instruction not found", { instrId, block })

    const newRegionId = this.insertNewBlockRegion()
    const newRegion = this.getBlockRegion(newRegionId)
    this.insertSequenceChildAfter(sequenceId, prevRegionId, newRegionId)

    const startNode = this.irFunction.getInstructionNode(instrId)!
    const prevId = startNode.prev
    const prevNode = this.irFunction.getInstructionNode(prevId!)
    compilerAssert(prevNode, "prevNode not found", { prevId, prevNode, instrId, prevRegionId, newRegionId })
    prevNode.next = null
    startNode.prev = null
    newRegion.parentSequence = sequenceId
    newRegion.firstInstruction = instrId
    newRegion.lastInstruction = newRegion.lastInstruction
    prevRegion.lastInstruction = prevId
    
    for (let visitId: InstructionId | null = instrId; visitId !== null; visitId = this.irFunction.getInstructionNode(visitId)!.next) {
      const node = this.irFunction.getInstructionNode(visitId)!
      node.region = newRegionId
    }

    return [prevRegionId, newRegionId] as [RegionId, RegionId]
  }

  ////////////////////////////////////////////////////////////////////////////////////////////////
  // These are higher level APIs for generating IR so maybe it should be in a different class
  //

  createDestructorInstructions(source: string, type: Type) {
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

  createParamDeallocStackInstructions(argIndex: number, type: Type) {
    const target = this.irFunction.parameterRegisters[argIndex];

    if (this.compiledFunction.isDestructor) {
      const fields = type.typeInfo.fields;
      const instrs = fields.flatMap((field, i) => {
        const fieldReg = this.newRegister();
        const getFieldPtr = new GetFieldPointerInstruction(fieldReg, target, field)
        const dealloc = this.createDestructorInstructions(fieldReg, field.fieldType)
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

  createMoveInstructions(instr: MoveInstruction, capability: Capability) {
    const sourceAccessReg = this.newRegister();
    const targetAccessReg = this.newRegister();
    compilerAssert(capability === Capability.Set || capability === Capability.Inout, 'Invalid capability');
    const metaobject = instr.type.typeInfo.metaobject;
    const moveFnBinding = capability === Capability.Set ? metaobject.moveInitBinding : metaobject.moveAssignBinding;
    compilerAssert(moveFnBinding && moveFnBinding instanceof Binding, `Move function not found for ${instr.type.shortName}`);
    const moveFn = this.globalState.functions.get(moveFnBinding);
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
    return instrs;
  }


}


export type Usage = {
  instrId: InstructionId,
  operandIndex: number,
}

export type UsageMap = Map<string, Usage[]>;
export const createRegionUsageMap = (irFunction: IrFunction) => {
  const usages = new Map<string, Usage[]>();
  irFunction.regions.forEach((region, regionId) => {
    if (region instanceof BlockRegion) {
      for (let instrId = region.firstInstruction; instrId !== null; instrId = irFunction.getInstructionNode(instrId)!.next) {
        const instr = irFunction.getInstructionNode(instrId)!.instruction;
        const operands = getInstructionOperands(instr);
        for (let i = 0; i < operands.length; i++) {
          const operand = operands[i];
          if (typeof operand === 'string') {
            const usagesList = usages.get(operand) ?? [];
            usages.set(operand, usagesList);
            usagesList.push({ instrId, operandIndex: i });
          }
        }
      }
    }
  });
  return usages;
}