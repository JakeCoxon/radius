import { inspect } from "bun";
import { CommentInstruction, formatInstruction, getInstructionIdentifier, getInstructionOperands, getInstructionResult, IRInstruction } from "../borrow/defs";
import { compilerAssert, textColors } from "../src/defs";

export type Regions = Region[];

export class InstructionNode {
  constructor(public instruction: IRInstruction, public prev: InstructionId | null, public next: InstructionId | null, public region: RegionId) {}
}

export class IrFunction {
  regions: Region[] = []
  sequences: RegionSequence[] = []
  instructions: { [key: string]: InstructionNode } = {}
  root: SequenceId

  constructor(public debugName: string) {}

  getInstruction(instrId: InstructionId): IRInstruction | null { return this.instructions[instrId]?.instruction ?? null; }
  getInstructionNode(instrId: InstructionId): InstructionNode | null { return this.instructions[instrId] ?? null; }
  getInstructionRegion(instrId: InstructionId): RegionId | null { return this.instructions[instrId]?.region ?? null; }
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
  constructor() {}
}

export type Region = BlockRegion | IfRegion | WhileRegion;

export const printIrFunction = (function_: IrFunction, opts?: { instructionNotes: {[key: InstructionId]: string} }) => {

  const regionsSet = new Set<number>(function_.regions.map((_, i) => i));
  const sequencesSet = new Set<number>(function_.sequences.map((_, i) => i));
  
  // console.dir({ function_ }, { depth: 4 });
  const visitRegion = (region: Region, label: string, depth = 0, isLast = true, prefix = "", regionId: number) => {
    const branch = isLast ? "└─ " : "├─ ";
    const connection = depth > 0 ? prefix + branch : "";
    const typeLabel = region ? textColors.blue(`[${region.constructor.name.replace("Region", "")} ${regionId}]`) : textColors.gray("undefined");
    console.log(`${connection}${typeLabel}`, region?.result ?? "");
    // const {instructions, ...regionWithoutInstructions} = region;
    // console.dir({ regionId, regionWithoutInstructions }, { depth: 4 });
    compilerAssert(regionsSet.has(regionId), "Region already visited", { regionId, regionsSet });
    regionsSet.delete(regionId);
    
    let nextPrefix = prefix + (isLast ? "   " : "│  ");
    
    if (region instanceof BlockRegion) {
      for (let instrId = region.firstInstruction; instrId !== null; instrId = function_.getInstructionNode(instrId)?.next ?? null) {
        const instruction = function_.getInstruction(instrId)
        compilerAssert(instruction, "Instruction not found", { instrId, region, function_ });
        const isLastInstruction = instrId === region.lastInstruction;
        // let label = instruction.result ? `${instruction.result} = ${instruction.label}` : instruction.label;
        let label = formatInstruction(instruction);
        label = instruction instanceof CommentInstruction ? textColors.gray(`// ${label}`) : label;
        label += textColors.gray(` (${instrId})`);
        let indent = nextPrefix + (isLastInstruction ? "   " : "   ")
        indent += (" ".repeat(Math.max(0, 15 - indent.length)));
        console.log(indent + label);
        if (opts?.instructionNotes && opts.instructionNotes[instrId]) {
          opts.instructionNotes[instrId].split("\n").forEach(line => {
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
    // let nextPrefix = prefix + (isLast ? "   " : "│  ");
    
    let region = sequence.firstChildRegion;
    let i = 0;
    while (region !== null) {
      const isLast = region === sequence.lastChildRegion;
      
      visitRegion(function_.regions[region], label, depth + 1, isLast, nextPrefix, region);
      region = function_.regions[region].nextRegion;
      i++;
    }
  }

  // console.dir({ sequences: function_.sequences.map((s, i) => ({ i, s })), regions: function_.regions.map((r, i) => ({ i, r })) }, { depth: 4 });

  // visitRegion(function_.regions[function_.root], "Root");
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
  irFunction: IrFunction

  regionSequence: SequenceId | null = null
  blockRegion: RegionId | null = null
  allocBlock: RegionId | null = null
  currentRegion: RegionId | null = null

  regionState: { region: RegionId | null, sequence: SequenceId }[] = []
  freshId = 0

  constructor(irFunction: IrFunction) {
    this.irFunction = irFunction;
  }

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

  insertInstruction(instr: IRInstruction) {
    compilerAssert(this.blockRegion !== null, 'No blockRegion. Call ensureBlock()', { blockRegion: this.blockRegion });
    this.insertBlockInstruction(this.blockRegion, instr);
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

  insertNewBlockRegion(): RegionId {
    const region = new BlockRegion()
    this.irFunction.regions.push(region);
    const regionId = this.irFunction.regions.length - 1;
    return regionId as RegionId;
  }

  createInstructionId(parent: RegionId, instruction: IRInstruction): InstructionId {
    const name = getInstructionIdentifier(instruction) ?? `instr${this.freshId++}`;
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
      const lastNode = this.irFunction.getInstructionNode(region.lastInstruction);
      lastNode.next = instrId;
      region.lastInstruction = instrId;
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

  getIfRegion(region: RegionId): IfRegion { return this.irFunction.regions[region] as IfRegion; }
  getWhileRegion(region: RegionId): WhileRegion { return this.irFunction.regions[region] as WhileRegion; }
  getBlockRegion(region: RegionId): BlockRegion { return this.irFunction.regions[region] as BlockRegion; }
  
  insertNewWhileRegion(): RegionId {
    const region = new WhileRegion()
    this.irFunction.regions.push(region);
    const regionId = this.irFunction.regions.length - 1 as RegionId
    region.conditionSequence = this.insertNewSequenceRegion(regionId);
    region.bodySequence = this.insertNewSequenceRegion(regionId);
    region.exitSequence = this.insertNewSequenceRegion(regionId);
    return regionId
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
      for (let instrId = region.firstInstruction; instrId !== null; instrId = irFunction.getInstructionNode(instrId).next) {
        const instr = irFunction.getInstructionNode(instrId).instruction;
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