import { inspect } from "bun";
import { CommentInstruction, formatInstruction, getInstructionResult, IRInstruction } from "../borrow/defs";
import { compilerAssert, textColors } from "../src/defs";

export type Regions = Region[];

export class InstructionNode {
  constructor(public instruction: IRInstruction, public prev: InstructionId | null, public next: InstructionId | null, public region: RegionId) {}
}

export class IrFunction {
  regions: Regions = []
  sequences: RegionSequence[] = []
  instructions: { [key: string]: InstructionNode } = {}
  root: SequenceId

  getInstruction(instrId: InstructionId) { return this.instructions[instrId]; }
}

export type RegionId = number & { __regionId: true };
export type SequenceId = number & { __sequenceId: true };
export type InstructionId = string & { __instructionId: true };

export class RegionSequence {
  firstChildRegion: RegionId | null = null
  lastChildRegion: RegionId | null = null
  parentRegion: RegionId | null = null
}

// export class SequenceRegion {
//   regions: number[] = []
//   parentRegion: number
//   result: string
//   constructor() {
//   }
// }

export class BlockRegion {
  // instructions: InstructionId[] = []
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
  result: string
  constructor() {}
}

export type Region = BlockRegion | IfRegion | WhileRegion;

// export class IrInstruction {
//   constructor(public result: string, public label: string, public operands: string[]) {}
// }


// const traverseSequenceRegion = (region: SequenceRegion, depth = 0) => {
//   region.regions.forEach((innerRegion, index) => {
//     const inden = " ".repeat(depth * 2)
//     console.log(inden, "Region", index)
//     visitRegion(innerRegion, depth);
//   });
// }
// const visitRegion = (region: Region, depth = 0) => {
//   const inden = " ".repeat(depth * 2)
//   if (region instanceof BlockRegion) {
//     region.instructions.forEach(instruction => {
//       console.log(inden, instruction.label);
//     });
//   } else if (region instanceof SequenceRegion) {
//     console.log(inden, "Sequence", region.result)
//     traverseSequenceRegion(region, depth + 1);
//   } else if (region instanceof IfRegion) {
//     // console.log(inden, "If", region.condition)
//     visitRegion(region.condition, depth + 1);
//     // console.log(inden, "Then", region.thenRegion)
//     if (region.thenRegion !== null) visitRegion(region.thenRegion, depth + 1);
//     // console.log(inden, "Else", region.elseRegion)
//     if (region.elseRegion !== null) visitRegion(region.elseRegion, depth + 1);
//   }
// }

export const printFunction = (function_: IrFunction) => {

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
      for (let instrId = region.firstInstruction; instrId !== null; instrId = function_.getInstruction(instrId).next) {
        const instructionNode = function_.getInstruction(instrId)
        compilerAssert(instructionNode, "Instruction not found", { instrId, region, function_ });
        const isLastInstruction = instrId === region.lastInstruction;
        // let label = instruction.result ? `${instruction.result} = ${instruction.label}` : instruction.label;
        let label = formatInstruction(instructionNode.instruction);
        label = instructionNode.instruction instanceof CommentInstruction ? textColors.gray(`// ${label}`) : label;
        let indent = nextPrefix + (isLastInstruction ? "   " : "   ")
        indent += (" ".repeat(Math.max(0, 15 - indent.length)));
        console.log(indent + label);
      }
      // region.instructions.forEach((instrId, index) => {
      //   const instructionNode = function_.instructions[instrId.instructionId];
      //   const isLastInstruction = index === region.instructions.length - 1;
      //   // let label = instruction.result ? `${instruction.result} = ${instruction.label}` : instruction.label;
      //   let label = formatInstruction(instructionNode.instruction);
      //   label = instructionNode.instruction instanceof CommentInstruction ? textColors.gray(`// ${label}`) : label;
      //   let indent = nextPrefix + (isLastInstruction ? "   " : "   ")
      //   indent += (" ".repeat(Math.max(0, 15 - indent.length)));
      //   console.log(indent + label);
      // });
    // } else if (region instanceof SequenceRegion) {
    //   region.regions.forEach((index, i) => {
    //     const innerRegion = function_.regions[index];
    //     visitRegion(innerRegion, `Region ${i}`, depth + 1, i === region.regions.length - 1, nextPrefix);
    //   });
    } else if (region instanceof IfRegion) {
      visitSequence(region.conditionSequence, "Cond", depth + 1, false, nextPrefix);
      if (region.thenSequence !== null) {
        const isLastBranch = region.elseSequence === null;
        visitSequence(region.thenSequence, "Then", depth + 1, isLastBranch, nextPrefix);
      }
      if (region.elseSequence !== null) {
        visitSequence(region.elseSequence, "Else", depth + 1, true, nextPrefix);
      }
    } else if (region instanceof WhileRegion) {
      visitSequence(region.conditionSequence, "Cond", depth + 1, false, nextPrefix);
      visitSequence(region.bodySequence, "Body", depth + 1, true, nextPrefix);
    }
  };
  const visitSequence = (sequenceId: number, label: string, depth = 0, isLast = true, prefix = "") => {
    const sequence = function_.sequences[sequenceId]
    const branch = isLast ? "└─ " : "├─ ";
    const connection = depth > 0 ? prefix + branch : "";
    console.log(`${connection}${textColors.green(label)}`);
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

  console.dir({ sequences: function_.sequences.map((s, i) => ({ i, s })), regions: function_.regions.map((r, i) => ({ i, r })) }, { depth: 4 });

  // visitRegion(function_.regions[function_.root], "Root");
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

  insertChildSequenceAndPushState(region: RegionId) {
    compilerAssert(this.regionSequence !== null, 'No parentSequence', { parentSequence: this.regionSequence });
    this.insertSequenceChild(this.regionSequence, region)
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
    const name = getInstructionResult(instruction) ?? `instr${this.freshId++}`;
    return name as InstructionId;
  }

  getInstructionById(instrId: InstructionId): InstructionNode {
    const instrNode = this.irFunction.getInstruction(instrId)
    compilerAssert(instrNode, "Instruction not found", { instrId, instructions: this.irFunction.instructions });
    return instrNode;
  }

  insertBlockInstruction(parent: RegionId, instruction: IRInstruction) {
    const instrId = this.createInstructionId(parent, instruction);
    const region = this.irFunction.regions[parent] as BlockRegion;
    const newNode = new InstructionNode(instruction, region.lastInstruction, null, parent)
    this.irFunction.instructions[instrId] = newNode;
    if (region.lastInstruction === null) {
      region.firstInstruction = instrId;
      region.lastInstruction = instrId;
    } else {
      const lastNode = this.getInstructionById(region.lastInstruction);
      lastNode.next = instrId;
      region.lastInstruction = instrId;
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
    return regionId
  }

  getIfRegion(region: RegionId): IfRegion { return this.irFunction.regions[region] as IfRegion; }
  getWhileRegion(region: RegionId): WhileRegion { return this.irFunction.regions[region] as WhileRegion; }
  getBlockRegion(region: RegionId): BlockRegion { return this.irFunction.regions[region] as BlockRegion; }
  
  // getIfCondSequence(region: RegionId): SequenceId {
  //   return (this.irFunction.regions[region] as IfRegion).conditionSequence;
  // }
  // getIfThenSequence(region: RegionId): SequenceId {
  //   return (this.irFunction.regions[region] as IfRegion).thenSequence;
  // }
  // getIfElseSequence(region: RegionId): SequenceId {
  //   return (this.irFunction.regions[region] as IfRegion).elseSequence;
  // }
  // getWhileCondSequence(region: RegionId): SequenceId {
  //   return (this.irFunction.regions[region] as WhileRegion).conditionSequence;
  // }
  // getWhileBodySequence(region: RegionId): SequenceId {
  //   return (this.irFunction.regions[region] as WhileRegion).bodySequence;
  // }

  // insertIfCond(parent: number, condRegion: number) {
  //   const region = this.irFunction.regions[parent] as IfRegion;
  //   this.insertSequenceChild(region.conditionSequence, condRegion);
  //   // region.conditionSequence = condRegion;
  //   // const then = this.irFunction.regions[condRegion];
  //   // then.parentRegion = parent;
  // }

  // insertIfThen(parent: number, thenRegion: number) {
  //   const region = this.irFunction.regions[parent] as IfRegion;
  //   region.thenRegion = thenRegion;
  //   const then = this.irFunction.regions[thenRegion];
  //   then.parentRegion = parent;
  // }

  // insertIfElse(parent: number, elseRegion: number) {
  //   const region = this.irFunction.regions[parent] as IfRegion;
  //   region.elseRegion = elseRegion;
  //   const else_ = this.irFunction.regions[elseRegion];
  //   else_.parentRegion = parent;
  // }

  insertNewWhileRegion(): RegionId {
    const region = new WhileRegion()
    this.irFunction.regions.push(region);
    const regionId = this.irFunction.regions.length - 1 as RegionId
    region.conditionSequence = this.insertNewSequenceRegion(regionId);
    region.bodySequence = this.insertNewSequenceRegion(regionId);
    return regionId
  }

  // insertWhileCond(parent: number, condRegion: number) {
  //   const region = this.irFunction.regions[parent] as WhileRegion;
  //   region.condition = condRegion;
  //   const cond = this.irFunction.regions[condRegion];
  //   cond.parentRegion = parent;
  // }

  // insertWhileBody(parent: number, bodyRegion: number) {
  //   const region = this.irFunction.regions[parent] as WhileRegion;
  //   region.bodyRegion = bodyRegion;
  //   const body = this.irFunction.regions[bodyRegion];
  //   body.parentRegion = parent;
  // }
}